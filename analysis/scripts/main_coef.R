library(reticulate)
library(tidyverse)
library(data.table)
library(ggplot2)

source_python("analysis/scripts/common_diag.py")
source_python("analysis/scripts/common_tools.py")

coef_gfs <- mesoda::read_satbias("~/datosmunin2/nomads.ncdc.noaa.gov/GDAS/201811/20181111/gdas.t18z.abias") %>%
  setDT()

sensor_list <- as.character(unique(coef_gfs$sensor))

files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/prueba_BC/ANA/*/diagfiles/diag_*.ensmean")

# diag <- read_diag_sat("/home/paola.corrales/datosmunin/EXP/prueba_BC/20181112010000/diagfiles/diag_hirs4_metop-a_ges.ensmean")

out_coef <- purrr::map(sensor_list, function(f) {
  print(f)
  files_sensor <- files[str_detect(files, f)]

  if (length(files_sensor) == 0) {
    print("no files")
    return(NULL)
  }

   print("files!")
    diag <- purrr::map(files_sensor, read_diag_sat)


    #Sensor
    nchanl <- diag[[1]]$nuchan

    out_sensor <- purrr::map(seq_along(nchanl), function(n) {
      channel_index <- n
print(n)
      this_sensor <- str_trim(diag[[1]]$sensor)
      plat <- str_trim(diag[[1]]$sat)

      #Fijos
      npred <- diag[[1]]$npred
      Bdiag <- rep(.0001, npred)
      # coef_prior <- rep(0, npred)
      coef_prior <- coef_gfs[sensor == paste(this_sensor, plat, sep = "_") & channel == nchanl[channel_index], 7:18]
      coef_prior <- as.vector(t(coef_prior))

      out_channel <- purrr::map(diag, function(x) {
        QCmask <- matrix(x$qc[, channel_index] == 0 & x$tb_obs[, channel_index] < 400)
        #matrix(x$qc[, channel_index] == 0)

        Rdiag <- 1/x$errinv[, channel_index][QCmask]
        Xobs <- matrix(x$tb_obs[, channel_index][QCmask])
        Xmod <- Xobs - matrix(x$tbcnob[, channel_index][QCmask])
        pred <- x$predictors[, as.vector(QCmask), channel_index]
        date <- rep(x$date, length(as.vector(Xobs)))

        list(Rdiag = Rdiag, Xobs = Xobs, Xmod = Xmod, pred = pred, date = date)
      } ) %>%
        reduce(function(x, y) list(Rdiag = c(x$Rdiag, y$Rdiag),
                                   Xobs = rbind(x$Xobs, y$Xobs),
                                   Xmod = rbind(x$Xmod, y$Xmod),
                                   pred = cbind(x$pred, y$pred),
                                   date = c(x$date, y$date)))

      if (length(out_channel$Xobs) > 1) {
        coef_est <- estimate_coef(out_channel$Xobs, out_channel$Xmod,
                                  out_channel$pred, coef_prior, out_channel$Rdiag, Bdiag) + coef_prior

        bias_est <- get_bias_correction(out_channel$pred, coef_est)

      } else {
        coef_est <- coef_prior

        bias_est <- rep(0, length(out_channel$Xobs))
      }

      coef_out <- data.table(id = n,
                             sensor = paste(this_sensor, plat, sep = "_"),
                             channel = nchanl[channel_index],
                             tlp1 = 0,
                             tlp2 = 0,
                             nc = 999,
                             coeff = t(coef_est)) %>%
        setnames(old = paste0("coeff.V", 1:12), new = paste0("coeff", 1:12))


      est <- data.table(sensor = paste(this_sensor, plat, sep = "_"),
                        channel = nchanl[channel_index],
                        xobs = c(out_channel$Xobs), #obs
                        xmod = c(out_channel$Xmod), #guess
                        date = c(out_channel$date),
                        bias_est) %>%
        .[, ":="(OmB_BC = xobs - bias_est - xmod,
                 OmB = xobs - xmod)]


      list(coef_out = coef_out, est = est)
    }) %>%
      reduce(function(x, y) list(coef_out = rbind(x$coef_out, y$coef_out),
                                 est = rbind(x$est, y$est)))

}) %>%
  reduce(function(x, y) list(coef_out = rbind(x$coef_out, y$coef_out),
                             est = rbind(x$est, y$est)))

write_rds(out_coef, "/home/paola.corrales/datosmunin/EXP/satbias_trained_zero-init.rds")


rbind(out_sensor$coef_out[, run := "off_line"], coef_gfs[sensor == "iasi_metop-a"][, run := "gfs"]) %>%
  iasi_asim[., on = .NATURAL] %>%
  .[!is.na(iuse)] %>%
  melt(measure.vars = paste0("coeff", 1:12)) %>%
  dcast(sensor + channel + variable ~ run) %>%
  ggplot(aes(gfs, off_line)) +
    geom_point(aes(color = factor(variable))) +
  facet_wrap(~variable, scales = "free")

  out_sensor$est %>%
    melt(measure.vars = c("OmB_BC", "OmB")) %>%
    ggplot(aes(value)) +
    geom_density(aes(color = variable))

  out_sensor$est %>%
    melt(measure.vars = c("OmB_BC", "OmB")) %>%
    .[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
          bias = mean(value, na.rm = TRUE)), by = .(variable, sensor)] %>%
    ggplot(aes(bias, sensor)) +
    geom_point(aes(color = variable))
    coord_cartesian(xlim = c(-10, 10))

  out_sensor$est %>%
    melt(measure.vars = c("OmB_BC", "OmB")) %>%
    .[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
          bias = mean(value, na.rm = TRUE)), by = .(variable, channel)] %>%
    ggplot(aes(channel, bias)) +
    geom_hline(yintercept = 0) +
    geom_point(aes(color = variable))
    geom_point(aes(y = OmB_BC), color = "red")


    out_sensor$est %>%
      .[channel == 12] %>%
      ggplot(aes(OmB, OmB_BC)) +
      geom_point(aes(color = factor(date)), size = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      facet_wrap(~date)

      melt(measure.vars = c("OmB_BC", "OmB")) %>%
      ggplot(aes(value)) +
      geom_density(aes(color = variable))
