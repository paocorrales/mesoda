library(reticulate)
library(tidyverse)
library(data.table)
library(ggplot2)

source_python("analysis/scripts/common_diag.py")
source_python("analysis/scripts/common_tools.py")

coef_gfs <- mesoda::read_satbias("~/datosmunin2/nomads.ncdc.noaa.gov/GDAS/201811/20181120/gdas.t18z.abias") %>%
  setDT()

sensor_list <- as.character(unique(coef_gfs$sensor))

files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/prueba_BC/*/diagfiles/diag_*.ensmean")

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

      this_sensor <- str_trim(diag[[1]]$sensor)
      plat <- str_trim(diag[[1]]$sat)

      #Fijos
      npred <- diag[[1]]$npred
      Bdiag <- rep(.0001, npred)
      coef_prior <- rep(0, npred)

      out_channel <- purrr::map(diag, function(x) {
        QCmask <- matrix(x$qc[, channel_index] == 0)

        Rdiag <- 1/x$errinv[, channel_index][QCmask]
        Xobs <- matrix(x$tb_obs[, channel_index][QCmask])
        Xmod <- Xobs - matrix(x$tbcnob[, channel_index][QCmask])
        pred <- x$predictors[, as.vector(QCmask), channel_index]

        list(Rdiag = Rdiag, Xobs = Xobs, Xmod = Xmod, pred = pred)
      } ) %>%
        reduce(function(x, y) list(Rdiag = c(x$Rdiag, y$Rdiag),
                                   Xobs = rbind(x$Xobs, y$Xobs),
                                   Xmod = rbind(x$Xmod, y$Xmod),
                                   pred = cbind(x$pred, y$pred)))

      if (length(out_channel$Xobs) > 1) {
        coef_est <- estimate_coef(out_channel$Xobs, out_channel$Xmod,
                                  out_channel$pred, coef_prior, out_channel$Rdiag, Bdiag) + coef_prior

        bias_est <- get_bias_correction(out_channel$pred, coef_est)

      } else {
        coef_est <- coef_prior

        bias_est <- rep(0, length(out_channel$Xobs))
      }

      coef_out <- data.table(sensor = paste(this_sensor, plat, sep = "_"),
                             channel = nchanl[channel_index],
                             tlp1 = 0,
                             tlp2 = 0,
                             nc = 999,
                             coeff = t(coef_est))


      est <- data.table(sensor = paste(this_sensor, plat, sep = "_"),
                        channel = nchanl[channel_index],
                        xobs = c(out_channel$Xobs),
                        xmod = c(out_channel$Xmod),
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

read_rds(out_coef, "/home/paola.corrales/datosmunin/EXP/satbias_trained.rds")

  out_coef$est %>%
    melt(measure.vars = c("OmB_BC", "OmB")) %>%
    ggplot(aes(value)) +
    geom_density(aes(color = variable))

  out_coef$est %>%
    melt(measure.vars = c("OmB_BC", "OmB")) %>%
    .[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
          bias = mean(value, na.rm = TRUE)), by = .(variable, sensor)]
