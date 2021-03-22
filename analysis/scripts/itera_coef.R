library(reticulate)
library(data.table)
library(ggplot2)

source_python("analysis/scripts/common_diag.py")
source_python("analysis/scripts/common_tools.py")

coef_gfs <- mesoda::read_satbias("~/datosmunin2/nomads.ncdc.noaa.gov/GDAS/201811/20181120/gdas.t18z.abias") %>%
  setDT()

diag <- read_diag_sat("/home/paola.corrales/datosmunin/EXP/prueba_BC/20181112010000/diagfiles/diag_hirs4_metop-a_ges.ensmean")

# diag <- read_diag_sat("/home/paola.corrales/diag_atms_n20_ges.ensmean")

this_channel <-  8
this_sensor <- str_trim(diag$sensor)
plat <- str_trim(diag$sat)

QCmask <- matrix(diag$qc[, this_channel] == 0)
Rdiag <- 1/diag$errinv[, this_channel][QCmask]

Xobs <- matrix(diag$tb_obs[, this_channel][QCmask])
Xmod <- Xobs - matrix(diag$tbcnob[, this_channel][QCmask])


npred <- diag$npred
pred <- diag$predictors[, as.vector(QCmask), this_channel]
Bdiag <- rep(.0001, npred)


for (i in seq(1:100)) {

  if (i == 1) {
    coef_prior <- coef_gfs[sensor == paste(this_sensor, plat, sep = "_") & channel == this_channel, 7:18]
    coef_prior <- matrix(as.vector(t(coef_prior)))
    # coef_prior <- rep(0, npred)
  } else {
    coef_prior <- coef_est
  }

  coef_est <- estimate_coef(Xobs, Xmod, pred, coef_prior, Rdiag, Bdiag)

  bias_est <- get_bias_correction(pred, coef_est)

  if (i == 1) {
    dt <- data.table("xobs" = c(Xobs), "xmod" = c(Xmod), "bias" = bias_est, numiter = i)
    coef <- data.table("coef" = as.vector(coef_est), numiter = i)
  } else {
    dt <- rbind(dt, data.table("xobs" = c(Xobs), "xmod" = c(Xmod), "bias" = bias_est, numiter = i))
    coef <- rbind(coef, data.table("coef" = as.vector(coef_est), numiter = i))
  }


}

dt <- dt[, ":="(OmB = xobs - xmod,
           OmB_BC = xobs - xmod - bias)] %>%
  melt(measure.vars = c("OmB", "OmB_BC"))

dt[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
       bias = mean(value, na.rm = TRUE)), by = .(numiter, variable)] %>%
  # .[variable == "OmB_BC"] %>%
  ggplot(aes(numiter, bias)) +
  geom_point(aes(color = variable))

dt[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
       bias = mean(value, na.rm = TRUE)), by = .(numiter, variable)] %>%
  ggplot(aes(numiter, rmse)) +
  geom_point(aes(color = variable))

dt[, .(bias_mean = mean(bias.V1)), by = numiter]

dt %>%
  ggplot(aes(value)) +
  geom_density(aes(color = factor(numiter))) +
  facet_wrap(~variable)
