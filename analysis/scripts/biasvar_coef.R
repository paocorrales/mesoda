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
Bdiag <- rep(.1, npred)


  coef_prior <- coef_gfs[sensor == paste(this_sensor, plat, sep = "_") & channel == this_channel, 7:18]
  coef_prior <- matrix(as.vector(t(coef_prior)))
  coef_prior <- rep(0, npred)

biasvar <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)

for (i in seq_along(biasvar)) {

  Bdiag <- rep(biasvar[i], npred)
  # Bdiag <- rep(0.1, npred)

  coef_est <- estimate_coef(Xobs, Xmod, pred, coef_prior, Rdiag, Bdiag)

  bias_est <- get_bias_correction(pred, coef_est + coef_prior)

  if (i == 1) {
    dt <- data.table("xobs" = c(Xobs), "xmod" = c(Xmod), "bias" = bias_est, biasv = biasvar[i])
    coef <- data.table("coef" = as.vector(coef_est), biasv = biasvar[i])
  } else {
    dt <- rbind(dt, data.table("xobs" = c(Xobs), "xmod" = c(Xmod), "bias" = bias_est, biasv = biasvar[i]))
    coef <- rbind(coef, data.table("coef" = as.vector(coef_est), biasv = biasvar[i]))
  }


}

dt <- dt[, ":="(OmB = xobs - xmod,
                OmB_BC = xobs - xmod - bias)] %>%
  melt(measure.vars = c("OmB", "OmB_BC"))

dt[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
       bias = mean(value, na.rm = TRUE)), by = .(biasv, variable)] %>%
  ggplot(aes(biasv, bias)) +
  geom_point(aes(color = variable)) +
  scale_x_log10()

dt[, .(rmse = sqrt(mean(value^2, na.rm = TRUE)),
       bias = mean(value, na.rm = TRUE)), by = .(biasv, variable)] %>%
  ggplot(aes(biasv, rmse)) +
  geom_point(aes(color = variable)) +
  scale_x_log10()

dt[, .(bias_mean = mean(bias.V1)), by = biasv]

coef %>%
  ggplot(aes(coef, biasv)) +
  geom_boxplot(aes(color = factor(biasv))) +
  scale_y_log10()
