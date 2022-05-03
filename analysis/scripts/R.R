library(tidyverse)
library(data.table)
library(metR)
library(mesoda)


Rcpp::cppFunction('double outer(NumericVector x, NumericVector y) {
  double n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      total += x[i]*y[j];
    }
  }
  total /= n*n;
  return total;
}')

# outer <- cov

path_data <- "/home/paola.corrales/datosmunin3/EXP/"

files_gue <- Sys.glob(paste0(path_data, "E10/ANA/*/diagfiles/asim_abi_g16_*.ensmean"))[1:50]
files_ana <- Sys.glob(paste0(path_data, "E10/ANA/*/diagfiles_ana/asim_abi_g16_*.ensmean"))[1:50]

d <- map2(files_gue, files_ana, function(fg, fa) {

  meta <- unglue::unglue(fg, "/home/paola.corrales/datosmunin3/EXP/E10/ANA/{date}/diagfiles/asim_abi_g16_{date2}.ensmean")
  gue <- read_diag_rad(fg, "E10") %>%
    .[channel %in% c(8:10) & qc == 0, .(channel, lat, lon, tbc)] %>%
    setnames("tbc", "gue")

  ana <- read_diag_rad(fa, "E10") %>%
    .[channel %in% c(8:10) & qc == 0, .(channel, lat, lon, tbc)] %>%
    setnames("tbc", "ana")

  ana[gue, on = c("channel", "lat", "lon") ] %>%
    na.omit(.) %>%
    melt(measure.vars = c("ana", "gue")) %>%
    dcast(lat + lon ~ channel + variable) %>%
    na.omit(.) %>%
    # .[, .(cor_88 = mean(`8_ana` %*% t(`8_gue`)),
    #       cor_89 = mean(`8_ana` %*% t(`9_gue`)),
    #       cor_810 = mean(`8_ana` %*% t(`10_gue`)),
    #       cor_98 = mean(`9_ana` %*% t(`8_gue`)),
    #       cor_99 = mean(`9_ana` %*% t(`9_gue`)),
    #       cor_910 = mean(`9_ana` %*% t(`10_gue`)),
    #       cor_108 = mean(`10_ana` %*% t(`8_gue`)),
    #       cor_109 = mean(`10_ana` %*% t(`9_gue`)),
    #       cor_1010 = mean(`10_ana` %*% t(`10_gue`)))] %>%
    .[, fecha := meta[[1]][["date"]]] %>%
    .[]

}) %>%
  rbindlist()

names <- colnames(d)[3:8]

d %>%
  melt(measure.vars = names) %>%
  .[, mean(value), by = .(variable, fecha)] %>%
  .[, fecha := lubridate::ymd_hms(fecha)] %>%
  ggplot(aes(fecha, V1)) +
  geom_line(aes(color = variable))

outer <- function(x, y) {

# return(sum(x * y)/length(x))
  return(cor(x, y))

}

d %>%
  # .[40000:60000] %>%
  .[, .(cor_88 = outer(`8_ana`, `8_gue`),
        cor_89 = outer(`8_ana`, `9_gue`),
        cor_810 = outer(`8_ana`, `10_gue`),
        cor_98 = outer(`9_ana`, `8_gue`),
        cor_99 = outer(`9_ana`, `9_gue`),
        cor_910 = outer(`9_ana`, `10_gue`),
        cor_108 = outer(`10_ana`, `8_gue`),
        cor_109 = outer(`10_ana`, `9_gue`),
        cor_1010 = outer(`10_ana`,`10_gue`))] %>% #, by = .(fecha)] %>%
  # melt(measure.vars = c("cor_88", "cor_89", "cor_810",
  #                       "cor_98", "cor_99", "cor_1010",
  #                       "cor_108", "cor_109", "cor_1010")) %>%
  # .[, mean(value), by = .(variable, fecha)] %>%
  # .[, fecha := lubridate::ymd_hms(fecha)] %>%
  # ggplot(aes(fecha, V1)) +
  # geom_line(aes(color = variable))
  # .[, .(cor_88 = mean(`8_ana` %*% t(`8_gue`)),
  #       cor_89 = mean(`8_ana` %*% t(`9_gue`)),
  #       cor_810 = mean(`8_ana` %*% t(`10_gue`)),
  #       cor_98 = mean(`9_ana` %*% t(`8_gue`)),
  #       cor_99 = mean(`9_ana` %*% t(`9_gue`)),
  #       cor_910 = mean(`9_ana` %*% t(`10_gue`)),
  #       cor_108 = mean(`10_ana` %*% t(`8_gue`)),
  #       cor_109 = mean(`10_ana` %*% t(`9_gue`)),
  #       cor_1010 = mean(`10_ana` %*% t(`10_gue`)))] %>%
# .[, .(cor_1010 = outer(`10_ana`,`10_gue`))]
  matrix(nrow = 3, byrow = TRUE)



ana[gue, on = c("channel", "lat", "lon") ] %>%
  na.omit(.) %>%

  .[, .(sigma2 = mean(ana * gue)), by = .(channel)]

ch10 <- ana[gue, on = c("channel", "lat", "lon") ] %>%
  na.omit(.) %>%
  .[channel == 8]

mean(ch10$ana %*% t(ch10$gue))
