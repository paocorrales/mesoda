# Calcula y guarda acumulados del pronóstico para todo el periodo

library(metR)
library(tidyverse)
library(data.table)
library(lubridate)
library(unglue)
library(mesoda)


# Constante
imerg_path <- "/home/paula.maldonado/datosalertar1/RRA_VERIF/data/raw/imerg_raw"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E4"
run <- "fcst"

ini_date <- ymd_hms("20181122060000")
ciclos <- 37

acumulado <- 6
q <- c(5, 10, 25, 50)
# q <- c(1, 5, 10, 25)

# Inicio

first <- ymd_hms("20181122060000")
last <- ymd_hms("20181123120000")

lead_time <- c(final = 30, inicial = 0)
pp_wrf <- purrr::map(seq_along(lead_time), function(f) {


  file_wrf <- paste0(wrf_path, exp, "/", toupper(run), "/",
                     format(ini_date, "%Y%m%d%H"), "/NPP/NPP_",
                     format(ini_date, "%Y"), "-",
                     format(ini_date, "%m"), "-",
                     format(ini_date, "%d"), "_",
                     format(ini_date, "%H"), "_FC",
                     formatC(lead_time[f], width = 2, flag = "0"),
                     ".nc")


  if (!file.exists(file_wrf)) {
    return(NULL)
  }

  ReadNetCDF(file_wrf, vars = c("PP", "XLAT", "XLONG")) %>%
    .[, ":="(lon = XLONG, lat = XLAT)] %>%
    .[, ":="(XLONG = NULL, XLAT = NULL)] %>%
    .[, ":="(exp = exp,
             lead_time = names(lead_time)[f])]

}) %>%
  rbindlist() %>%
  dcast(ens + lat + lon ~ lead_time, value.var = "PP") %>%
  .[, pp_acum := final - inicial] %>%
  .[, ":="(final = NULL,
           inicial = NULL)] %>%
  .[, c("x", "y") := wrf_project(lon, lat)]


pp_wrf_mean <- pp_wrf[, .(pp_acum = mean(pp_acum)),
                      by = .(lat, lon, x, y)]


pp_prop <- purrr::map(q, function(f){

    pp_wrf %>%
    .[, .(prop = mean(pp_acum > f)), by = .(lon, lat, x, y)] %>%
    .[, umbral := f]

}) %>%
  rbindlist()


saveRDS(pp_prop, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_prop_acum_30h.rds"))
saveRDS(pp_wrf_mean, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_mean_acum_30h.rds"))



# Acumulados horarios -----------------------------------------------------



# Constante
imerg_path <- "/home/paula.maldonado/datosalertar1/RRA_VERIF/data/raw/imerg_raw"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E4"
run <- "fcst"

ini_date <- ymd_hms("20181122060000")


acumulado <- 1
#q <- c(5, 10, 25, 50)
q <- c(1, 5, 10, 25, 50)

# Inicio

lead_time <- seq(0, 30, acumulado)

pp_wrf <- purrr::map(lead_time, function(f) {


  file_wrf <- paste0(wrf_path, exp, "/", toupper(run), "/",
                     format(ini_date, "%Y%m%d%H"), "/NPP/NPP_",
                     format(ini_date, "%Y"), "-",
                     format(ini_date, "%m"), "-",
                     format(ini_date, "%d"), "_",
                     format(ini_date, "%H"), "_FC",
                     formatC(f, width = 2, flag = "0"),
                     ".nc")

  print(file_wrf)

  if (!file.exists(file_wrf)) {
    return(NULL)
  }

  ReadNetCDF(file_wrf, vars = c("PP", "XLAT", "XLONG")) %>%
    .[, ":="(lon = XLONG, lat = XLAT)] %>%
    .[, ":="(XLONG = NULL, XLAT = NULL)] %>%
    .[, ":="(exp = exp,
             lead_time = ini_date + hours(f))]

}) %>%
  rbindlist() %>%
.[, pp_acum := c(0, diff(PP)), by = .(ens, exp, lon, lat)] %>%
  .[, PP := NULL] %>%
  .[, c("x", "y") := wrf_project(lon, lat)]


pp_wrf_mean <- pp_wrf[, .(pp_acum = mean(pp_acum)),
                      by = .(lat, lon, x, y, exp, lead_time)]


pp_prop <- purrr::map(q, function(f){

  pp_wrf %>%
    .[, .(prop = mean(pp_acum > f)), by = .(lon, lat, x, y, lead_time)] %>%
    .[, umbral := f]

}) %>%
  rbindlist()



saveRDS(pp_prop, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_prop_acum_1h.rds"))
saveRDS(pp_wrf_mean, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_mean_acum_1h.rds"))




