# Calcula y guarda acumulados del pronóstico para todo el periodo

library(metR)
library(tidyverse)
library(data.table)
library(lubridate)
library(unglue)
library(mesoda)


imerg_path <- "/home/paola.corrales/mesoda/analysis/data/derived_data/"
wrf_path <- "/home/paola.corrales/datosmunin3/EXP/"
exp <- "E2"
run <- "fcst"

ini_date <- ymd_hms("20181122000000")
ciclos <- 37
acumulado <- 1

dates <- seq.POSIXt(ini_date + hours(acumulado), by = "hour",
                    length.out = ciclos - acumulado)


q <- c(1, 5, 10, 25, 50)

# Inicio

pp_wrf <- purrr::map(dates, function(d) {

  print(d)
  date <- d

  lead_time <- as.numeric(difftime(date, ini_date, units = "hours"))

  files_wrf <-  purrr::map(c(acumulado, 0), function(l) {
    paste0(wrf_path, exp, "/", toupper(run), "/",
           format(ini_date, "%Y%m%d%H"), "/NPP/NPP_",
           format(ini_date, "%Y"), "-",
           format(ini_date, "%m"), "-",
           format(ini_date, "%d"), "_",
           format(ini_date, "%H"), "_FC",
           formatC(lead_time - l, width = 2, flag = "0"),
           ".nc")
  }) %>% unlist()


  pp_wrf <- purrr::map(files_wrf, function(f) {

    if (!file.exists(f)) {
      return(NULL)
    }

    ReadNetCDF(f, vars = c("PP", "XLAT", "XLONG")) %>%
      .[, ":="(lon = XLONG, lat = XLAT)] %>%
      .[, ":="(XLONG = NULL, XLAT = NULL)] %>%
      .[, ":="(exp = exp,
               date = date,
               t = basename(f))]

  }) %>%
    rbindlist() %>%
    .[, .(pp_acum = diff(PP)), by = .(lon, lat, date, exp, ens)] %>%
    .[, c("x", "y") := wrf_project(lon, lat)]


  pp_wrf_mean <- pp_wrf[, .(pp_acum = mean(pp_acum)),
                        by = .(lat, lon, x, y, exp)] %>%
    .[, date := d]


  pp_prop <- purrr::map(q, function(f){

    pp_wrf %>%
      .[, .(prop = mean(pp_acum > f)), by = .(lon, lat, x, y, exp)] %>%
      .[, umbral := f] %>%
      .[, date := d]

  }) %>%
    rbindlist()

  list(pp_wrf_mean = pp_wrf_mean, pp_prop = pp_prop)

}) %>%
  reduce(function(x, y) list(pp_wrf_mean = rbind(x$pp_wrf_mean, y$pp_wrf_mean),
                             pp_prop = rbind(x$pp_prop, y$pp_prop)))

saveRDS(pp_wrf$pp_prop, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_prop_acum_", acumulado, "h.rds"))
saveRDS(pp_wrf$pp_wrf_mean, paste0(exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_mean_acum_", acumulado, "h.rds"))
