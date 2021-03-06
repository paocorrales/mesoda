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
exp <- "E8"
run <- "ana"

ini_date <- ymd_hms("20181120180000")

acumulado <- 6
q <- c(5, 10, 25, 50, 100)

# Inicio


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


acumulado <- 1
q <- c(1, 5, 10, 25, 50)

# Inicio

lead_time <- seq(0, 36, acumulado)

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

# Acumulados cada 6 horas -----------------------------------------------------

imerg_path <- "/home/paola.corrales/mesoda/analysis/data/derived_data/"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E7"
run <- "fcst"

ini_date <- ymd_hms("20181122060000")
ciclos <- 31
acumulado <- 6

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

# Acumulados horarios ANA ------------------------------------------------

imerg_path <- "/home/paola.corrales/mesoda/analysis/data/derived_data/"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E8"
run <- "ana"

ini_date <- ymd_hms("20181120180000")
ciclos <- 67

acumulado <- 6
q <- c(1, 5, 10, 25, 50)


dates <- seq.POSIXt(ini_date + hours(acumulado), by = "hour",
                    length.out = ciclos - acumulado)

pp_wrf_out <- purrr::map_dbl(dates, function(d) {

  date <- d

  print(d)

  files_wrf <-  purrr::map(seq(0, acumulado - 1), function(l) {
    list.files(path = paste0(wrf_path, exp, "/", toupper(run), "/", format(date - hours(l), "%Y%m%d%H%M%S")),
               full.names = TRUE,
               recursive = TRUE,
               pattern = "analysis.mem*")
  }) %>% unlist()


  pp_wrf <- purrr::map(files_wrf, function(f) {

    metadatos <- unglue(basename(f), "analysis.mem0{mem}")

    ReadNetCDF(f, vars = c("RAINNC", "RAINC", "RAINSH",
                           lon = "XLONG", lat = "XLAT")) %>%
      .[, ":="(pp_acum = RAINNC + RAINC + RAINSH,
               exp = exp,
               date = date,
               mem = metadatos[[1]][["mem"]])] %>%
      .[, ":="(RAINNC = NULL,
               RAINC = NULL,
               RAINSH = NULL,
               Time = NULL)]
  }) %>%
    rbindlist() %>%
    .[, .(pp_acum = sum(pp_acum)), by = .(exp, mem, date, lon, lat)] %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  pp_wrf_mean <- pp_wrf[, .(pp_acum = mean(pp_acum)),
                            by = .(lat, lon, x, y, exp, date)]


  pp_prop <- purrr::map(q, function(f){



    pp_wrf %>%
      .[, .(prop = mean(pp_acum > f)), by = .(lon, lat, x, y, exp, date)] %>%
      .[, umbral := f]
  }) %>%
    rbindlist()


  saveRDS(pp_prop, paste0(exp, "_ana_", format(date, "%Y%m%d%H"), "_prop_acum_", acumulado, "h.rds"))
  saveRDS(pp_wrf_mean, paste0(exp, "_ana_", format(date, "%Y%m%d%H"), "_mean_acum_", acumulado, "h.rds"))

d
})


