library(metR)
library(tidyverse)
library(data.table)
library(lubridate)
library(unglue)
library(mesoda)


# Constante
#future::plan("cluster", workers = 3)
imerg_path <- "/home/paola.corrales/mesoda/analysis/data/derived_data/"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E5"
run <- "ana_ens"

ini_date <- ymd_hms("20181120180000")
ciclos <- 67

acumulado <- 6
q <- c(1, 5, 10, 25) #10mm, para arrancar pensando en pp acumulada
w <- c(1, 5, 11, 51, 101) #ancho de cada box en puntos de grilla

# Inicio
dates <- seq.POSIXt(ini_date + hours(acumulado), by = "hour",
                    length.out = ciclos - acumulado)


pp_imerg_all <- readRDS(paste0(imerg_path, "IMERG_", acumulado, "h.rds"))


fss_out <- purrr::map_dfr(dates, function(d) {

  print(d)
  date <- d

  # Observaciones

  pp_imerg <- pp_imerg_all[end_date == date]

  # Modelo

  if (run == "ana") {
    files_wrf <-  purrr::map(seq(0, acumulado - 1), function(l) {
      list.files(path = paste0(wrf_path, exp, "/", toupper(run), "/", format(date - hours(l), "%Y%m%d%H%M%S")),
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = "analysis.ensmean")
    }) %>% unlist()


    pp_wrf <- purrr::map(files_wrf, function(f) {
      ReadNetCDF(f, vars = c("RAINNC", "RAINC", "RAINSH",
                             lon = "XLONG", lat = "XLAT")) %>%
        .[, ":="(pp_acum = RAINNC + RAINC + RAINSH,
                 exp = exp,
                 date = date)] %>%
        .[, ":="(RAINNC = NULL,
                 RAINC = NULL,
                 RAINSH = NULL,
                 Time = NULL)]
    }) %>%
      rbindlist() %>%
      .[, .(pp_acum = sum(pp_acum, na.rm = TRUE)), by = .(lon, lat, date, exp)] %>%
      .[, c("x", "y") := wrf_project(lon, lat)]

  } else if (run == "ana_ens") {

    files_wrf <- paste0(wrf_path, "derived_data/ppacum/", exp, "_ana_", format(date, "%Y%m%d%H"), "_prop_acum_",
                        acumulado, "h.rds")

    pp_wrf <- readRDS(files_wrf)

  } else if (run == "fcst_ens") {

    files_wrf <- paste0(wrf_path, "derived_data/ppacum/", exp, "_fcst_", format(ini_date, "%Y%m%d%H"), "_prop_acum_",
                        acumulado, "h.rds")

    pp_wrf <- readRDS(files_wrf) %>%
      .[, date := lead_time] %>%
      .[, exp := exp] %>%
      .[date == d]

  } else {

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
        .[, .(pp_acum = mean(PP, na.rm = TRUE),
              exp = exp,
              date = date,
              t = basename(f)), by = .(lat, lon)]

    }) %>%
      rbindlist() %>%
      .[, .(pp_acum = diff(pp_acum)), by = .(lon, lat, date, exp)] %>%
      .[, c("x", "y") := wrf_project(lon, lat)]

  }

  # browser()

  pp_imerg <- pp_imerg[x %between% c(-900000, 900000) & y %between% c(-1100000, 1100000)]
  pp_wrf <- pp_wrf[x %between% c(-900000, 900000) & y %between% c(-1100000, 1100000)]

  obs <- pp_imerg %>%
    .[, .(pp = list(dcast(.SD, x ~ y, value.var = "pp_acum") %>%
                      .[, -1] %>%
                      as.matrix()))]

  fss <- purrr::map(q, function(qi) {

    if (run == "ana_ens") {
      fcst <- pp_wrf[umbral == qi] %>%
        .[, .(pp = list(dcast(.SD, x ~ y, value.var = "prop") %>%
                          .[, -1] %>%
                          as.matrix())),
          by = .(exp, date)]

      fss <- fcst[, calculate_fss(pp[[1]], obs[, pp[[1]]],
                                  q = qi, w = w, binary = FALSE),
                  by = .(exp, date)]
    } else if (run == "fcst_ens") {

      fcst <- pp_wrf[umbral == qi] %>%
        .[, .(pp = list(dcast(.SD, x ~ y, value.var = "prop") %>%
                          .[, -1] %>%
                          as.matrix())),
          by = .(exp)]

      fss <- fcst[, calculate_fss(pp[[1]], obs[, pp[[1]]],
                                  q = qi, w = w, binary = FALSE),
                  by = .(exp)] %>%
        .[, date := d]
    } else {
      fcst <- pp_wrf %>%
        .[, .(pp = list(dcast(.SD, x ~ y, value.var = "pp_acum") %>%
                          .[, -1] %>%
                          as.matrix())),
          by = .(exp, date)]
      fss <- fcst[, calculate_fss(pp[[1]], obs[, pp[[1]]],
                                  q = qi, w = w),
                  by = .(exp, date)]
    }

  }) %>%
    rbindlist()
})

if (run %in% c("ana", "ana_ens")) {
  fwrite(fss_out, file = paste0("fss_nob_", acumulado, "h_", run, "_", exp, ".csv"))

} else {

  fwrite(fss_out, file = paste0("fss_", acumulado, "h_", run, "_",
                                format(ini_date, "%Y%m%d%H"), "_", exp, "_ens.csv"))

}
