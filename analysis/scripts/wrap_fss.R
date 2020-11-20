library(metR)
library(tidyverse)
library(data.table)
library(lubridate)
library(unglue)
library(mesoda)


# Constante
future::plan("cluster", workers = 3)
imerg_path <- "/home/paula.maldonado/datosalertar1/RRA_VERIF/data/raw/imerg_raw"
wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
exp <- "E7"
run <- "fcst"

ini_date <- ymd_hms("20181122000000")
ciclos <- 37

acumulado <- 6
q <- c(1, 5, 10) #10mm, para arrancar pensando en pp acumulada
w <- c(2, 5, 25, 50) #ancho de cada box w*2+1

# Inicio
dates <- seq.POSIXt(ini_date, by = "hour", length.out = ciclos)

fss_out <- furrr::future_map_dfr(dates, function(d) {

  print(d)
  date <- d

  # Observaciones
  files_imerg <- purrr::map(seq(acumulado), function(l) {
    list.files(imerg_path, pattern = format(date - hours(l), "%Y%m%d-S%H"), full.names = TRUE)
  }) %>% unlist()

  pp_imerg <- purrr::map(files_imerg, function(f) {
    meta <- unglue(basename(f), "3B-HHR-L.MS.MRG.3IMERG.{dia}-S{hora_ini}-E{hora_fin}.{algo}.V05B.RT-H5")
    ReadNetCDF(f,
               vars = c(pp = "Grid/precipitationCal"),
               subset = list("Grid/lon" = -80:-50,
                             "Grid/lat" = -45:-15)) %>%
      .[, fecha := ymd_hms(paste0(meta[[1]][[1]], meta[[1]][[3]]))]

  }) %>%
    rbindlist() %>%
    setnames(c("Grid/lon", "Grid/lat"), c("lon", "lat")) %>%
    .[, .(pp_acum = sum(pp)), by = .(lon, lat)]


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
      .[, .(pp_acum = sum(pp_acum, na.rm = TRUE)), by = .(lon, lat, date, exp)]

  } else {

    lead_time <- as.numeric(difftime(date, ini_date, units = "hours"))

    files_wrf <-  purrr::map(seq(0, acumulado - 1), function(l) {
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
              date = date), by = .(lat, lon)]

    }) %>%
      rbindlist() %>%
      .[, .(pp_acum = sum(pp_acum, na.rm = TRUE)), by = .(lon, lat, date, exp)]

  }

  interpolate <- function(lon, lat, pp) {
    data <- interp::interp(lon, lat, pp, output = "grid",
                           xo = unique(pp_imerg$lon),
                           yo = unique(pp_imerg$lat))
    dimnames(data$z) <- list(lon = data$x,
                             lat = data$y)
    reshape2::melt(data$z)
  }

  pp_wrf <- pp_wrf[, interpolate(lon, lat, pp_acum),
                   by = .(date, exp)] %>%
    setnames(c("value"), c("pp_acum"))


  fcst <- pp_wrf %>%
    .[, .(pp = list(dcast(.SD, lon ~ lat, value.var = "pp_acum") %>%
                      .[, -1] %>%
                      as.matrix())),
      by = .(exp, date)]

  obs <- pp_imerg %>%
    .[, .(pp = list(dcast(.SD, lon ~ lat, value.var = "pp_acum") %>%
                      .[, -1] %>%
                      as.matrix()))]

  fss <- fcst[, calculate_fss(pp[[1]], obs[, pp[[1]]],
                              q = q, w = w, progress = FALSE),
              by = .(exp, date)]

})

if (run == "ana") {
  fwrite(fss_out, file = paste0("fss_", acumulado, "h_", run, "_", exp, ".csv"))

} else {

  fwrite(fss_out, file = paste0("fss_", acumulado, "h_", run, "_",
                                format(ini_date, "%Y%m%d%H"), "_", exp, ".csv"))

}
