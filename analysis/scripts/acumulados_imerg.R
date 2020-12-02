# Calcula y guarda acumulados de pp_imerg

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
#exp <- "E4"
#run <- "ana"

ini_date <- ymd_hms("20181120000000")
ciclos <- 85

acumulado <- 6

# Inicio
dates <- seq.POSIXt(ini_date + hours(acumulado), by = "hour",
                    length.out = ciclos - acumulado)

# Modelo
pp_wrf <- purrr::map("/home/paola.corrales/datosmunin/EXP/E7/ANA/20181122060000/analysis.ensmean", function(f) {
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


interpolate <- function(x, y, pp) {

  data <- akima::interp(x, y, pp,
                        xo = unique(pp_wrf$x),
                        yo = unique(pp_wrf$y))
  dimnames(data$z) <- list(x = data$x,
                           y = data$y)
  reshape2::melt(data$z)
}

pp_acum_imerg <- furrr::future_map_dfr(dates, function(d) {

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
    .[, .(pp_acum = sum(pp)), by = .(lon, lat)] %>%
    .[, c("x", "y") := wrf_project(lon, lat)]


  pp_imerg_interp <- pp_imerg[, interpolate(x, y, pp_acum)] %>%
    setnames(c("value"), c("pp_acum")) %>%
    setDT() %>%
    .[, end_date := date]


})

saveRDS(pp_acum_imerg, file = paste0("IMERG_", acumulado, "h.rds"))

