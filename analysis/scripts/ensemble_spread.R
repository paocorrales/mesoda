library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
source(here::here("temp_R/postprocesamiento.R"))

fisica <- data.table(mem = as.character(formatC(1:60, flag = "0", width = 3)),
                     fisica = rep(c("KF-YSU",
                                    "BMJ-YSU",
                                    "GF-YSU",
                                    "KF-MYJ",
                                    "BMJ-MYJ",
                                    "GF-MYJ",
                                    "KF-MYNN2",
                                    "BMJ-MYNN2",
                                    "GF-MYNN2"), length.out = 60)) %>% setDT()
# geom_mapa <- function(fill = NA) {
#   list(geom_sf(data = map_arg, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
#        geom_sf(data = map_limitrofes, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
#        coord_sf(ylim = c(-34, -29), xlim = c(-66, -63)),
#        scale_x_longitude(ticks = 1.5),
#        scale_y_latitude(ticks = 1.5))
# }

coord <- ReadNetCDF("/home/paola.corrales/datosmunin3/EXP/E2/ANA/20181120180000/analysis.ensmean",
                    vars = c(lon = "XLONG", lat = "XLAT")) %>%
  .[, Time := NULL]

wrf_path <- "/home/paola.corrales/datosmunin3/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67

date <- ymd_hms(20181121180000)

  files_ana <- Sys.glob(paste0(wrf_path, "E[2,5,6,9]/ANA/", format(date, "%Y%m%d%H%M%S"), "/analysis.mem*"))

  # files_gue <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.mem*"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/ANA/{fecha}/analysis.mem{mem}")


    ReadNetCDF(f, vars = c("QVAPOR", "P", "PB", "T",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = 1)) %>%
      .[, ":="(td = td(QVAPOR, P+PB),
               t = tk(T, P+PB, T_BASE = 290),
               exp = metadatos[[1]][["exp"]],
               mem = metadatos[[1]][["mem"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, ":="(P = NULL,
               PB = NULL,
               T = NULL,
               QVAPOR = NULL)]
    # .[, .(td = mean(td)), by = .(lon, lat, exp, mem, date)]
  }) %>%
    rbindlist()

  ana %>%
    .[, .(sd= sd(t)), by = .(bottom_top, south_north, west_east, exp)] %>%
    .[bottom_top == 1] %>%
    ggplot(aes(west_east, south_north)) +
    geom_raster(aes(fill = sd)) +
    scale_fill_viridis_c() +
    facet_wrap(vars(exp))


  ana %>%
    .[, .(sd= sd(t)), by = .(bottom_top, south_north, west_east, exp)] %>%
    dcast(bottom_top + south_north + west_east ~ exp) %>%
    .[bottom_top == 1 & south_north %between% c(0, 50) & west_east %between% c(0, 50)] %>%
    .[, ":="(E9_E2 = E9 - E2,
                            E9_E5 = E9 - E5,
                            E9_E6 = E9 - E6)] %>%
    melt(measure.vars = c("E9_E2", "E9_E5", "E9_E6")) %>%
    ggplot(aes(west_east, south_north)) +
    geom_raster(aes(fill = value)) +
    scale_fill_divergent() +
    # scale_fill_viridis_c()
    facet_wrap(vars(variable))
