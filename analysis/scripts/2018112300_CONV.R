library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
source(here::here("temp_R/postprocesamiento.R"))

geom_mapa <- function(fill = NA) {
  list(geom_sf(data = map_arg, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       geom_sf(data = map_limitrofes, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       coord_sf(ylim = c(-34, -29), xlim = c(-66, -63)),
       scale_x_longitude(ticks = ),
       scale_y_latitude(ticks = 1))
}


diag <- read_diag_conv("/home/paola.corrales/datosmunin/EXP/E4/ANA/20181123000000/diagfiles/asim_conv_20181123000000.ensmean", exp = "CONV") %>%
  .[, ":="(lon = ConvertLongitude(lon),
           obs = fcase(var == "t", obs - 273.15,
                       var == "q", obs*1000,
                       var %in% c("v", "u"), obs))]

diag[usage.flag == 1 & lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5), .N, by = .(var)]

diag[usage.flag == 1 & lat %between% c(-32, -30) & lon %between% c(-66, -64)] %>%
  .[var %in% c("t","q")] %>%
  ggplot(aes(obs.guess, pressure)) +
  geom_point(aes(color = factor(type))) +
  scale_y_level() +
  facet_wrap(~var, scales = "free_x")


wrf_path <- "/home/paola.corrales/datosmunin/EXP/"

ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
             length.out = ciclos)

files_ana <- Sys.glob(paste0(wrf_path, "E[4-7]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

files_gue <- Sys.glob(paste0(wrf_path, "E[4-7]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

ana <- purrr::map(files_ana, function(f) {

  metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


  ReadNetCDF(f, vars = c("QVAPOR", "P", "PB",
                         lon = "XLONG", lat = "XLAT"),
             subset = list(bottom_top = c(1:15))) %>%
    .[, ":="(td = td(QVAPOR, P+PB),
             exp = metadatos[[1]][["exp"]],
             date = metadatos[[1]][["fecha"]])]
  # %>%
  #   .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
}) %>%
  rbindlist() %>%
  .[, c("x", "y") := wrf_project(lon, lat)]

guess <- purrr::map(files_gue, function(f) {

  metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

  ReadNetCDF(f, vars = c(QVAPOR_g = "QVAPOR", "P", "PB",
                         lon = "XLONG", lat = "XLAT"),
             subset = list(bottom_top = c(1:15))) %>%
    .[, ":="(td_g = td(QVAPOR_g, P+PB),
             exp = metadatos[[1]][["exp"]],
             date = metadatos[[1]][["fecha"]])]
  # %>%
  #   .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
}) %>%
  rbindlist() %>%
  .[, c("x", "y") := wrf_project(lon, lat)]
# %>%
#   setnames("td2", "td2_guess")



guess %>%
  .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = QVAPOR_g*1000, fill = stat(level_d)),
                    proj = norargentina_lambert,
                    breaks = seq(-6, 26, 2)) +
  scale_fill_viridis_c(super = ScaleDiscretised,
                       limits = c(-6, 26),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  geom_mapa() +
  facet_wrap(~exp) +
  labs(caption = paste0("ANA | ", dates[d]), fill = "~Td2m") +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))

  cbind(ana[, c("QVAPOR")], guess) %>%
  # .[, QVAPOR_g := QVAPOR] %>%
  # .[, QVAPOR := NULL] %>%
  .[, diff := QVAPOR - QVAPOR_g] %>%
  # .[, ":="(x = NULL, y = NULL, lat = NULL, lon = NULL)] %>%
  .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = diff*1000, fill = stat(level_d)),
                    proj = norargentina_lambert) +
  scale_fill_divergent(super = ScaleDiscretised,
                       #                      # limits = c(1, 30),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  geom_mapa() +
  labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
  facet_grid(exp ~ bottom_top) +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))

  patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  cbind(ana[, c("td")], guess) %>%
    # .[, QVAPOR_g := QVAPOR] %>%
    # .[, QVAPOR := NULL] %>%
    .[, diff := td - td_g] %>%
    # .[, ":="(x = NULL, y = NULL, lat = NULL, lon = NULL)] %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = td_g, fill = stat(level_d)),
                      proj = norargentina_lambert) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
    facet_grid(exp ~ bottom_top) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2))



