library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
library(patchwork)
source(here::here("temp_R/postprocesamiento.R"))

map_arg <- rnaturalearth::ne_states(country = c("argentina"),
                                    returnclass = "sf")
map_limitrofes <- rnaturalearth::ne_countries(country = c("Brazil", "Chile", "Uruguay", "Paraguay", "Bolivia"), returnclass = "sf")

geom_mapa <- function(fill = NA) {
  list(geom_sf(data = map_arg, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       geom_sf(data = map_limitrofes, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       coord_sf(ylim = c(-42, -19), xlim = c(-76, -51)),
       scale_x_longitude(ticks = 5),
       scale_y_latitude(ticks = 5))
}

wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
             length.out = ciclos)



for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  files_gue <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


    ReadNetCDF(f, vars = c(lon = "XLONG", lat = "XLAT", u = "P")) %>%
      .[, c("u", "v") := uvmet(f)] %>%
      .[, ":="(exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[bottom_top == 7]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  guess <- purrr::map(files_gue, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c(lon = "XLONG", lat = "XLAT", u = "P")) %>%
      .[, c("u", "v") := uvmet(f)] %>%
      .[, ":="(exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[bottom_top == 7]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    setnames(c("u", "v"), c("u_guess", "v_guess"))

  ana %>%
    # .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = v, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-20, 20, 3)) +
    geom_contour2(aes(z = v),
                  proj = norargentina_lambert,
                  breaks = seq(-20, 20, 3), color = "white", size = 0.1) +
    scale_fill_divergent(super = ScaleDiscretised,
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("ANA | ", dates[d]), fill = "v (s = 7)") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    guess %>%
    ana[, on = .NATURAL] %>%
    .[, diff := v - v_guess] %>%
    # .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = diff, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-7, 7, 1)) +
    geom_contour2(aes(z = diff),
                  color = "white", size = 0.1,
                  proj = norargentina_lambert,
                  breaks = seq(-7, 7, 1)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "v (s = 7)") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/Vs7_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 4, width = 8)

}
