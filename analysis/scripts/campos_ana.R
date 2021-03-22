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


wrf_path <- "/home/paola.corrales/datosmunin/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
                    length.out = ciclos)

# Td2m

for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E[4-7]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  files_gue <- Sys.glob(paste0(wrf_path, "E[4-7]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


    ReadNetCDF(f, vars = c("QVAPOR", "P", "PB",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = c(1:3))) %>%
      .[, ":="(td = td(QVAPOR, P+PB),
               exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  guess <- purrr::map(files_gue, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c("QVAPOR", "P", "PB",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = c(1:3))) %>%
      .[, ":="(td = td(QVAPOR, P+PB),
               exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    setnames("td2", "td2_guess")

  ana %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = td2, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(0, 30, 2)) +
    scale_fill_viridis_c(super = ScaleDiscretised,
                         limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("ANA | ", dates[d]), fill = "~Td2m") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    guess %>%
    ana[, on = .NATURAL] %>%
    .[, diff := td2 - td2_guess] %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = diff, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-4, 4, 0.5)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/T2m_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}

# Temp
for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E[4-7]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  files_gue <- Sys.glob(paste0(wrf_path, "E[4-7]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


    ReadNetCDF(f, vars = c(t = "T",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = c(1:3))) %>%
      .[, ":="(t = t + 290 - 273.15,
               exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, .(t2 = mean(t)), by = .(lon, lat, exp, date)]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  guess <- purrr::map(files_gue, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c(t = "T",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = c(1:3))) %>%
      .[, ":="(t = t + 290 - 273.15,
               exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, .(t2 = mean(t)), by = .(lon, lat, exp, date)]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    setnames("t2", "t2_guess")

  ana %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = t2, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(10, 34, 2)) +
    scale_fill_viridis_c(super = ScaleDiscretised,
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("ANA | ", dates[d]), fill = "~T2m") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    guess %>%
    ana[, on = .NATURAL] %>%
    .[, diff := t2 - t2_guess] %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = diff, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-4, 4, 0.5)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~T2m") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/T2m_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}


# Obs

# Td2m

for (d in seq_along(dates)) {

  print(dates[d])


  files_diag <- Sys.glob(paste0(wrf_path, "E[4-7]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/diagfiles/asim_conv_", format(dates[d], "%Y%m%d%H%M%S"), ".ensmean"))

  diag <- purrr::map(files_diag, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/diagfiles/asim_conv_{fecha2}.ensmean")


    read_diag_conv(f, exp = metadatos[[1]][["exp"]], member = "000") %>%
      .[, lon := ConvertLongitude(lon)]

  }) %>%
    rbindlist()

  diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "v"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs), alpha = 0.7) +
    scale_color_viridis_c(breaks = seq(-30, 30, 5), limits = c(-30, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("OBS | ", dates[d]), color = "v") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

  diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
      usage.flag == 1 & rerr != 1.0e+10 & var == "v"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs.guess), alpha = 0.7) +
    scale_color_divergent(breaks = seq(-30, 30, 5), limits = c(-30, 30),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("OBS - GUESS | ", dates[d]), color = "v") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/v_OmB_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}



