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
       scale_x_longitude(ticks = 1.5),
       scale_y_latitude(ticks = 1.5))
}

td_colors <- c("#341A02", "#56300B", "#7D4813", "#A46522", "#C6893E", "#DBB773", "#ECD7A4",
               "#FFF9BE", "#FFE390", "#FFBF62", "#FF9348",
               "#DCF2D9", "#BAE4B8", "#8BD290", "#59BC71", "#259F57",
               "#A9CFE4", "#5FA9D2", "#227CB9", "#00488F", "#0000AC")

wrf_path <- "/home/paola.corrales/datosmunin3/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
             length.out = ciclos)

# Td2m

for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E[9]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  files_gue <- Sys.glob(paste0(wrf_path, "E[9]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

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
                      breaks = c(-Inf, seq(-12, 26, 2), Inf)) +
    geom_contour2(aes(z = td2),
                      proj = norargentina_lambert,
                      breaks = seq(-12, 26, 2), color = "white", size = 0.2) +
    scale_fill_manual(drop = FALSE,
                      values = td_colors,
                      guide = guide_colorsteps(barwidth = 0.5,
                                               barheight = 15, show.limits = FALSE)) +
    # geom_point(aes(x = -64.277, y = -32.149), color = "red") +
    geom_mapa() +
    facet_wrap(~exp, labeller = labeller(exp = c(E2 = "CONV", E5 = "AUT",
                                                 E6 = "SATWND", E8 = "RAD"))) +
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
                      breaks = seq(-6, 6, 0.5)) +
    geom_contour2(aes(z = diff),
                  color = "grey70", size = 0.2,
                      proj = norargentina_lambert,
                      breaks = seq(-6, 6, 0.5)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    # geom_point(aes(x = -64.277, y = -32.149), color = "red") +
    geom_mapa() +
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
    facet_wrap(~exp, labeller = labeller(exp = c(E2 = "CONV", E5 = "AUT",
                                                 E6 = "SATWND", E8 = "RAD"))) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/Td2m_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}

# Temp
for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  files_gue <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


    ReadNetCDF(f, vars = c(t = "T", p = "P", "PB",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = 1)) %>%
      .[, ":="(t = tk(t, p + PB, T_BASE = 290) - 273.15,
               exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, .(t2 = mean(t)), by = .(lon, lat, exp, date)]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  guess <- purrr::map(files_gue, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c(t = "T", p = "P", "PB",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(bottom_top = 1)) %>%
      .[, ":="(t = tk(t, p + PB, T_BASE = 290) - 273.15,
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
                      breaks = seq(0, 36, 2)) +
    geom_contour2(aes(z = t2),
                  proj = norargentina_lambert,
                  breaks = seq(0, 36, 2), color = "white", size = 0.2) +
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
                      breaks = seq(-6, 6, 0.5)) +
    geom_contour2(aes(z = diff),
                  color = "grey70", size = 0.2,
                  proj = norargentina_lambert,
                  breaks = seq(-6, 6, 0.5)) +
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

# Viento meridional
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
      .[bottom_top == 1]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  guess <- purrr::map(files_gue, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c(lon = "XLONG", lat = "XLAT", u = "P")) %>%
      .[, c("u", "v") := uvmet(f)] %>%
      .[, ":="(exp = metadatos[[1]][["exp"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[bottom_top == 1]
  }) %>%
    rbindlist() %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    setnames(c("u", "v"), c("u_guess", "v_guess"))

  ana %>%
    # .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = v, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-20, 10, 3)) +
    geom_contour2(aes(z = v),
                  proj = norargentina_lambert,
                  breaks = seq(-20, 10, 3), color = "white", size = 0.1) +
    scale_fill_divergent(super = ScaleDiscretised,
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("ANA | ", dates[d]), fill = "v (s = 1)") +
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
    labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "v (s = 1)") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/Vs1_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 4, width = 8)

}


# Obs

# Q

for (d in seq_along(dates)) {

  print(dates[d])


  files_diag <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/diagfiles/asim_conv_", format(dates[d], "%Y%m%d%H%M%S"), ".ensmean"))

  diag <- purrr::map(files_diag, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/diagfiles/asim_conv_{fecha2}.ensmean")


    read_diag_conv(f, exp = metadatos[[1]][["exp"]], member = "000") %>%
      .[, lon := ConvertLongitude(lon)]

  }) %>%
    rbindlist()

  diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "q"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs*1000), alpha = 0.7) +
    scale_color_viridis_c(breaks = seq(0, 13, 1), limits = c(0, 13),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                   barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("OBS | ", dates[d]), color = "Q") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "q"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs.guess*1000), alpha = 0.7) +
    scale_color_divergent(breaks = seq(-5, 5, 1), limits = c(-6, 6),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                   barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("OBS - GUESS | ", dates[d]), color = "Q") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/q_OmB_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}

# Td

for (d in seq_along(dates)) {

  print(dates[d])


  files_diag <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/diagfiles/asim_conv_", format(dates[d], "%Y%m%d%H%M%S"), ".ensmean"))

  diag <- purrr::map(files_diag, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/diagfiles/asim_conv_{fecha2}.ensmean")


    read_diag_conv(f, exp = metadatos[[1]][["exp"]], member = "000") %>%
      .[, lon := ConvertLongitude(lon)]

  }) %>%
    rbindlist()

  diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "q"] %>%
    # .[, color := cut(td(obs, pressure*100), breaks = seq(-12, 26, 2))]
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = cut(td(obs, pressure*100), breaks = seq(-12, 26, 2)))) +
    scale_color_manual(drop = FALSE,
                      values = td_colors,
                      guide = guide_colorsteps(barwidth = 0.5,
                                              barheight = 15)) +
    # scale_color_viridis_c(breaks = seq(0, 26, 2), limits = c(0, 26),
    #                       guide = guide_colorsteps(barwidth = 0.5,
    #                                                barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp, labeller = labeller(exp = c(E2 = "CONV", E5 = "AUT",
                                                 E6 = "SATWND", E8 = "RAD"))) +
    labs(caption = paste0("OBS | ", dates[d]), color = "Td") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "q"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = td(obs, pressure*100) - td(obs - obs.guess, pressure*100)), alpha = 0.7) +
    scale_color_divergent(breaks = seq(-6, 6, 1), limits = c(-6, 6),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                   barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("OBS - GUESS | ", dates[d]), color = "Td") +
    facet_wrap(~exp, labeller = labeller(exp = c(E2 = "CONV", E5 = "AUT",
                                                 E6 = "SATWND", E8 = "RAD"))) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/td_OmB_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}

# T

for (d in seq_along(dates)) {

  print(dates[d])


  files_diag <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/diagfiles/asim_conv_", format(dates[d], "%Y%m%d%H%M%S"), ".ensmean"))

  diag <- purrr::map(files_diag, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/diagfiles/asim_conv_{fecha2}.ensmean")


    read_diag_conv(f, exp = metadatos[[1]][["exp"]], member = "000") %>%
      .[, lon := ConvertLongitude(lon)]

  }) %>%
    rbindlist()

  diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr < 1.0e+9 & var == "t"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs - 273.15), alpha = 0.7) +
    scale_color_viridis_c(breaks = seq(0, 36, 2), limits = c(0, 36),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                   barheight = 15)) +
    geom_mapa() +
    facet_wrap(~exp) +
    labs(caption = paste0("OBS | ", dates[d]), color = "T2m") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    diag %>%
    .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5) &
        usage.flag == 1 & rerr != 1.0e+10 & var == "t"] %>%
    ggplot(aes(lon, lat)) +
    geom_point(aes(color = obs.guess ), alpha = 0.7) +
    scale_color_divergent(breaks = seq(-6, 6, 1), limits = c(-6, 6),
                          guide = guide_colorsteps(barwidth = 0.5,
                                                   barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("OBS - GUESS | ", dates[d]), color = "T") +
    facet_wrap(~exp) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2)) +

    patchwork::plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(paste0("/home/paola.corrales/campos/t_OmB_", format(dates[d], "%Y%m%d%H%M%S"), ".png"), height = 6, width = 9)

}

