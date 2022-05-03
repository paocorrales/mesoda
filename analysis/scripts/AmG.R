library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
source(here::here("temp_R/postprocesamiento.R"))

map <- rnaturalearth::ne_states(country = c("argentina", "Brazil", "Chile", "Uruguay", "Paraguay", "Bolivia"), returnclass = "sf")

geom_mapa <- function() {
  list(geom_sf(data = map, fill = NA, color = "black", size = 0.2, inherit.aes = FALSE),
       coord_sf(xlim = c(-76, -52), ylim = c(-41, -20)))
}

wrf_path <- "/home/paola.corrales/datosmunin3/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67/3

dates <- seq(ini_date, by = "3 hour",
             length.out = ciclos)

coord <- ReadNetCDF(paste0(wrf_path, "E9/ANA/20181120180000/analysis.ensmean"),
                    vars = c(lon = "XLONG", lat = "XLAT"), subset = list(bottom_top = 1))


levs <- c(300, 500, 700)

for (d in seq_along(dates)) {

  print(dates[d])

files_ana <- Sys.glob(paste0(wrf_path, "E[1,6,9]*/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

ana <- map(files_ana, function(f) {

  meta <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/ANA/{date}/analysis.ensmean")

  ReadNetCDF(f, vars = c(t_ana = "T", p = "P", "PB", "QVAPOR",
                         lon = "XLONG", lat = "XLAT")) %>%
    .[, ":="(t_ana = tk(t_ana, p + PB, T_BASE = 290) - 273.15,
             rh_ana = rh(QVAPOR, p + PB, tk(t_ana, p + PB, T_BASE = 290)),
             p = p + PB)] %>%
    .[, PB := NULL] %>%
    .[, ":="(date = ymd_hms(meta[[1]][["date"]]),
             exp = meta[[1]][["exp"]])] %>%
    .[, .(lev = levs,
          t_ana = approx(p, t_ana, xout = levs*100)$y,
          rh_ana = approx(p, rh_ana, xout = levs*100)$y),
      by = .(date, exp, south_north, west_east)] %>%
    coord[., on = .NATURAL] %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    .[]

}) %>%
  rbindlist()


files_gue <- Sys.glob(paste0(wrf_path, "E[1,6, 9]*/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

gue <- map(files_gue, function(f) {

  meta <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/GUESS/{date}/wrfarw.ensmean")

  ReadNetCDF(f, vars = c(t_gue = "T", p = "P", "PB", "QVAPOR",
                         lon = "XLONG", lat = "XLAT")) %>%
    .[, ":="(t_gue = tk(t_gue, p + PB, T_BASE = 290) - 273.15,
             rh_gue = rh(QVAPOR, p + PB, tk(t_gue, p + PB, T_BASE = 290)),
             p = p + PB)] %>%
    .[, PB := NULL] %>%
    .[, ":="(date = ymd_hms(meta[[1]][["date"]]),
             exp = meta[[1]][["exp"]])] %>%
    .[, .(lev = levs,
          t_gue = approx(p, t_gue, xout = levs*100)$y,
          rh_gue = approx(p, rh_gue, xout = levs*100)$y),
      by = .(date, exp, south_north, west_east)] %>%
    coord[., on = .NATURAL] %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    .[]

}) %>%
  rbindlist()

ana[gue, on = .NATURAL] %>%
  .[, exp := fct_relevel(exp, c("E6", "E9", "E10"))] %>%
  .[, lev := paste(lev, "hPa")] %>%
  .[, date := format(date, "%m/%d %H UTC")] %>%
  .[, a_g := t_ana - t_gue] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = a_g, fill = ..level..), proj = norargentina_lambert,
                    breaks = c(-Inf, seq(-1, 1, 0.2), Inf)) +
  scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 15,
                                                          barwidth = 0.5)) +
  geom_mapa() +
  labs(x = NULL, y = NULL, fill = "AmB",
       title = "Temperature",
       subtitle = as.character(dates[d])) +
  facet_grid(exp~lev, labeller = labeller(exp = c("E6" = "SATWND",
                                                  "E9" = "RAD", "E10" = "ABI"))) +
  theme_minimal(base_size = 8)

 ggsave(paste0("AmB_t_", format(dates[d], "%Y-%m-%d_%H"), ".png"), height = 6.1, width = 6)


ana[gue, on = .NATURAL] %>%
  .[, exp := fct_relevel(exp, c("E6", "E9", "E10"))] %>%
  .[, date := format(date, "%m/%d %H UTC")] %>%
  .[, a_g := rh_ana - rh_gue] %>%
  .[, lev := paste(lev, "hPa")] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = a_g, fill = ..level..), proj = norargentina_lambert,
                    breaks = c(-Inf, seq(-30, 30, 5), Inf)) +
  scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 15,
                                                          barwidth = 0.5)) +
  geom_mapa() +
  labs(x = NULL, y = NULL, fill = "AmB",
       title = "Relative humidity",
       subtitle = as.character(dates[d])) +
  facet_grid(exp~lev, labeller = labeller(exp = c("E6" = "SATWND",
                                                  "E9" = "RAD", "E10" = "ABI"))) +
  theme_minimal(base_size = 8)

  ggsave(paste0("AmB_rh_", format(dates[d], "%Y-%m-%d_%H"), ".png"), height = 6.1, width = 6)

}


for (d in seq_along(dates)) {

  print(dates[d])

  files_ana <- Sys.glob(paste0(wrf_path, "EG[2,4,5]*/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

  ana <- map(files_ana, function(f) {

    meta <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/ANA/{date}/analysis.ensmean")

    ReadNetCDF(f, vars = c(t_ana = "T", p = "P", "PB", "QVAPOR",
                           lon = "XLONG", lat = "XLAT")) %>%
      .[, ":="(t_ana = tk(t_ana, p + PB, T_BASE = 290) - 273.15,
               rh_ana = rh(QVAPOR, p + PB, tk(t_ana, p + PB, T_BASE = 290)),
               p = p + PB)] %>%
      .[, PB := NULL] %>%
      .[, ":="(date = ymd_hms(meta[[1]][["date"]]),
               exp = meta[[1]][["exp"]])] %>%
      .[, .(lev = levs,
            t_ana = approx(p, t_ana, xout = levs*100)$y,
            rh_ana = approx(p, rh_ana, xout = levs*100)$y),
        by = .(date, exp, south_north, west_east)] %>%
      coord[., on = .NATURAL] %>%
      .[, c("x", "y") := wrf_project(lon, lat)] %>%
      .[]

  }) %>%
    rbindlist()


  files_gue <- Sys.glob(paste0(wrf_path, "EG[2,4,5]*/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

  gue <- map(files_gue, function(f) {

    meta <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/GUESS/{date}/wrfarw.ensmean")

    ReadNetCDF(f, vars = c(t_gue = "T", p = "P", "PB", "QVAPOR",
                           lon = "XLONG", lat = "XLAT")) %>%
      .[, ":="(t_gue = tk(t_gue, p + PB, T_BASE = 290) - 273.15,
               rh_gue = rh(QVAPOR, p + PB, tk(t_gue, p + PB, T_BASE = 290)),
               p = p + PB)] %>%
      .[, PB := NULL] %>%
      .[, ":="(date = ymd_hms(meta[[1]][["date"]]),
               exp = meta[[1]][["exp"]])] %>%
      .[, .(lev = levs,
            t_gue = approx(p, t_gue, xout = levs*100)$y,
            rh_gue = approx(p, rh_gue, xout = levs*100)$y),
        by = .(date, exp, south_north, west_east)] %>%
      coord[., on = .NATURAL] %>%
      .[, c("x", "y") := wrf_project(lon, lat)] %>%
      .[]

  }) %>%
    rbindlist()

  ana[gue, on = .NATURAL] %>%
    .[, exp := fct_relevel(exp, c("EG2", "EG4", "EG5"))] %>%
    .[, lev := paste(lev, "hPa")] %>%
    .[, date := format(date, "%m/%d %H UTC")] %>%
    .[, a_g := t_ana - t_gue] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = a_g, fill = ..level..), proj = norargentina_lambert,
                      breaks = c(-Inf, seq(-1, 1, 0.2), Inf)) +
    scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 15,
                                                            barwidth = 0.5)) +
    geom_mapa() +
    labs(x = NULL, y = NULL, fill = "AmB",
         title = "Temperature",
         subtitle = as.character(dates[d])) +
    facet_grid(exp~lev, labeller = labeller(exp = c("EG2" = "ABI_8910",
                                                    "EG4" = "ABI_910", "EG5" = "ABI_10"))) +
    theme_minimal(base_size = 8)

  ggsave(paste0("ABI_AmB_t_", format(dates[d], "%Y-%m-%d_%H"), ".png"), height = 6.1, width = 6)


  ana[gue, on = .NATURAL] %>%
    .[, exp := fct_relevel(exp, c("EG2", "EG4", "EG5"))] %>%
    .[, date := format(date, "%m/%d %H UTC")] %>%
    .[, a_g := rh_ana - rh_gue] %>%
    .[, lev := paste(lev, "hPa")] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = a_g, fill = ..level..), proj = norargentina_lambert,
                      breaks = c(-Inf, seq(-30, 30, 5), Inf)) +
    scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 15,
                                                            barwidth = 0.5)) +
    geom_mapa() +
    labs(x = NULL, y = NULL, fill = "AmB",
         title = "Relative humidity",
         subtitle = as.character(dates[d])) +
    facet_grid(exp~lev, labeller = labeller(exp = c("EG2" = "ABI_8910",
                                                    "EG4" = "ABI_910", "EG5" = "ABI_10"))) +
    theme_minimal(base_size = 8)

  ggsave(paste0("ABI_AmB_rh_", format(dates[d], "%Y-%m-%d_%H"), ".png"), height = 6.1, width = 6)

}
