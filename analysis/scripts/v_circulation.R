library(mesoda)
library(metR)
library(tidyverse)
library(lubridate)
library(data.table)
library(here)
library(patchwork)
library(unglue)
library(knitr)
library(kableExtra)
library(tagger)

levs <- c(200, 300, 500, 600)

files <- Sys.glob("/home/paola.corrales/datosmunin3/EXP/E[2,5,6,8]/ANA/20181122180000/analysis.ensmean")

coord <- ReadNetCDF(files[1], vars = c(lon = "XLONG", lat = "XLAT"), subset = list(bottom_top = 1))

ana <- purrr::map(files, function(f) {
  descriptores <- unglue::unglue(f, c("/home/paola.corrales/datosmunin3/EXP/{exp}/ANA/{date}/analysis.ensmean"))

  # Leo pronóstico con algo de post procesamiento
  fcst <- ReadNetCDF(f, vars = c(p = "P", "PB", t = "T", qv = "QVAPOR",
                                        lon = "XLONG", lat = "XLAT")) %>%
    .[, p := p + PB] %>%
    .[, t := (t + 300)*(p/100000)^(2/7)] %>%
    # .[, td := td(qv, p) + 273.15] %>%
    .[, ":="(Time = NULL,
             PB = NULL)] %>%
    .[, c("u", "v") := uvmet(f)] %>%
    .[, ":="(date = ymd_hms(descriptores[[1]][["date"]]),
             exp = descriptores[[1]][["exp"]])] %>%
    .[, .(lev = levs,
          t = approx(p, t, xout = levs*100)$y,
          qv = approx(t, qv, xout = levs*100)$y,
          u = approx(p, u, xout = levs*100)$y,
          v = approx(p, v, xout = levs*100)$y),
      by = .(date, south_north, west_east, exp)] %>%
    coord[., on = .NATURAL]

}) %>%
  rbindlist()

files <- Sys.glob("/home/paola.corrales/datosmunin3/EXP/derived_data/dbz/maxdbz_ana_E[2,5,6,8]_20181122180000.nc")

dbz <- purrr::map(files,  function(f) {

  exp <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/derived_data/dbz/maxdbz_ana_{exp}_20181122180000.nc")
  ReadNetCDF(f, vars = "max_dbz") %>%
    .[, exp := exp[[1]][["exp"]]]

  }) %>% rbindlist()

ana %>%
  dbz[., on = .NATURAL] %>%
  .[, c("x", "y") := wrf_project(lon, lat)] %>%
  .[lev == 200] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = v, fill = stat(level_d)), proj = norargentina_lambert) +
  scale_fill_divergent_discretised() +
  geom_contour2(aes(z = max_dbz, color = ..level..),
                proj = norargentina_lambert, breaks = seq(10, 40, 10)) +
  geom_mapa() +
  labs(caption = "200 hPA", fill = "v", color = "dbz") +
  facet_wrap(~exp) +
  theme_minimal()


ana %>%
  dbz[., on = .NATURAL] %>%
  .[, c("x", "y") := wrf_project(lon, lat)] %>%
  .[lev == 500] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = v, fill = stat(level_d)), proj = norargentina_lambert) +
  scale_fill_divergent_discretised() +
  geom_contour2(aes(z = max_dbz, color = ..level..),
                proj = norargentina_lambert, breaks = seq(10, 40, 10)) +
  geom_mapa() +
  labs(caption = "500 hPA", fill = "v", color = "dbz") +
  facet_wrap(~exp) +
  theme_minimal()

ana %>%
  dbz[., on = .NATURAL] %>%
  .[, c("x", "y") := wrf_project(lon, lat)] %>%
  .[lev == 600] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = v, fill = stat(level_d)), proj = norargentina_lambert) +
  scale_fill_divergent_discretised() +
  geom_contour2(aes(z = max_dbz, color = ..level..),
                proj = norargentina_lambert, breaks = seq(10, 40, 10)) +
  geom_mapa() +
  labs(caption = "600 hPA", fill = "v", color = "dbz") +
  facet_wrap(~exp) +
  theme_minimal()
