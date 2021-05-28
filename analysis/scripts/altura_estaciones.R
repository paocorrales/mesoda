library(mesoda)
library(metR)
library(tidyverse)
library(lubridate)
library(data.table)
library(here)

dominio <- fread(here("analysis", "data", "derived_data", "dominio_hgt.csv")) %>%
  .[, c("x", "y") := wrf_project(lon, lat)]


oficiales <- fread("/home/paola.corrales/E2_asim_conv_20181121120000.ensmean",
                   na.strings = c("0.100E+11", "-0.100E+06", "-99999.90", "-100000.00", "0.100E+12")) %>%
  .[, c("V2", "V4") := NULL] %>%
  setnames(colnames(.), c("var", "stationID", "type", "dhr", "lat", "lon", "alt", "usage.flag", "flag", "obs", "obs.guess", "obs2", "obs.guess2", "rerr")) %>%
  .[type %in% c(181, 281)] %>% unique(by = c("stationID")) %>%
  .[!str_detect(stationID, pattern = "[A-Z]")] %>%
  .[, source := "Sfc - Official"]

no_oficiales <- fread("/home/paola.corrales/E5_asim_conv_20181121120000.ensmean",
                      na.strings = c("0.100E+11", "-0.100E+06", "-99999.90", "-100000.00", "0.100E+12")) %>%
  .[, c("V2", "V4") := NULL] %>%
  setnames(colnames(.), c("var", "stationID", "type", "dhr", "lat", "lon", "alt", "usage.flag", "flag", "obs", "obs.guess", "obs2", "obs.guess2", "rerr")) %>%
  .[type %in% c(181, 187)] %>%
  unique(by = c("stationID")) %>%
  .[!str_detect(stationID, "SMN")] %>%
  .[!(stationID %in% oficiales$stationID)] %>%
  .[, source := "Sfc - Non-official"]

obs <- rbind(no_oficiales, oficiales) %>%
  .[, c("x", "y") := wrf_project(ConvertLongitude(lon), lat)]


alturas <- dominio %>%
  .[, Interpolate(hgt ~ x + y, x.out = obs$x, y.out = obs$y, grid = FALSE)] %>%
  .[obs, on = .NATURAL] %>%
  .[, diff := alt - hgt] %>%
  .[, diff_b := fcase(abs(diff) %between% c(0, 100), "[0, 100]",
                      abs(diff) %between% c(100, 300), "[100, 300]",
                      abs(diff) > 300, "[300, Inf]")]

alturas[abs(diff) < 1000] %>%
ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_point(aes(color = diff)) +
  scale_color_divergent(trans = mesoda::symlog_trans(),
                        guide = guide_colourbar(barheight = 25)) +
  geom_mapa() +
  labs(title = "Estación - Modelo", color = NULL) +
  theme_minimal()

alturas %>%
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_point(aes(color = diff_b)) +
  # scale_color_divergent(trans = mesoda::symlog_trans(),
  #                       guide = guide_colourbar(barheight = 25)) +
  geom_mapa() +
  labs(title = "Estación - Modelo", color = NULL) +
  theme_minimal()

