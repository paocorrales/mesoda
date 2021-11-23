librayr(tidyverse)
library(ggplot2)

df <- map_df( Sys.glob("~/datosmunin3/DATA/OBS/datain/ADPSFC_n3/201811*"), ~fread(.x, na.strings = c("1e+11",
                                                                                                     "1.000000e+11",
                                                                                                     "1.0000e+11"))) %>%
  setnames(paste0("V", c(1:10)), c("lat", "lon", "alt", "stationID", "date", "t", "q", "u", "v", "p"))


n_stations <-
  df[lat %between% c(-41.5, -20) & lon %between% c(-75, -54), .N, by = stationID] %>%
  .[str_detect(stationID, "SMN", negate = TRUE)]

df[lat %between% c(-41.5, -20) & lon %between% c(-75, -54)] %>%
  .[str_detect(stationID, "SMN", negate = TRUE)] %>%
  melt(measure.vars = c("t", "q", "u", "v", "p")) %>%
  .[, is_na := fifelse(is.na(value), "NA", "OBS")] %>%
  .[, .N, by = .(stationID, variable, is_na)] %>%
  dcast(stationID + variable ~ is_na) %>%
  .[!is.na(OBS)] %>%
  dcast(stationID ~ variable, value.var = "OBS")

df[lat %between% c(-41.5, -20) & lon %between% c(-75, -54)] %>%
  .[str_detect(stationID, "SMN", negate = TRUE)] %>%
  melt(measure.vars = c("t", "q", "u", "v", "p")) %>%
  .[, is_na := fifelse(is.na(value), "NA", "OBS")] %>%
  .[, .N, by = .(stationID, variable, is_na)] %>%
  dcast(stationID + variable ~ is_na) %>%
  .[!is.na(OBS), .N, by = variable]

SMN <- %>%
  df[lat %between% c(-41.5, -20) & lon %between% c(-75, -54)] %>%
  # unique(by = c("stationID")) %>%
  .[str_detect(stationID, "SMN")] %>%
  unique(by = "date")

df[lat %between% c(-41.5, -20) & lon %between% c(-75, -54)] %>%
  unique(by = c("lat", "lon")) %>%
  ggplot(aes(lon, lat)) +
  geom_point() +
  geom_point(data = square, size = 0.1) +
  geom_mapa()
