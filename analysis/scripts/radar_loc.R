library(tidyverse)
library(data.table)
library(mesoda)


files <- Sys.glob("/home/jruiz/share/DATA/DATOS_RADAR/MOSAICO/*.nc")[1]

rad_loc <- ReadNetCDF(files, vars = c(lat = "radar_latitude", lon = "radar_longitude")) %>%
  .[, c("x", "y") := proj4::project(list(lon, lat), proj_radar, inverse = FALSE)]

grilla <- CJ(x = seq(-680000, 990000, length.out = 100), y = seq(-1200000, 1100000, length.out = 100))


ranges <- map(rad_loc$nradar, function(n) {

  this_radar <- rad_loc[nradar == n]

  grilla[, ":="(x_radar = this_radar$x, y_radar = this_radar$y)] %>%
    .[, z := Mag(x - x_radar, y - y_radar)] %>%
    .[]

  range <- StatContour$compute_group(grilla, breaks = 240000) %>%
    setDT() %>%
    .[, c("lon", "lat") := proj4::project(list(x, y), proj_radar, inverse = TRUE)]


}) %>% rbindlist(idcol = "ID")

write_csv(ranges, here::here("analysis", "data", "derived_data", "radar_loc.csv"))



ranges %>%
  ggplot(aes(lon, lat)) +
  geom_path(aes(group = ID)) +
  geom_mapa()
