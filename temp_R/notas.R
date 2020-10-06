obs[, ratio := obs.guess/max(1.3, min(5.6, rerr, na.rm = TRUE), na.rm = TRUE)] %>% 
  .[usage.flag == 1 & var == "t"] %>% 
  ggplot(aes(ratio)) +
  geom_density()


diag <- fread("E1/test/test.csv", na.strings = c("0.100E+11", "-0.100E+06", "-99999.90", "-100000.00"))

files <- Sys.glob("*/wrfout_d01_2018-11-20_18:00:00")

ana <- lapply(files, function(f) {
  ens <- substr(f, 1, 2)
  message(paste0("Leyendo el archivo ",f))
  ReadNetCDF(f, vars = c("T", "XLAT", "XLONG"), subset = list(bottom_top = 16)) %>% 
    .[, c("Time", "south_north", "west_east", "bottom_top") := NULL] %>% 
    .[, ens := ens] %>% 
    .[]
}) %>% 
  rbindlist()

colnames(ana) <- c("var", "lat", "lon", "ens")

spread <- ana %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean))]

print(paste0("El spread de T500 es ", spread))

copy(ana) %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean)), by = .(lon, lat)] %>% 
  ggplot(aes(lon, lat)) +
  geom_point(aes(color = sprd)) +
  scale_color_viridis_c() +
  labs(title = "Ensemble spread", subtitle = "Temperature, level 16 (~500 hPa)")
ggsave("T500.png")

ana <- lapply(files, function(f) {
  ens <- substr(f, 1, 2)
  message(paste0("Leyendo el archivo ",f))
  ReadNetCDF(f, vars = c("U", "XLAT_U", "XLONG_U"), subset = list(bottom_top = 16)) %>%
    .[, c("Time", "south_north", "west_east_stag", "bottom_top") := NULL] %>%
    .[, ens := ens] %>%
    .[]
}) %>%
  rbindlist()

colnames(ana) <- c("var", "lat", "lon", "ens")

spread <- ana %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean))]

print(paste0("El spread de U500 es ", spread))

copy(ana) %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean)), by = .(lon, lat)] %>% 
  ggplot(aes(lon, lat)) +
  geom_point(aes(color = sprd)) +
  scale_color_viridis_c() +
  labs(title = "Ensemble spread", subtitle = "Viento, level 16 (~500 hPa)")
ggsave("U500.png")

ana <- lapply(files, function(f) {
  ens <- substr(f, 1, 2)
  message(paste0("Leyendo el archivo ",f))
  ReadNetCDF(f, vars = c("QVAPOR", "XLAT", "XLONG"), subset = list(bottom_top = 9)) %>% 
    .[, c("Time", "south_north", "west_east", "bottom_top") := NULL] %>% 
    .[, ens := ens] %>% 
    .[]
}) %>% 
  rbindlist()

colnames(ana) <- c("var", "lat", "lon", "ens")

spread <- ana %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean))]

print(paste0("El spread de Q850 es ", spread))

copy(ana) %>% 
  .[, mean := mean(var), by = .(lon, lat)] %>% 
  .[, .(sprd = sd(var - mean)), by = .(lon, lat)] %>% 
  ggplot(aes(lon, lat)) +
  geom_point(aes(color = sprd)) +
  scale_color_viridis_c() +
  labs(title = "Ensemble spread", subtitle = "Humedad, level 9 (~850 hPa)")
ggsave("Q850.png")