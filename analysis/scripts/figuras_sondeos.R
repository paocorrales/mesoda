
# leo sondeos
# files <- list.files(path = "/home/paola.corrales/datosmunin/DA/DA_DATA/OBS_RELAMPAGO/sondeos_raw",
#                     pattern = "cls", full.names = TRUE)
#
# sondeos <- purrr::map(files, ~ read_radiosonde_relampago(.x)) %>%
#   rbindlist()
# sondeos %>%
#   unique(by = c("site", "nominal_launch_time")) %>%
# .[nominal_launch_time %between% c(as_datetime("2018-11-20 18:00:00"),
#                                   as_datetime("2018-11-23 12:00:00"))] %>%
#   .[, c("site", "lon", "lat", "nominal_launch_time")] %>%
#   fwrite("analysis/data/derived_data/lista_sondeos.csv")

run <- "ANA"

colores_exp <- c(E4 = "#0077BB", E5 = "#88CCEE", E6 = "#EE7733", E7 = "#CC3311")

files <- Sys.glob(paste0("/home/paola.corrales/datosmunin/EXP/derived_data/sondeos/", run, "/*"))

sondeos <- purrr::map(files, ~ fread(.x)) %>%
  rbindlist() %>%
  .[, value := fifelse(variable %in% c("t", "td"), value + 273.15, value)] %>%
  .[, var_type := fcase(variable %in% c("t", "td"), "temperatura",
                        variable %in% c("u", "v"), "viento")]

lista_sondeos <- fread("analysis/data/derived_data/lista_sondeos.csv")

for (s in seq_len(nrow(lista_sondeos))) {

# s <- 70
print(s)
this_site <- lista_sondeos$site[s]
this_lauch_time <- lista_sondeos$nominal_launch_time[s]


sondeos[site == this_site & nominal_launch_time == as_datetime(this_lauch_time) &
          !is.na(var_type) & alt %between% c(0, 5000)]  %>%
  ggplot(aes(alt, value)) +
  geom_line(data = ~.x[exp == "E7"], aes(linetype = variable)) +
  geom_line(aes(y = fcst_value, color = exp, linetype = variable)) +
  scale_color_manual(values = colores_exp, labels = c(E4 = "CONV", E5 = "AUT",
                                                      E6 = "SATWND", E7 = "RAD")) +
  scale_linetype_manual(values = c(1, 2, 1, 2)) +
  facet_wrap(~var_type, scales = "free_x") +
  coord_flip() +
  labs(caption = paste0(run, " | ", this_site, " | ", this_lauch_time),
       color = NULL, linetype = NULL, x = "Altura (m)", y = NULL) +
  theme_minimal()

ggsave(paste0("/home/paola.corrales/sondeos/", run, "/sondeo_", formatC(s, width = 3, flag = 0), "_", run, ".png"))

}


arg_topo <- metR::GetTopography(-75+360, -50+360, -20, -60, resolution = 1/10)
arg_topo[, lon := metR::ConvertLongitude(lon)]

cordillera <- list(
  ggnewscale::new_scale_fill(),
  geom_contour_fill(data = arg_topo, aes(x = lon, y = lat,
                                         z = h),
                    breaks = seq(400, 3000, by = 200)),
  scale_fill_gradient(low = "#E2E6E6", high = "#7E7E7E", guide = "none",
                      oob = scales::squish))


sites <- lista_sondeos[, dia := day(nominal_launch_time)]  %>%
  unique(by = c("site", "dia"))

lista_sondeos %>%
  .[lat %between% c(-34, -29) & lon %between% c(-66, -63)] %>%
  .[, dia := day(nominal_launch_time)] %>%
  .[, .N, by = .(site, dia)] %>%
  sites[., on = c("site", "dia")] %>%
  .[, site := gsub(", Argentina", ",\nArgentina", site)] %>%
  ggplot(aes(lon, lat)) +
  cordillera +
  geom_point(alpha = 0.8, color = "darkorange", aes(size = N)) +
  ggrepel::geom_label_repel(aes(label = site), force = 5, size = 3) +
  # geom_label(aes(label = site)) +
  scale_size_area(max_size = 5) +
  geom_mapa() +

  coord_sf(ylim = c(-34, -29), xlim = c(-66, -63)) +
  facet_wrap(~dia, ncol = 4)
  theme_minimal()


