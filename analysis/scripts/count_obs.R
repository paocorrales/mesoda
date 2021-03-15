files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E6/ANA/*/diagfiles/asim*ensmean")

diag <- read_diag_conv(files, "E6", variable = "uv")

diag[type %between% c(240, 260) & var == "u" & usage.flag == 1 &
       date %between% c(as_datetime("2018-11-20 18:00:00"), as_datetime("2018-11-23 12:00:00"))] %>%
  .[, sat_type := case_when(
    type == 240 ~ "GOES SW winds",
    type == 241 ~ "India",
    type == 242 ~ "JMA Visible",
    type == 243 ~ "EUMETSAT visible",
    type == 244 ~ "AVHRR winds",
    type == 245 ~ "GOES IR",
    type == 246 ~ "GOES WV cloud top",
    type == 247 ~ "GOES WV deep layer",
    type == 248 ~ "GOES cloud top (sounder)",
    type == 249 ~ "GOES deep layer (sounder)",
    type == 250 ~ "JMA WV deep layer",
    type == 251 ~ "GOES visible",
    type == 252 ~ "JMA IR winds",
    type == 253 ~ "EUMETSAT IR winds",
    type == 254 ~ "EUMETSAT WV deep layer winds",
    type == 257 ~ "MODIS IR",
    type == 258 ~ "MODIS WV cloud top",
    type == 259 ~ "MODIS WV deep layer winds",
    type == 260 ~ "VIIR IR winds")] %>%
  separate(sat_type, into = c("sat", "wn"), remove = FALSE) %>%
  .[, .N, by = sat]


diag <- read_diag_conv(files, "E6", variable = "t")

diag[type %between% c(130, 134) & usage.flag == 1] %>%
  .[, .N, by = type]


multiespectrales <- c("airs_aqua",
                      "iasi_metop-a",
                      "iasi_metop-b")

files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E7/ANA/*/diagfiles/asim*ensmean")

files <- files[str_detect(files, "conv", negate = TRUE)]

diag <- read_diag_rad(files, "E7")

diag[, tipo := fifelse(sensor %in% multiespectrales, "multiespectral", "no-multiespectral")] %>%
  satinfo[., on = c("sensor", "channel")] %>%
  .[errinv %between% c(0.0000316227, 1000) &
  qc == 0 &
  iuse == 1 &
  peakwt %between% c(0.001, 1200)] %>%
  .[, .(prop = round(.N*100/201100, 2)), by = sensor] %>%
  asimilable[., on = c("sensor")] %>%
 separate(sensor, into = c("sensor", "plataforma"), sep = "_") %>%
  write_csv("analysis/data/derived_data/tabla_radianzas.csv")


asimilable <- satinfo[iuse == 1, .N, by = sensor]

diag[tipo == "no-multiespectral", unique(sensor)]
