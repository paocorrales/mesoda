files <- Sys.glob("/home/paola.corrales3/datosmunin/EXP/E6/ANA/*/diagfiles/asim*ensmean")

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

asimilable <- satinfo[iuse == 1, .N, by = sensor]

files <- Sys.glob("/home/paola.corrales/datosmunin3/EXP/E8/ANA/*/diagfiles/asim*ensmean")

files <- files[str_detect(files, "conv", negate = TRUE)]

diag <- read_diag_rad(files, "E8")

diag[, tipo := fifelse(sensor %in% multiespectrales, "multiespectral", "no-multiespectral")] %>%
  satinfo[., on = c("sensor", "channel")] %>%
  .[errinv %between% c(0.0000316227, 1000) &
  qc == 0 &
  iuse == 1 &
  peakwt %between% c(0.001, 1200)] %>%
  .[, .(prop = round(.N*100/456774, 2)), by = sensor] %>%
  asimilable[., on = c("sensor")] %>%
 separate(sensor, into = c("sensor", "plataforma"), sep = "_") %>%
  write_csv("analysis/data/derived_data/tabla_radianzas.csv")


diag[tipo == "no-multiespectral", unique(sensor)]


a <- diag[, tipo := fifelse(sensor %in% multiespectrales, "multiespectral", "no-multiespectral")] %>%
  satinfo[., on = c("sensor", "channel")] %>%
  .[errinv %between% c(0.0000316227, 1000) &
      qc == 0 &
      iuse == 1 &
      peakwt %between% c(50, 1200)] %>%
  asimilable[., on = c("sensor")] %>%
  separate(sensor, into = c("sensor", "plataforma"), sep = "_") %>%
  .[, .N, by = .(sensor, plataforma, channel)]



a %>%
  .[sensor == "mhs"]


# Porcentaje de obs por experimento ---------------------------------------

files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E2/ANA/*/diagfiles/asim*ensmean")[1:67]

conv <- mesoda::read_diag_conv(files, exp = "E2", member = "000")

conv[, bufr_code := fcase(type %in% c(181, 187, 281, 287), "ADPSFC",
                          type %in% c(120, 220, 221), "ADPUPA",
                          type %in% c(130, 131, 133, 230, 231, 233), "AIRCFT",
                          type %in% c(290), "ASCATW",
                          type %in% c(180, 280, 183, 283, 184, 284), "SFCSHP",
                          type %in% c(240:260), "SATWND")]

conv[usage.flag == 1 & rerr != 1.0e+10] %>%
  .[, N := .N] %>%
  .[, .(exp = "E2",
        N = .N,
        proporcion = .N/unique(N)), by = bufr_code] %>%
  fwrite(here::here("analysis/data/derived_data/count_obs_E2.csv"))

files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E4/ANA/*/diagfiles/asim*ensmean")[1:67]

conv <- read_diag_conv(files, exp = "E4", member = "000")

conv[, bufr_code := fcase(type %in% c(181, 187, 281, 287), "ADPSFC",
                          type %in% c(120, 220, 221), "ADPUPA",
                          type %in% c(130, 131, 133, 230, 231, 233), "AIRCFT",
                          type %in% c(290), "ASCATW",
                          type %in% c(180, 280, 183, 283, 184, 284), "SFCSHP",
                          type %in% c(240:260), "SATWND")]

conv[usage.flag == 1 & rerr != 1.0e+10] %>%
  .[, N := .N] %>%
  .[, .(exp = "E4",
        N = .N,
        proporcion = .N/unique(N)), by = bufr_code] %>%
  fwrite(here("analysis/data/derived_data/count_obs_E4.csv"))


files <- files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E5/ANA/*/diagfiles/asim*ensmean")[1:67]

aut <- read_diag_conv(files, exp = "E5", member = "000")

aut[, bufr_code := fcase(type %in% c(181, 187, 281, 287), "ADPSFC",
                         type %in% c(120, 220, 221), "ADPUPA",
                         type %in% c(130, 131, 133, 230, 231, 233), "AIRCFT",
                         type %in% c(290), "ASCATW",
                         type %in% c(180, 280, 183, 283, 184, 284), "SFCSHP",
                         type %in% c(240:260), "SATWND")]

aut[usage.flag == 1 & rerr != 1.0e+10] %>%
  .[, N := .N] %>%
  .[, .(exp = "E5",
        N = .N,
        proporcion = .N/unique(N)), by = bufr_code] %>%
  fwrite(here("analysis/data/derived_data/count_obs_E5.csv"))

files <- files <- Sys.glob("/home/paola.corrales/datosmunin/EXP/E6/ANA/*/diagfiles/asim*ensmean")[1:67]

satwnd <- read_diag_conv(files, exp = "E6", member = "000")

satwnd[, bufr_code := fcase(type %in% c(181, 187, 281, 287), "ADPSFC",
                            type %in% c(120, 220, 221), "ADPUPA",
                            type %in% c(130, 131, 133, 230, 231, 233), "AIRCFT",
                            type %in% c(290), "ASCATW",
                            type %in% c(180, 280, 183, 283, 184, 284), "SFCSHP",
                            type %in% c(240:260), "SATWND")]

satwnd[usage.flag == 1 & rerr != 1.0e+10] %>%
  .[, N := .N] %>%
  .[, .(exp = "E6",
        N = .N,
        proporcion = .N/unique(N)), by = bufr_code] %>%
  fwrite(here("analysis/data/derived_data/count_obs_E6.csv"))
