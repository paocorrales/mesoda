library(mesoda)
library(metR)
library(tidyverse)
library(lubridate)
library(data.table)

miembros <- 60

IOP <- tribble(
  ~iop, ~ini, ~end,
  "IOP07", ymd_hms("20181121150000"), ymd_hms("20181121210000"),
  "IOP08", ymd_hms("20181122140000"), ymd_hms("20181122200000")
) %>% setDT()


files <- Sys.glob("/home/paola.corrales/datosmunin3/EXP/E[2,5,6,8]/FCST/*/sondeos/*")


estadisticos <- list()

for (m in seq_len(miembros)) {

  print(m)

  files_mem <- files[str_detect(files, paste0("_", formatC(m, width = 2, flag = 0),"_"))]

  print(files_mem)
  sondeos <- purrr::map(files_mem, function(f){

    fread(f) %>%
      .[, value := fifelse(variable %in% c("t", "td"), value + 273.15, value)] %>%
      .[, launch_time := as_datetime(launch_time)] %>%
      .[, fcst_exp := basename(dirname(dirname(f)))] %>%
      .[, mem := m]

  }) %>%
    rbindlist() %>%
    .[p >= 50] %>%
    .[, iop := fcase(launch_time %between% c(ymd_hms("20181121150000"), ymd_hms("20181121210000")), "IOP07",
                     launch_time %between% c(ymd_hms("20181122140000"), ymd_hms("20181122200000")), "IOP08",
                     default = "No IOP")] %>%
    .[variable %in% c("t", "td", "u", "v") & !str_detect(site, "/") & !is.na(fcst_value) & !is.na(iop)] %>%
    .[site != "Sao Borja, Brazil"] %>%
    .[, lev := cut_round(alt, c(seq(0, 3000, 500), seq(4000, 21000, 1000)))]


  estadisticos[[m]] <- sondeos %>%
    .[, .(RMSE = mean(sqrt((value - fcst_value)^2), na.rm = TRUE),
          BIAS = mean(value - fcst_value, na.rm = TRUE)),
      by = .(iop, exp, variable, lev, fcst_exp, mem)] %>%
    .[]

  if (m == 1) {
    print(paste0("miembro ", m, " (init media)"))
    media <- sondeos[, c("time", "lon", "lat", "alt",
                         "site", "launch_time",
                         "xp", "yp", "variable", "value", "exp", "member", "fcst_value",
                         "fcst_exp", "iop", "lev")] %>%
      unique(by = c("time", "lon", "lat", "alt",
                    "site", "launch_time",
                    "xp", "yp", "variable", "value", "exp",
                    "fcst_exp", "iop", "lev"))

  } else if (m %between% c(2, 59)) {
    print(paste0("miembro ", m, " (sumo media)"))

    sondeo <- sondeos[, c("time", "lon", "lat", "alt",
                          "site", "launch_time",
                          "xp", "yp", "variable", "value", "exp", "member", "fcst_value",
                          "fcst_exp", "iop", "lev")] %>%
      unique(by = c("time", "lon", "lat", "alt",
                    "site", "launch_time",
                    "xp", "yp", "variable", "value", "exp",
                    "fcst_exp", "iop", "lev"))

    media <- sondeo[media, on = c("time", "lon", "lat", "alt",
                                  "site", "launch_time",
                                  "xp", "yp", "variable", "value", "exp",
                                  "fcst_exp", "iop", "lev")] %>%
      .[, fcst_value := fcst_value + i.fcst_value] %>%
      .[, ":="(i.fcst_value = NULL,
               i.member = NULL)]

  } else {
    print(paste0("miembro ", m, " (calculo media)"))


    sondeo <- sondeos[, c("time", "lon", "lat", "alt",
                          "site", "launch_time",
                          "xp", "yp", "variable", "value", "exp", "member", "fcst_value",
                          "fcst_exp", "iop", "lev")] %>%
      unique(by = c("time", "lon", "lat", "alt",
                    "site", "launch_time",
                    "xp", "yp", "variable", "value", "exp",
                    "fcst_exp", "iop", "lev"))

    media <- sondeo[media, on = c("time", "lon", "lat", "alt",
                                  "site", "launch_time",
                                  "xp", "yp", "variable", "value", "exp",
                                  "fcst_exp", "iop", "lev")] %>%
      .[, fcst_value := (fcst_value + i.fcst_value)/60] %>%
      .[, ":="(i.fcst_value = NULL,
               i.member = NULL)]

  }


}


estadisticos %>%
  rbindlist() %>%
  write_rds("/home/paola.corrales/datosmunin/EXP/derived_data/sondeos_agregados.rds")

write_rds(media, "/home/paola.corrales/datosmunin/EXP/derived_data/sondeos_media.rds")


