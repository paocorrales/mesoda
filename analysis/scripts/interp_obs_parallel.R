# Script para interpolar el pronóstico a las observaciones
# Hay pronósticos 2018112200 a 36 horas y 2018112206 a 30 horas.
# El plan es generar un archivo .csv con la info del diag files y una nueva
# columna con el fcst para cada experiment.

#Librerías
library(tidyverse)
library(lubridate)
library(data.table)
library(metR)
library(unglue)
library(interp)
library(foreach)
library(doParallel)


obs_path <- "/home/paola.corrales/datosmunin/EXP/derived_data/interp_obs/"
fcst_path <- "/home/paola.corrales/datosmunin/EXP"

ini_date <- ymd_hms("20181122000000")

lead_time <- 2

# out <- foreach(l = 0:lead_time),
# .packages = c("data.table", "metR", "lubridate", "interp", "dplyr"),
# .export = c("files", "obs", "fecha_ini", "var_nc", "var_rra"),
# .combine = "rbind") %dopar% {

  # sink("log.txt", append=TRUE)

purrr::map_dbl(c(0:lead_time), function(l) {

  print(paste0("lead time = ", l))
  #Leo observaciones para ese tiempo
  files_obs <- Sys.glob(paste0(obs_path, format(ini_date, "%Y%m%d%H"), "/intep_conv_", format(ini_date + hours(l), "%Y%m%d%H%M%S"), ".*"))

  obs <- purrr::map(files_obs, function(f) {

    member <- unglue(basename(f), "intep_conv_{date}.mem{member}")

    obs <- mesoda::read_diag_conv(f, exp = "E5i", member = member[[1]][["member"]]) %>%
      .[type %in% c(181, 187, 281, 287)] %>%
      .[, mem := as.numeric(mem)]

  }) %>% rbindlist() %>%
    .[, id := .N, by = .(mem, lon, lat, var)]

  #Leo pronostico para ese tiempo
  files_fcst <- Sys.glob(paste0(fcst_path, "/E[4-7]/FCST/", format(ini_date, "%Y%m%d%H"),
                                "/NPP/NPP_",
                                format(ini_date, "%Y"), "-",
                                format(ini_date, "%m"), "-",
                                format(ini_date, "%d"), "_",
                                format(ini_date, "%H"), "_FC",
                                formatC(l, width = 2, flag = "0"),
                                ".nc"))

  fcst <- purrr::map(files_fcst, function(f) {

    fcst_exp <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{fcst_exp}/FCST/{info}")
    fcst <- ReadNetCDF(f, vars = c("XLONG", "XLAT", p = "PSFC",
                                   t = "T2",q = "Q2", u = "U10", v = "V10")) %>%
      .[, fcst_exp := fcst_exp[[1]][["fcst_exp"]]]

  }) %>% rbindlist()

  variables <- unique(obs$var)

  # Interpolo


  fcst_interp <- purrr::map(variables, function(v) {

    obs_subset <- obs[var == v & mem == 1, .(lon, lat)] %>%
      unique()

    myCluster <- makeCluster(20)
    registerDoParallel(myCluster)
    out_interp <- foreach(m = seq(1:60),
            .packages = c("data.table", "metR", "interp", "dplyr"),
            .export = c("fcst", "obs_subset", "v"),
            .combine = "rbind") %dopar% {

              temp <- fcst[ens == m, interp(XLONG, XLAT, get(..v), output = "points",
                                            xo = ConvertLongitude(obs_subset$lon), yo = obs_subset$lat),
                           by = .(ens, fcst_exp)] %>%
                setnames(c("x", "y", "z"), c("lon", "lat", "fcst")) %>%
                .[, var := v]
            }
    stopCluster(myCluster)

    out_interp

  })  %>%
    rbindlist() %>%
    .[, lon := ConvertLongitude(lon)] %>%
    dcast(ens + lon + lat + var ~ fcst_exp, value.var = "fcst")



    temp <- fcst_interp[obs, on = c("lon", "lat", "var", "ens" = "mem")]

    path_out <- paste0(obs_path, format(ini_date, "%Y%m%d%H"),
                       "/interp_conv_", format(ini_date + hours(l), "%Y%m%d%H%M%S"), ".rds")
    write_rds(temp, path_out)

  l
}) %>% rbindlist()





