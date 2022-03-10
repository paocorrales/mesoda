#' Calculate areal coverage
#'
#' @param exp name of the experiment
#' @param run if "ana" or "fcst
#' @param ini_date init date and time
#' @param ciclos number of hourly cycles/fcst
#' @param acumulado time of accumulated
#' @param q pp threshold
#' @param lon_band longitud limits
#' @param lat_band latitude limits
#' @param wrf_path where the simulations are located
#' @param out_path where the output file will be saved
#'
#' @export


calculate_areal_coverage <- function(exp, run, ini_date, ciclos, acumulado = 1,
                                     q = c(1, 5, 10, 25, 50),
                                     lon_band = c(-66.5, -61.5),
                                     lat_band = c(-35.5, -29),
                                     wrf_path = "/home/paola.corrales/datosmunin3/EXP/",
                                     out_path = "/home/paola.corrales/datosmunin3/EXP/derived_data/ppacum/") {

  ini_date <- lubridate::ymd_hms(ini_date)


  if (run == "ana") {

    dates <- seq.POSIXt(ini_date + lubridate::hours(acumulado), by = "hour",
                        length.out = ciclos - acumulado)

    pp_wrf_out <- purrr::map_dbl(dates, function(d) {

      date <- d

      print(d)

      files_wrf <-  purrr::map(seq(0, acumulado - 1), function(l) {
        list.files(path = paste0(wrf_path, exp, "/", toupper(run), "/", format(date - lubridate::hours(l), "%Y%m%d%H%M%S")),
                   full.names = TRUE,
                   recursive = TRUE,
                   pattern = "analysis.mem*")
      }) %>% unlist()


      pp_wrf <- purrr::map(files_wrf, function(f) {

        metadatos <- unglue::unglue(basename(f), "analysis.mem0{mem}")

        metR::ReadNetCDF(f, vars = c("RAINNC", "RAINC", "RAINSH",
                               lon = "XLONG", lat = "XLAT")) %>%
          .[, ":="(pp_acum = RAINNC + RAINC + RAINSH,
                   exp = exp,
                   date = date,
                   mem = metadatos[[1]][["mem"]])] %>%
          .[, ":="(RAINNC = NULL,
                   RAINC = NULL,
                   RAINSH = NULL,
                   Time = NULL)] %>%
          .[lon %between% lon_band & lat %between% lat_band]
      }) %>%
        data.table::rbindlist() %>%
        .[, .(pp_acum = sum(pp_acum)), by = .(exp, mem, date, lon, lat)] %>%
        .[, c("x", "y") := wrf_project(lon, lat)]


      pp_prop <- purrr::map(q, function(f){

        pp_wrf %>%
          .[, .(area = sum(pp_acum > f)/.N), by = .(mem, exp, date)] %>%
          .[, umbral := f]
      }) %>%
        data.table::rbindlist()


      saveRDS(pp_prop, paste0(out_path, exp, "_ana_", format(date, "%Y%m%d%H"), "_area_acum_", acumulado, "h.rds"))

      d
    })

  } else if (run == "fcst") {

    lead_time <- seq(acumulado, ciclos)

    out <- purrr::map(lead_time, function(f) {

      lt <- c(f - acumulado, f)

      pp_wrf <- map(lt, function(f){

        file_wrf <- paste0(wrf_path, exp, "/", toupper(run), "/",
                           format(ini_date, "%Y%m%d%H"), "/NPP/NPP_",
                           format(ini_date, "%Y"), "-",
                           format(ini_date, "%m"), "-",
                           format(ini_date, "%d"), "_",
                           format(ini_date, "%H"), "_FC",
                           formatC(f, width = 2, flag = "0"),
                           ".nc")

        print(file_wrf)

        if (!file.exists(file_wrf)) {
          return(NULL)
        }

        metR::ReadNetCDF(file_wrf, vars = c("PP", "XLAT", "XLONG")) %>%
          .[, ":="(lon = XLONG, lat = XLAT)] %>%
          .[, ":="(XLONG = NULL, XLAT = NULL)] %>%
          .[, ":="(exp = exp,
                   date = ini_date + lubridate::hours(f))] %>%
          .[lon %between% lon_band & lat %between% lat_band] %>%
          data.table::setnames("ens", "mem")
      }) %>%
        rbindlist() %>%
        .[, pp_acum := c(0, diff(PP)), by = .(mem, exp, lon, lat)] %>%
        .[, PP := NULL] %>%
        .[, c("x", "y") := wrf_project(lon, lat)]


      pp_prop <- purrr::map(q, function(f){

        pp_wrf %>%
          .[, .(area = sum(pp_acum > f)/.N), by = .(mem, exp, date)] %>%
          .[, umbral := f]
      }) %>%
        data.table::rbindlist()


    }) %>%
      data.table::rbindlist()

    saveRDS(out, paste0(out_path, exp, "_", run, "_", format(ini_date, "%Y%m%d%H"), "_area_acum_", acumulado, "h.rds"))
  }

}

# Acumulados horarios ------------------------------------------------

