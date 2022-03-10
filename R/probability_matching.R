#' Calculate probability matching
#'
#' @param exp name of the experiment
#' @param run if "ana" or "fcst
#' @param ini_date init date and time
#' @param ciclos number of hourly cycles/fcst
#' @param acumulado time of accumulated
#' @param wrf_path where the simulations are located
#' @param out_path where the output file will be saved
#'
#' @export


calculate_probability_marching <- function(exp, run, ini_date, ciclos, acumulado = 1,
                                           wrf_path = "/home/paola.corrales/datosmunin3/EXP/",
                                           out_path = "/home/paola.corrales/datosmunin3/EXP/derived_data/ppacum/") {

  ini_date <- lubridate::ymd_hms(ini_date)

  members <- 60
  dates <- seq.POSIXt(ini_date, by = "hour",
                      length.out = ciclos)


  if (run == "ana") {
    pp_wrf_out <- purrr::map_dbl(dates, function(d) {

      date <- d

      print(d)

      files_wrf <- list.files(path = paste0(wrf_path, exp, "/", toupper(run), "/", format(date, "%Y%m%d%H%M%S")),
                              full.names = TRUE,
                              recursive = TRUE,
                              pattern = "analysis.mem*")

      pp_wrf <- purrr::map(files_wrf, function(f) {

        metadatos <- unglue::unglue(basename(f), "analysis.mem0{mem}")

        metR::ReadNetCDF(f, vars = c("RAINNC", "RAINC", "RAINSH",
                                     lon = "XLONG", lat = "XLAT")) %>%
          .[, ":="(pp_acum = RAINNC + RAINC + RAINSH,
                   mem = metadatos[[1]][["mem"]])] %>%
          .[, ":="(RAINNC = NULL,
                   RAINC = NULL,
                   RAINSH = NULL,
                   Time = NULL)]
      }) %>%
        data.table::rbindlist()


      pp_wrf_mean <- pp_wrf[, .(ens_mean = mean(pp_acum)), by = .(lon, lat)]

      pp_wrf_mean[, rank := data.table::frank(-ens_mean, ties.method = "first")]

      out <- pp_wrf[, rank := data.table::frank(-pp_acum, ties.method = "first")] %>%
        .[rank %in% metR::JumpBy(rank, by = members)] %>%
        .[, .(rank, pp_acum)] %>%
        .[, rank := data.table::frank(-pp_acum, ties.method = "first")] %>%
        .[pp_wrf_mean, on = "rank"] %>%
        .[, ":="(exp = exp,
                 date = date)] %>%
        .[, c("x", "y") := wrf_project(lon, lat)] %>%
        data.table::setnames("pp_acum", "pp_pmm")

      saveRDS(out, paste0(out_path, exp, "_", run, "_", format(date, "%Y%m%d%H"), "_pp_pmm.rds"))
      d
    })


  } else if (run == "fcst") {

    lead_time <- seq(acumulado, ciclos)

    out <- purrr::map(lead_time, function(f) {

      print(paste0(f, " of ", ciclos, " cycles"))

      this_date <- ini_date + lubridate::hours(f)
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



        if (!file.exists(file_wrf)) {
          return(NULL)
        }

        metR::ReadNetCDF(file_wrf, vars = c("PP", "XLAT", "XLONG")) %>%
          .[, ":="(lon = XLONG, lat = XLAT)] %>%
          .[, ":="(XLONG = NULL, XLAT = NULL)] %>%
          .[, ":="(exp = exp,
                   date = ini_date + lubridate::hours(f))] %>%
          data.table::setnames("ens", "mem")
      }) %>%
        data.table::rbindlist() %>%
        .[, .(pp_acum =  diff(PP), date = this_date), by = .(mem, exp, lon, lat)] %>%
        .[, c("x", "y") := wrf_project(lon, lat)]
      # %>%

      pp_wrf_mean <- pp_wrf[, .(ens_mean = mean(pp_acum)), by = .(lon, lat, date)]
      pp_wrf_mean[, rank := data.table::frank(-ens_mean, ties.method = "first")]

      pp_wrf[, rank := data.table::frank(-pp_acum, ties.method = "first")] %>%
        .[rank %in% metR::JumpBy(rank, by = members)] %>%
        .[, .(rank, pp_acum)] %>%
        .[, rank := data.table::frank(-pp_acum, ties.method = "first")] %>%
        .[pp_wrf_mean, on = "rank"] %>%
        .[, ":="(exp = exp,
                 ini_date = ini_date)] %>%
        .[, c("x", "y") := wrf_project(lon, lat)] %>%
        setnames("pp_acum", "pp_pmm")

    }) %>% data.table::rbindlist()

    saveRDS(out, paste0(out_path, exp, "_", run, "_", format(ini_date, "%Y%m%d%H"), "_pp_pmm.rds"))
  }
}












