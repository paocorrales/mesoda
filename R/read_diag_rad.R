#' Lectura de archivos diag para radianzas
#'
#' Internamente define el sensor, plataforma, la fecha y el miembro del ensamble asociado.
#' El formato de nombre de archivo debería ser "asim_{sensor}_{plat}_{date}.{mem}"
#'
#' @param file_list lista de archivos
#' @param exp nombre del experimento asociado
#' @export
read_diag_rad <- function(file_list, exp) {

  diag <- purrr::map(seq_along(file_list), function(f){
    if (file.size(file_list[f]) == 0) {
      return(NULL)
    }
    meta <- unglue::unglue(basename(file_list[f]), "asim_{sensor}_{plat}_{date}.{mem}")
    # print(f)
    out <- data.table::fread(file_list[f])
    # .[V10 == 1] %>%

    if (file.size(file_list[f]) != 0) {
      out[, ":="(date = lubridate::ymd_hms(meta[[1]][["date"]]),
                 mem = meta[[1]][["mem"]],
                 exp = exp)]
    }
    out
  }) %>%
    rbindlist()

  colnames(diag) <- c("sensor", "channel", "freq", "lat", "lon", "press", "elev_sup", "dhr", "tb_obs", "tbc", "tbcnob",
                      "errinv", "qc", "emis", "tlapchn", "rzen", "razi", "rlnd", "rice", "rsnw", "rcld",
                      "rcldp", paste0("pred", seq(8)), "date", "mem", "exp")
  return(diag)
}

.datatable.aware <- TRUE
