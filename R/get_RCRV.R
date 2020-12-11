#' Calculate Reduced Centered Random Variable (RCRV)
#'
#' @param dt data.table with the diag file
#' @param tipo.var character, superficie == variables de superficie, perfil = variables en altura,
#' radianzas = randianzas.
#' @param calculo character, temporal == calculo en el tiempo, box = por cajas, nada = por variable
#'
#' @export
get_RCRV <- function(dt, tipo.var = "superficie", calculo = "temporal") {

  if ("temporal" %in% calculo) {
    if ("superficie" %in% tipo.var) {    # Variable de superficie a lo largo del tiempo
      dt[usage.flag == 1 & !is.na(obs)] %>%
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>%
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>%
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, date)]
    } else if ("perfil" %in% tipo.var) {  # Variables en altura a lo largo del tiempo
      dt[usage.flag == 1 & !is.na(obs)] %>%
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>%
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>%
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, error.level, date)]
    }
  } else if ("box" %in% calculo) {
    if ("superficie" %in% tipo.var) {
      dt[, ":="(lon.box = cut_round(lon, breaks = seq(284, 309, 2.5)),
               lat.box = cut_round(lat, breaks = seq(-42, -17, 2.5)))] %>%
        .[usage.flag == 1 & !is.na(obs)] %>%
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>%
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>%
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, lat.box, lon.box)]
    } else {
      stop(print("tipo.var debe ser superficie"))
    }
} else {
  if ("superficie" %in% tipo.var) {    # Variable de superficie
    dt[usage.flag == 1 & !is.na(obs)] %>%
      .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
               sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>%
      .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>%
      .[, .(mean.y = mean(y, na.rm = TRUE),
            sd.y = sd(y, na.rm = TRUE)), by = .(var, type)]
  } else if ("perfil" %in% tipo.var) { # Variable en altura
    dt[usage.flag == 1 & !is.na(obs)] %>%
      .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
               sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>%
      .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>%
      .[, .(mean.y = mean(y, na.rm = TRUE),
            sd.y = sd(y, na.rm = TRUE)), by = .(var, type, error.level)]
  }
}

}
