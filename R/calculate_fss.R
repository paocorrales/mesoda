#' Wrap para FSS
#'
#' Usa la función fss del paquete {verification} pero previamente requiere que las
#' variables estén en matrices. También puede iterar para distintos q (valor de pp) y
#' w (tamaño de la caja = w2+1)
#'
#' @param fcst matriz con precipitación pronosticada
#' @param obs matriz con precipitación observada (mismas dimensiones que fcst)
#' @param q número o vector numérico con umbrales a ser calculados
#' @param w número o vector numérico con escalas, correspondiente al ancho de la caja
#'
#' @export
calculate_fss <- function(fcst, obs, q, w) {
# browser()
  fss <- lapply(q, function(q) {
    fcst_b <-  fcst > q
    fcst_b[, ] <- as.numeric(fcst_b)

    obs_b <- obs > q
    obs_b[, ] <- as.numeric(obs_b)

    fss <- SpatialVx::calculate_FSSvector_from_binary_fields(fcst_b, obs_b, w)
    data.table::data.table(w = w,
                           fss = fss,
                           q = rep(q, length(fss)))
  })

  data.table::rbindlist(fss)
}


# calculate_fss_old <- function(fcst, obs, q, w, progress = TRUE, plan = "cluster", workers = 3) {
#
#   obs[is.na(fcst)] <- NA
#
#   future::plan("cluster", workers = 3)
#
#   out <- furrr::future_map_dfr(q, function(q) {
#     fcst_q <- fcst >= q
#     obs_q <- obs >= q
#
#     return <- list(fss = furrr::future_map_dbl(w, ~ verification::fss(obs_q, fcst_q, .x)),
#                    w = w,
#                    q = rep(q, length(w)))
#
#     if (progress == TRUE) {
#
#       message(paste("Listo q = ", q))
#
#     }
#
#     return(return)
#   })
#
#   return(out)
# }
