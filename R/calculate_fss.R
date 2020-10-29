#' Wrap para FSS
#'
#' Usa la función fss del paquete {verification} pero previamente requiere que las
#' variables estén en matrices. También puede iterar para distintos q (valor de pp) y
#' w (tamaño de la caja = w2+1)
#'
#' @param fcst matriz con precipitación pronosticada
#' @param obs matriz con precipitación observada (mismas dimensiones que fcst)
#' @param q número o vector numérico con umbrales a ser calculados
#' @param w número o vector numérico con escalas (w2+1)
#' @param progress lógico, muestra o no el progreso
#'
#' @export
calculate_fss <- function(fcst, obs, q, w, progress = TRUE) {
  out <- purrr::map_dfr(q, function(q) {
    fcst_q <- fcst >= q
    obs_q <- obs >= q

    return <- list(fss = purrr::map_dbl(w, ~ verification::fss(obs_q, fcst_q, .x)),
                   w = w,
                   q = rep(q, length(w)))

    if (progress == TRUE) {

      message(paste("Listo q = ", q))

    }

    return(return)
  })

  return(out)
}
