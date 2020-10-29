#' Transformación de escalas a symlog
#'
#' Permite transformar una escala (colores, ejes, etc.) a symlog.
#'
#' @references Un buen samaritano en https://stackoverflow.com/a/14674703
#'
#' @export
symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))

  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) *
             base^((sign(x) * x - thr) / scale) * thr)

  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      scales::pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(scales::pretty_breaks()(c(min(abs(x)), thr)),
                       scales::log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * scales::log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * scales::log_breaks()(c(max(abs(x)), thr)),
                 scales::pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-scales::log_breaks(base)(c(thr, -x[1])),
                 scales::pretty_breaks()(c(-thr, thr)),
                 scales::log_breaks(base)(c(thr, x[2]))))
    }
  }
  scales::trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}
