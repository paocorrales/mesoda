#' Dew Point
#'
#' @param QVAPOR vector con la humedad especifica en kg/kg
#' @param P vector con la presión en Pa
#'
#'  @export
td <- function(QVAPOR, P) {

  # qv en kg/kg
  # p en pascales
  P <- P*0.01

  QVAPOR <- ifelse(QVAPOR < 0, 0, QVAPOR)

  tdc <- QVAPOR*P / (0.622 + QVAPOR)

  tdc <- ifelse(tdc < 0.001, 0.001, tdc)

  td <- (243.5*log(tdc) - 440.8) / (19.48 - log(tdc))

  return(td)
}

# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L970
