#' Humedad relativa
#'
#' @param QVAPOR vector con la humedad especifica en kg/kg
#' @param P vector con la presión en Pa
#' @param T vector con la temperatura en kelvin
#'
#' @export
# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L730

rh <- function(QVAPOR, P, T) {
  P <- P*0.01      # Debe estar en hPa
  T <- T - 273.15  # Debe estar en Celsius
  es <- 6.112 * exp(17.67*(T)/(T + 273.15 - 29.65))

  qvs <- es/(P - (1 - 0.622)*es)

  rh <- 100*pmax(pmin(QVAPOR/qvs, 1), 0)

  return(rh)
}
