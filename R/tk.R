#' Temperatura
#'
#' @param T vector con la temperatura potencial en kelvin
#' @param P vector con la presión en Pa
#' @param T_BASE valor numérico con la temperatura de base del WRF, usualmente 300 K
#'
#' @export
# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L57

tk <- function(T, P, T_BASE = 300){
  tk <- (T + T_BASE)*((P/100000)^(2/7))
}
