#' Rota los vientos e interpola a la retícula de masa
#'
#' La función calcula la rotación correcta e interpola a la reticula de masa.
#' Solo funciona para Labert
#'
#' @param ncfile path al archivo netCDF donde están los datos
#'
#' @export
uvmet <- function(ncfile) {

  nc <- ncdf4::nc_open(ncfile)

  # https://github.com/NCAR/wrf-python/blob/b40d1d6e2d4aea3dd2dda03aae18e268b1e9291e/src/wrf/g_uvmet.py#L179
  true_lat1 <- ncdf4::ncatt_get(nc, 0, "TRUELAT1")[[2]]
  true_lat2 <- ncdf4::ncatt_get(nc, 0, "TRUELAT2")[[2]]
  cen_lon <- ncdf4::ncatt_get(nc, 0, "STAND_LON")[[2]]

  lon <- ncdf4::ncvar_get(nc, "XLONG")
  lat <- ncdf4::ncvar_get(nc, "XLAT")

  u <- ncdf4::ncvar_get(nc, "U")
  v <- ncdf4::ncvar_get(nc, "V")

  rpd <- pi/180.0

  if ((abs(true_lat1 - true_lat2) > 0.1) & (abs(true_lat2 - 90.) > 0.1)) {
    cone = (log(cos(true_lat1*rpd)) -
              log(cos(true_lat2*rpd)))
    cone = (cone /
              (log(tan((45.- abs(true_lat1/2.))*rpd))
               - log(tan((45.- abs(true_lat2/2.)) *
                           rpd))))
  } else {
    cone = sin(abs(true_lat1)*rpd)
  }

  # https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L801

  longca <- lon - cen_lon

  longca[longca > 180] <- longca[longca > 180] - 360
  longca[longca < -180] <- longca[longca < -180] + 360

  longcb <- longca*cone*rpd            # Hemisferio norte
  longcb[lat < 0] <- -longcb[lat < 0]  # Corrección si el dominio está en el HS

  longca <- cos(longcb)
  longcb <- sin(longcb)

  # destagger
  u <- 0.5*(u[1:(nrow(u)-1), ,] + u[c(2:nrow(u)), , ])
  v <- 0.5*(v[, 1:(ncol(v)-1), ] + v[, c(2:ncol(v)), ])
  longcb <- rray::rray_broadcast(longcb, dim(v))
  longca <- rray::rray_broadcast(longca, dim(u))
  # desrotación
  umet <- v * longcb + u * longca
  vmet <- v * longca - u * longcb

  list(c(umet),
       c(vmet))
}
