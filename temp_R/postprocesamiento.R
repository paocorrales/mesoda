
# Post procesamiento WRF --------------------------------------------------

## Humedad relativa
# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L730

rh <- function(QVAPOR, P, T) {
  P <- P*0.01      # Debe estar en hPa
  T <- T - 273.15  # Debe estar en Celsius
  es <- 6.112 * exp(17.67*(T)/(T + 273.15 - 29.65))

  qvs <- es/(P - (1 - 0.622)*es)

  rh <- 100*pmax(pmin(QVAPOR/qvs, 1), 0)

  return(rh)
}

## Dew Point
# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L970

td <- function(QVAPOR, P) {
  P <- P*0.01

  QVAPOR <- ifelse(QVAPOR < 0, 0, QVAPOR)

  tdc <- QVAPOR*P / (0.622 + QVAPOR)

  tdc <- ifelse(tdc < 0.001, 0.001, tdc)

  td <- (243.5*log(tdc) - 440.8) / (19.48 - log(tdc))

  return(td)
}

## T
# https://github.com/NCAR/wrf-python/blob/d9585354c0e2a75a0f7c1d6b200d353f5e4eb084/fortran/wrf_user.f90#L57

tk <- function(T, P, T_BASE = 300){
  tk <- (T + T_BASE)*(P/100000)^(2/7)
}

## UV
# Rota los vientos e interpola a la retícula de masa
# Solo para Labert

uvmet <- function(ncfile) {
  library(ncdf4)
  nc <- nc_open(ncfile)

  # https://github.com/NCAR/wrf-python/blob/b40d1d6e2d4aea3dd2dda03aae18e268b1e9291e/src/wrf/g_uvmet.py#L179
  true_lat1 <- ncatt_get(nc, 0, "TRUELAT1")[[2]]
  true_lat2 <- ncatt_get(nc, 0, "TRUELAT2")[[2]]
  cen_lon <- ncatt_get(nc, 0, "STAND_LON")[[2]]

  lon <- ncvar_get(nc, "XLONG")
  lat <- ncvar_get(nc, "XLAT")

  u <- ncvar_get(nc, "U")
  v <- ncvar_get(nc, "V")

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


# wrf_proj ----------------------------------------------------------------


# wrf_project <- function(lon, lat, inverse = FALSE, round = TRUE) {
#   map_proj <- "+proj=lcc +lat_1=-30.9659996032715 +lat_2=-30.9659996032715 +lat_0=-30.9660034179688 +lon_0=-63.5670013427734 +a=6370000 +b=6370000"
#   xy <- proj4::project(list(lon, lat), map_proj, inverse = inverse)
#   if (round == TRUE) {
#   list(x = round(xy$x, -3),
#        y = round(xy$y, -4))
#   } else {
#     list(x = xy$x,
#          y = xy$y)
#   }
# }
