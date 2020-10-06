#' Proyección de coordenadas
#'
#' Proyecta y reproyecta coordenadas utilizando el paquete proj4
#'
#' @param lon,lat longitud y latitud o x e y para calcular la inversa
#' @param inverse si es FALSE la función devuelve las coordenadas cartográficas (distancia al centro del dominio).
#' Si es TRUE devuelve lon, lat.
#' @param round igual a TRUE resuelve problemas de redondeo
#' @param map_proj recibe un string con la proyección correspondiente en el formato aceptado por proj4
#' @export

wrf_project <- function(lon, lat, inverse = FALSE, round = TRUE, map_proj = norargentina_lambert) {

  xy <- proj4::project(list(lon, lat), map_proj, inverse = inverse)
  if (round == TRUE) {
    list(x = round(xy$x, -3),
         y = round(xy$y, -4))
  } else {
    list(x = xy$x,
         y = xy$y)
  }
}

#' @export
norargentina_lambert <- "+proj=lcc +lat_1=-30.9659996032715 +lat_2=-30.9659996032715 +lat_0=-30.9660034179688 +lon_0=-63.5670013427734 +a=6370000 +b=6370000"
