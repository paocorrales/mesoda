
# projection --------------------------------------------------------------

goes_projection <- function(x, y, ncfile) {

    proj_info <- ncdf4::ncatt_get(ncfile, "goes_imager_projection")
  h <- proj_info$perspective_point_height 
  
  map_proj <- paste0("+proj=geos",
                     " +h=",  proj_info$perspective_point_height,
                     " +lon_0=", proj_info$longitude_of_projection_origin,
                     " +sweep=", proj_info$sweep_angle_axis,
                     " +ellps=GRS80")
  
  x_atr <- ncdf4::ncatt_get(ncfile, "x")
  x <- (x*x_atr$scale_factor + x_atr$add_offset)*h
  y_atr <- ncdf4::ncatt_get(ncfile, "y")
  y <- (y*y_atr$scale_factor + y_atr$add_offset)*h

  proj4::project(list(x, y), map_proj, inverse = TRUE)
}

# ggplot2::ggplot() +
#   
#   scattermore::geom_scattermost(data_sub[!is.na(z_norm), .(lon, lat)], interpolate = TRUE, pixels = c(512, 512),
#                                 viridisLite::viridis(100)[1 + 99*data_sub[!is.na(z_norm), z_norm]]) +
#   geom_path(data = map, aes(long, lat, group = group)) +
#   ggplot2::coord_equal(ylim = c(-45, -20), xlim = c(-77, -40))


# scale_color_topes -------------------------------------------------------

scale_color_topes <- function(colours = hex, 
                              limits = c(-90, 50), 
                              breaks = seq(-90, 50, 20),
                              guide = guide_colorbar(barheight = 15),
                              ...) {
  # https://github.com/garciafido/cima-goes/tree/master/src/cima/goes/img
  topes <- data.table::fread("paleta_topes")
  colours <- rgb(topes$V2, topes$V3, topes$V4, maxColorValue = 255)
  
  scale_color_gradientn(colours = colours, 
                        limits = limits, 
                        breaks = breaks, 
                        guide = guide, ...) 
}
# Planck ------------------------------------------------------------------

rad_to_tb <- function(rad, ncfile, h = 6.629e-34, c = 2.998e8, kb = 1.381e-23) {
  # https://www.unidata.ucar.edu/mailing_lists/archives/ldm-users/2018/msg00028.html
  # mu <- c(mu)
  rad <- c(rad)
  # tb <- h*c/(kb*mu*log(1 + 2*h*c^2/(rad*mu^5)))
  
  planck_fk1 <- ncdf4::ncvar_get(ncfile, "planck_fk1")
  planck_fk2 <- ncdf4::ncvar_get(ncfile, "planck_fk2")
  planck_bc1 <- ncdf4::ncvar_get(ncfile, "planck_bc1")
  planck_bc2 <- ncdf4::ncvar_get(ncfile, "planck_bc2")
  tb <- ( planck_fk2 / (log((planck_fk1 / rad) + 1 )) - planck_bc1) / planck_bc2
  
  tb <- tb - 273.15
  
  return(tb)
}


# Radiancia ---------------------------------------------------------------

calculate_rad <- function(rad, ncfile) {
  
  rad_atr <- ncdf4::ncatt_get(ncfile, "Rad")
  # rad <- rad*rad_atr$scale_factor + rad_atr$add_offset
  if_else(rad %between% rad_atr$valid_range, rad, NA_real_)
}
