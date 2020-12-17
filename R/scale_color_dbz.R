#' Reflectivity color pallete
#'
#' @param colour
#' @param limits
#' @param guide
#' @param ...
#'
#' @export
scale_color_dbz <- function(colours = hex,
                              limits = c(0, 60),

                              guide = guide_colorbar(barheight = 15),
                              ...) {

  colours <- rgb(ref$R, ref$G, ref$B, maxColorValue = 1)

  scale_color_gradientn(colours = colours,
                        limits = limits,
                        guide = guide, ...)
}

ref <- dplyr::tribble(
  ~R, ~G, ~B,
   1.0/255.0, 159.0/255.0, 244.0/255.0,
   3.0/255.0,   0.0/255.0, 244.0/255.0,
   2.0/255.0, 253.0/255.0,   2.0/255.0,
   1.0/255.0, 197.0/255.0,   1.0/255.0,
   0.0/255.0, 142.0/255.0,   0.0/255.0,
 253.0/255.0, 248.0/255.0,   2.0/255.0,
 229.0/255.0, 188.0/255.0,   0.0/255.0,
 253.0/255.0, 149.0/255.0,   0.0/255.0,
 253.0/255.0,   0.0/255.0,   0.0/255.0,
# 212.0/255.0,   0.0/255.0,   0.0/255.0,
 188.0/255.0,   0.0/255.0,   0.0/255.0,
 248.0/255.0,   0.0/255.0, 253.0/255.0,
 152.0/255.0,  84.0/255.0, 198.0/255.0
)
