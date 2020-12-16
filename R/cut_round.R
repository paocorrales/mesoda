#' Discretiza una variable en cajas y devuelve labes lindos
#'
#' @param x variable a discretizar
#' @param breaks vector con breaks
#'
#' @export
cut_round <- function(x, breaks) {

  labels <- na.omit((breaks + data.table::shift(breaks, -1))/2)
  cuts <- cut(x, breaks = breaks, labels = labels)

  as.numeric(as.character(cuts))
}
