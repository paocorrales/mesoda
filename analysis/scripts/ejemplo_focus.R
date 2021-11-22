# Función para convertir una matriz tidy en matriz, aplicar una función y
# devolver un vector ordenado
with_matrix <- function(formula, data = NULL, fun, fill = NULL) {
  f <- as.character(formula)

  if (length(f) == 1) {  # formula.tool did its thing
    f <- stringr::str_split(f, "~", n = 2)[[1]]
  } else {
    f <- f[-1]
  }

  value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

  matrix.vars <- f[stringr::str_detect(f, "\\|")]
  matrix.vars <- stringr::str_split(matrix.vars,"\\|", n = 2)[[1]]

  row.vars <- stringr::str_squish(stringr::str_split(matrix.vars[1], "\\+")[[1]])
  col.vars <- stringr::str_squish(stringr::str_split(matrix.vars[2], "\\+")[[1]])

  if (is.null(data)) {
    formula <- Formula::as.Formula(formula)
    data <- data.table::as.data.table(eval(quote(model.frame(formula, data  = data))))
  } else {
    # Check if columns are indata
    all.cols <- c(value.var, row.vars, col.vars)
    missing.cols <- all.cols[!(all.cols %in% colnames(data))]
    if (length(missing.cols) != 0) {
      stopf("Columns not found in data: %s.", paste0(missing.cols, collapse = ", "))
    }
    data <- data.table::setDT(data)[, (all.cols), with = FALSE]
  }

  data.table::setDT(data)
  dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
  dcast.formula <- stats::as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))
  value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

  g <- metR:::.tidy2matrix(data, dcast.formula, value.var, fill = fill)
  id.name <- "ff19bdd67ff5f59cdce2824074707d20"
  data[, (id.name) := 1:.N]
  id <- c(metR:::.tidy2matrix(data, dcast.formula, id.name)$matrix)

  g$matrix <- c(fun(g$matrix))

  c(g$matrix)[order(id)]
}


library(ggplot2)
library(terra)
#> terra version 1.4.11
library(raster)
#> Loading required package: sp
# Hace desvio estándard en una ventana movil de 3x3
rolling_2d_sd <- function(m) {
  as(m, "RasterLayer") %>%
    raster::focal(w = matrix(1/9, ncol = 3, nrow = 3), fun = sd) %>%
    as("matrix")

}

library(metR)





temperature[lev == 200] |>
  data.table::copy() |>
  D(, air_std := with_matrix(air ~ lon | lat, fun = rolling_2d_sd)) |>
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = air_std)) +
  geom_contour2(aes(z = air))
