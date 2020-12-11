#' Input conventional observation errors from errtable to diag files
#'
#' @param variable character
#' @param type numeric, BUFR code
#' @param level numeric
#' @param path_to_errtable path to errtable file
#'
#' @export
input_obs_error <- function(variable, type, level, path_to_errtable = "errtable.csv") {

    errtable <- data.table::fread(path_to_errtable) %>%
    .[, ":="(u = uv,
             v = uv,
             uv = NULL,
             pw = NULL)] %>%
    data.table::melt(id.vars = c("type", "nivel"))


  data.table::data.table(variable = variable, type = type, pressure = level) %>%
    .[, level := errtable[metR::Similar(nivel, pressure), unique(nivel)], by = pressure] %>%
    errtable[., on = c("type", "nivel" = "level", "variable")] %>%
    .[, .(value, nivel)]

}
