#' Lectura de archivos diag para observaciones convencionales
#'
#' Internamente define la fecha.
#' El formato de nombre de archivo debería ser "asim_conv_{date}.{mem}"
#'
#' @param file_list lista de archivos
#' @param exp nombre del experimento asociado
#' @param member miembro del ensamble en caracteres. Si es la media correspode "000"
#' @param variable vector con las variables que se quieren obtener.
#' @export
# read_diag_mean ---------------------------------------------------------------
# Read mean diagfiles and tidy uv observations

read_diag_conv <- function(file_list, exp, member = "000", variable = c("uv", "p", "t", "q")) {

  obs <- purrr::map(file_list, function(f) {

    if (file.size(f) == 0) {
      return(NULL)
    }

    diag <- fread(f) %>%
      # .[V10 == 1] %>%
      .[, exp := exp] %>%
      .[, mem := member] %>%
      .[, date := ymd_hms(str_extract(basename(f), "\\d{14}"))] %>%
      .[, c("V2", "V4") := NULL]

    # cat("Archivo ", basename(files[f]))

    colnames(diag) <- c("var", "stationID", "type", "dhr", "lat", "lon", "pressure", "usage.flag", "flag.prep", "obs", "obs.guess", "obs2", "obs.guess2", "rerr", "exp", "mem", "date")

    # Parse only wind
    if ("uv" %in% variable & length(variable) == 1) {

      diag <- diag[var == "uv"] %>%
        melt(measure.vars = c("obs", "obs2", "obs.guess", "obs.guess2")) %>%
        .[, var := if_else(str_detect(variable, "2"), "v", "u")] %>%
        .[, variable := str_remove(variable, "2")]

      vars <- rlang::syms(setdiff(names(diag), "value"))
      diag <- diag %>%
        distinct(!!!vars, .keep_all = TRUE) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        setDT %>%
        .[, id := 1:.N, by = mem]

      # Parse wind and other variables
    } else if ("uv" %in% variable & length(variable) != 1) {


      uv <- diag[var == "uv"] %>%
        melt(measure.vars = c("obs", "obs2", "obs.guess", "obs.guess2")) %>%
        .[, var := if_else(str_detect(variable, "2"), "v", "u")] %>%
        .[, variable := str_remove(variable, "2")]

      vars <- rlang::syms(setdiff(names(uv), "value"))
      uv <- uv %>%
        distinct(!!!vars, .keep_all = TRUE) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        setDT

      variable <- c(variable, "u", "v")

      diag <- diag[var != "uv", -c("obs2", "obs.guess2"), with = FALSE] %>%
        rbind(uv) %>%
        .[var %in% variable] %>%
        .[, id := 1:.N, by = mem]

      # Parse other variables (not wind)
    } else {
      diag <- diag[var %in% variable, -c("obs2", "obs.guess2"), with = FALSE] %>%
        .[, id := 1:.N, by = mem]
    }

    diag[, obs := ifelse(obs == -1e+05, NA, obs)][]

  }) %>%
    rbindlist()
}
