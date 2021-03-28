#' Lee archivo "satbias" de coeficientes
#'
#' Esta función parsea el archivo satbias y lo ordena en formato tidy.
#'
#' @param file archivo a leer
#' @export
read_satbias <- function(file) {

  string <- read_lines(file)

  string <- split_chunks(string)

  satbias <- purrr::map(string, parse_chunk) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    setNames(c("id", "sensor", "channel", "tlp1", "tlp2", "nc",
               paste0("coeff", seq(12)))) %>%
    dplyr::mutate(across(channel:coeff12, ~as.numeric(as.character(.x)))) %>%
    dplyr::mutate(id = as.numeric(as.character(id)))

  return(satbias)
}

split_chunks <- function(string) {

  start <- seq(1, length(string), 3)
  end <- c(start-1, length(string))[-1]

  obs_type <- stringr::str_extract(string[start], "\\d+")

  chunks <- purrr::map(seq_along(start), ~ string[(start[.x]):end[.x]])
  chunks
}

parse_chunk <- function(chunk) {
  stringr::str_split(chunk, " ") %>%
    purrr::map( ~ stringr::str_subset(.x, "^$", negate = TRUE)) %>%
    unlist(., recursive = FALSE)
}
