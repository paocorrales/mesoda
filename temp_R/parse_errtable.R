library(tidyverse)
library(data.table)

string <- read_lines("prepobs_errtable.global")

split_chunks <- function(string, pattern = "OBSERVATION TYPE", negate = FALSE) {
  start <- stringr::str_which(string, pattern, negate = negate)
  end <- c(start-1, length(string))[-1]
  
  obs_type <- stringr::str_extract(string[start], "\\d+")
  
  chunks <- purrr::map(seq_along(start), ~ string[(start[.x]+1):end[.x]])
  
  names(chunks) <- obs_type  
  chunks
}

string <- split_chunks(string, "OBSERVATION TYPE")

chunk <- string[[1]]

parse_chunk <- function(chunk) {
  str_split(chunk, " ") %>% 
    map( ~ str_subset(.x, "^$", negate = TRUE) %>% 
           setNames(c("P", "T", "Q", "U", "V", "PW")) %>%
           as.numeric()) %>% 
    do.call(rbind, .) %>% 
    as.data.frame()
}

errtable <- map(string, parse_chunk) %>% 
  rbindlist(idcol = "type") 

na_fill <- function(x, na.char) {
  nas <- grepl(na.char, x)
  x[nas] <- NA
  return(x)
}

errtable %>% 
  mutate(type = as.numeric(type)) %>% 
  rename(nivel = V1, 
         t = V2,
         rh = V3,
         uv = V4,
         ps = V5, 
         pw = V6) %>% 
  mutate_all(~ifelse(. == 1e+09, NA, .)) %>% 
  write_csv("errtable.csv")
