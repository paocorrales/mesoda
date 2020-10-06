# Help functions


# read_diag ---------------------------------------------------------------
# Read diagfiles and tidy uv observations

read_diag <- function(path, variable) {
  library(foreach)
  library(doParallel)
  
  files <- Sys.glob(paste0(path, ".mem*"))
  
  myCluster <- makeCluster(20)
  registerDoParallel(myCluster)
  
  obs <- foreach(f = 1:length(files),
                 .packages = c("data.table", "metR", "lubridate", "tidyverse"),
                 .export = c("files", "variable"),
                 .combine = "rbind") %dopar% {
                   
                   # sink("log.txt", append=TRUE)
                   
                   diag <- fread(files[f]) %>% 
                     .[V10 == 1] %>% 
                     .[, exp := basename(dirname(files[f]))] %>% 
                     .[, mem := str_extract(files[f], "\\d{3}$")] %>% 
                     .[, date := ymd_hms(str_extract(files[f], "\\d{14}"))] %>% 
                     .[, c("V2", "V4") := NULL]
                   
                   # cat("Archivo ", basename(files[f]))
                   
                   colnames(diag) <- c("var", "stationID", "type", "dhr", "lat", "lon", "pressure", "usage.flag", "flag.prep", "obs", "obs.guess", "obs2", "obs.guess2", "rerr", "exp", "mem", "date")
                   
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
                     
                   } else {
                     diag <- diag[var %in% variable, -c("obs2", "obs.guess2"), with = FALSE] %>% 
                       .[, id := 1:.N, by = mem] 
                   }
                   
                   diag[, obs := ifelse(obs == -1e+05, NA, obs)][]
                 }
  stopCluster(myCluster)
  return(obs)
}

# read_diag_mean ---------------------------------------------------------------
# Read mean diagfiles and tidy uv observations

read_diag_mean <- function(path, variable = c("uv", "p", "t", "q")) {
  
  
  files <- list.files(path, pattern = "asim_conv", full.names = TRUE,  recursive = TRUE)
  
  
  obs <- purrr::map(files, function(f) {
    
    if (file.size(f) == 0) {
      return(NULL)
    } 
    
    diag <- fread(f) %>% 
      # .[V10 == 1] %>% 
      .[, exp := basename(dirname(f))] %>% 
      .[, mem := "00"] %>% 
      .[, date := ymd_hms(str_extract(f, "\\d{14}"))] %>% 
      .[, c("V2", "V4") := NULL]
    
    # cat("Archivo ", basename(files[f]))
    
    colnames(diag) <- c("var", "stationID", "type", "dhr", "lat", "lon", "pressure", "usage.flag", "flag.prep", "obs", "obs.guess", "obs2", "obs.guess2", "rerr", "exp", "mem", "date")
    
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
      
    } else {
      diag <- diag[var %in% variable, -c("obs2", "obs.guess2"), with = FALSE] %>% 
        .[, id := 1:.N, by = mem] 
    }
    
    diag[, obs := ifelse(obs == -1e+05, NA, obs)][]
    
  }) %>% 
    rbindlist()
}


read_diag_mean_rad <- function(file_list) {
  
  diag <- purrr::map(file_list, function(f){
    if (file.size(f) == 0) {
      return(NULL)
    } 
    meta <- unglue::unglue(basename(f), "asim_{sensor}_{plat}_{date}.ensmean")
    # print(f)
    out <- fread(f)
    # .[V10 == 1] %>% 
    
    if (file.size(f) != 0) {
      out[, date := ymd_hms(meta[[1]][["date"]])]
    }
    out
  }) %>%
    rbindlist()
  
  colnames(diag) <- c("sensor", "channel", "freq", "lat", "lon", "press", "elev_sup", "dhr", "tb_obs", "tbc", "tbcnob",
                      "errinv", "qc", "emis", "tlapchn", "rzen", "razi", "rlnd", "rice", "rsnw", "rcld", 
                      "rcldp", paste0("pred", seq(8)), "date")
  return(diag)
}


# read satbias ------------------------------------------------------------

read_satbias <- function(file) {
  
  library(tidyverse)
  library(data.table)
  
  string <- read_lines(file)
  split_chunks <- function(string) {
    start <- seq(1, length(string), 3)
    end <- c(start-1, length(string))[-1]
    
    obs_type <- stringr::str_extract(string[start], "\\d+")
    
    chunks <- purrr::map(seq_along(start), ~ string[(start[.x]):end[.x]])
    chunks
  }
  
  string <- split_chunks(string)
  
  parse_chunk <- function(chunk) {
    str_split(chunk, " ") %>% 
      map( ~ str_subset(.x, "^$", negate = TRUE)) %>% 
      unlist(., recursive = FALSE) 
  }
  
  satbias <- map(string, parse_chunk) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    setNames(c("id", "sensor", "channel", "tlp1", "tlp2", "nc", 
               paste0("coeff", seq(12)))) %>% 
    mutate(across(channel:coeff12, ~as.numeric(as.character(.x))))
  
  return(satbias)
}

# input_obs_error ---------------------------------------------------------

input_obs_error <- function(variable, type, nivel, path_to_errtable = "errtable.csv") {
  errtable <- fread(path_to_errtable) %>% 
    .[, ":="(u = uv,
             v = uv, 
             uv = NULL,
             pw = NULL)] %>% 
    melt(id.vars = c("type", "nivel"))
  
  
  data.table(variable = variable, type = type, pressure = nivel) %>% 
    .[, nivel := errtable[metR::Similar(nivel, pressure), unique(nivel)], by = pressure] %>% 
    errtable[., on = .NATURAL] %>% 
    .[, .(value, nivel)]
  
}


# CR_bias -----------------------------------------------------------------

get_cr <-  function(dt, tipo = "superficie") {
  
  if ("temporal" %in% tipo) {
    if ("superficie" %in% tipo) {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, guess := obs - obs.guess] %>% 
        .[, ":="(guess.mean = mean(guess),
                 obs.guess.mean = mean(obs.guess),                # media de la innovación (y0 - H(xf)) para el ensamble 
                 var.guess = var(guess)), by = .(id, date)] %>%   # varianza de H(xf) calculada sobre el ensamble
        .[, ":="(d.mean = mean(obs.guess.mean)), by = .(var, type, date)] %>% # innovación promediada sobre el dominio para cada tipo de obs
        .[, .(rmsi = mean((obs.guess.mean - d.mean)^2),                       # <(d - <d>)^2>
              spread = error[1]^2 + mean(var.guess, na.rm = TRUE),            # error^2 + media de la varianza de H(xf) sobre el dominio
              bias = mean(obs.guess, na.rm = TRUE),                           # bias hecho y derecho, obs.guess = y0 - H(xf)
              count = .N),  by = .(var, type, date)] %>%                      # Cantidad de observaciones por tipo (x60)
        .[, cr := spread / rmsi]
    } else {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, guess := obs - obs.guess] %>% 
        .[, ":="(guess.mean = mean(guess),
                 obs.guess.mean = mean(obs.guess),
                 var.guess = var(guess)), by = .(id, date)] %>% 
        .[, ":="(d.mean = mean(obs.guess.mean)), by = .(var, type, nivel.error, date)] %>% 
        .[, .(rmsi = mean((obs.guess.mean - d.mean)^2),
              spread = error[1]^2 + mean(var.guess, na.rm = TRUE),
              bias = mean(obs.guess, na.rm = TRUE),
              count = .N),  by = .(var, type, nivel.error, date)] %>% 
        .[, cr := spread / rmsi]
    } 
  } else {
    if ("superficie" %in% tipo) {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, guess := obs - obs.guess] %>% 
        .[, ":="(guess.mean = mean(guess),
                 obs.guess.mean = mean(obs.guess),
                 var.guess = var(guess)), by = .(id, date)] %>% 
        .[, ":="(d.mean = mean(obs.guess.mean)), by = .(var, type)] %>% 
        .[, .(rmsi = mean((obs.guess.mean - d.mean)^2),
              spread = error[1]^2 + mean(var.guess, na.rm = TRUE),
              bias = mean(obs.guess, na.rm = TRUE),
              count = .N),  by = .(var, type)] %>% 
        .[, cr := spread / rmsi]
    } else {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, guess := obs - obs.guess] %>% 
        .[, ":="(guess.mean = mean(guess),
                 obs.guess.mean = mean(obs.guess),
                 var.guess = var(guess)), by = .(id, date)] %>% 
        .[, ":="(d.mean = mean(obs.guess.mean)), by = .(var, type, nivel.error)] %>% 
        .[, .(rmsi = mean((obs.guess.mean - d.mean)^2),
              spread = error[1]^2 + mean(var.guess, na.rm = TRUE),
              bias = mean(obs.guess, na.rm = TRUE),
              count = .N),  by = .(var, type, nivel.error)] %>% 
        .[, cr := spread / rmsi]
    }
  }
}


# RCRV --------------------------------------------------------------------

get_RCRV <- function(dt, tipo = "superficie") {
  
  if ("temporal" %in% tipo) {
    if ("superficie" %in% tipo) {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>% 
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>% 
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, date)]
    } else {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>% 
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>% 
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, nivel.error, date)]
    } 
  } else {
    if ("superficie" %in% tipo) {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>% 
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>% 
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type)]
    } else {
      dt[usage.flag == 1 & !is.na(obs)] %>% 
        .[, ":="(mean.guess = mean(obs - obs.guess, na.rm = TRUE),
                 sd.guess = sd(obs - obs.guess, na.rm = TRUE)), by = .(id, date)] %>% 
        .[, y := (obs - mean.guess)/sqrt(sd.guess^2 + error^2), by = .(id, date)] %>% 
        .[, .(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE)), by = .(var, type, nivel.error)]
    }
  }
  
}


# Label box ---------------------------------------------------------------

cut_round <- function(x, breaks) {
  labels <- na.omit((breaks + data.table::shift(breaks, -1))/2)
  cuts <- cut(x, breaks = breaks, labels = labels)
  
  as.numeric(as.character(cuts)) 
}


# Wrap FSS ----------------------------------------------------------------

# Usa la función fss del paquete verification pero previamente requiere que las
# variables estén en matrices. También puede iterar para distintos q (valor de pp) y
# w (tamaño de la caja = w2+1)

FSS <- function(fcst, obs, q, w) {
  out <- purrr::map_dfr(q, function(q) {
    fcst_q <- fcst >= q
    obs_q <- obs >= q
    
    return <- list(fss = purrr::map_dbl(w, ~ verification::fss(obs_q, fcst_q, .x)),
                   w = w,
                   q = rep(q, length(w)))
    message(paste("Listo q = ", q))
    return(return)
  })
  
  return(out)
}

# Parse radiosondes_RELAMPAGO ---------------------------------------------

library(data.table)
library(dplyr)

read_radiosonde_relampago <- function(file){
  # Leo línea por línea
  lines <- readLines(file)
  
  # Indices donde comienza cada sondeo
  idx <- which(grepl("Data Type:", lines))
  idx <- c(idx, length(lines)+1)
  soundings <- list()
  for (i in seq_len(length(idx)-1)) { 
    
    out <- read.table(text = lines[(idx[i] + 15):(idx[i + 1] - 1)]) %>% 
      as.data.table()
    
    names <- strsplit(lines[idx[i] + 12], " ")[[1]]
    names <- names[names != ""]
    colnames(out) <- names
    
    launch <- lubridate::ymd_hms(strsplit(lines[idx[i] + 4], "    ")[[1]][2])
    nominal_launch <- lubridate::ymd_hms(strsplit(lines[idx[i] + 11], "):")[[1]][2])
    site <- strsplit(lines[idx[i] + 2], "         ")[[1]][2]  
    
    out <- out[, ":="(Site = site,
                      Nominal_launch_time = nominal_launch,
                      Launch_time = launch)] %>% 
      .[, Time := seconds(Time) + Launch_time] %>% 
      .[, lapply(.SD, function(x) replace(x, as.character(x) %in% c("999", "9999", "999.0"), NA))] %>% 
      .[]
    if (length(colnames(out) == 24)) {
      colnames(out) <- c("time", "p", "t", "td", "rh", "u", "v", "spd", "dir", "w", "lon", "lat", "ele", 
                         "azi", "alt", "qp", "qt", "qrh", "qu", "qv", "qdZ", "site", "nominal_launch_time", 
                         "launch_time")
    } else {
      colnames(out) <- c("time", "p", "t", "td", "rh", "u", "v", "spd", "dir", "w", "lon", "lat", "ele", 
                         "azi", "alt", "qp", "qt", "qrh", "qu", "qv", "qdZ", "site", "nominal_launch_time", 
                         "launch_time", "mixr", "ptmp")
    }
    if (site == "Sao Borja, Brazil") {
      out <- out[, ":="(lon = data.table::nafill(lon, "locf"),
                        lat = data.table::nafill(lat, "locf"))]
    }
    
    soundings[[i]] <- out
  }
  soundings <- rbindlist(soundings, fill=TRUE)
}


# interp_lite -------------------------------------------------------------


interp_lite <- function (x, y = NULL, z, xo = seq(min(x), max(x), length = nx), 
                         yo = seq(min(y), max(y), length = ny), linear = (method == 
                                                                            "linear"), extrap = FALSE, duplicate = "error", dupfun = NULL, 
                         nx = 40, ny = 40, input = "points", output = "grid", method = "linear", 
                         deltri = "shull") 
{
  if (method == "linear") 
    linear <- TRUE
  is.sp <- FALSE
  sp.coord <- NULL
  sp.z <- NULL
  sp.proj4string <- NULL
  if (is.null(y) && is.character(z)) {
    if (class(x) == "SpatialPointsDataFrame" && requireNamespace("sp", 
                                                                 quietly = TRUE)) {
      sp.coord <- dimnames(sp::coordinates(x))[[2]]
      sp.z <- z
      sp.proj4string <- x@proj4string
      z <- x@data[, z]
      y <- sp::coordinates(x)[, 2]
      x <- sp::coordinates(x)[, 1]
      is.sp <- TRUE
      xo = seq(min(x), max(x), length = nx)
      yo = seq(min(y), max(y), length = ny)
    }
    else stop("either x,y,z are numerical or x is SpatialPointsDataFrame and z a name of a data column in x")
  }
  if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
    stop("missing values and Infs not allowed")
  drx <- diff(range(x))
  dry <- diff(range(y))
  if (drx == 0 || dry == 0) 
    stop("all data collinear")
  if (drx/dry > 10000 || drx/dry < 1e-04) 
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  nx <- length(xo)
  ny <- length(yo)
  if (length(y) != n || length(z) != n) 
    stop("Lengths of x, y, and z do not match")
  # dups_found <- isTRUE(anyDuplicated(cbind(x, y), MARGIN = 1) != 
  #                        0L)
  dups_found <- FALSE
  if (dups_found) {
    if (duplicate == "error") {
      stop("duplicate data points: need to set 'duplicate = ..' ")
    }
    else {
      xy <- paste(x, y, sep = ",")
      i <- match(xy, xy)
      if (duplicate == "user") 
        dupfun <- match.fun(dupfun)
      ord <- !duplicated(xy)
      if (duplicate != "strip") {
        centre <- function(x) switch(duplicate, mean = mean(x), 
                                     median = median(x), user = dupfun(x))
        z <- unlist(lapply(split(z, i), centre))
      }
      else {
        z <- z[ord]
      }
      x <- x[ord]
      y <- y[ord]
      n <- length(x)
    }
  }
  if (method == "linear" | method == "akima") {
    if (!linear) 
      stop("method=\"akima\" (linear=FALSE) is currently under developement and not yet available!")
    if (deltri == "deldir") {
      if (!linear) 
        stop("method=\"akima\" (linear=FALSE) is not implemented for deltri=\"deldir\"!")
      triangles <- deldir::triang.list(deldir::deldir(x = x, y = y, z = z))
      ans <- interp:::interpDeltri(xo, yo, z, triangles, input, 
                                   output)
    }
    else if (deltri == "shull") {
      ans <- interp:::interpShull(xo, yo, x, y, z, linear, input, 
                                  output)
      if (output == "points") 
        ans$z <- c(ans$z)
    }
    else stop(paste("unknown triangulation method", deltri))
  }
  else stop(paste("method=\"", method, "\" not implemented!", 
                  sep = ""))
  if (is.sp && requireNamespace("sp", quietly = TRUE)) {
    zm <- nx
    zn <- ny
    zvec <- c(ans$z)
    xvec <- c(matrix(rep(ans$x, zn), nrow = zm, ncol = zn, 
                     byrow = FALSE))
    yvec <- c(matrix(rep(ans$y, zm), nrow = zm, ncol = zn, 
                     byrow = TRUE))
    nona <- !is.na(zvec)
    ret <- data.frame(xvec[nona], yvec[nona], zvec[nona])
    names(ret) <- c(sp.coord[1], sp.coord[2], sp.z)
    sp::coordinates(ret) <- sp.coord
    ret@proj4string <- sp.proj4string
    sp::gridded(ret) <- TRUE
  }
  else {
    if (output == "grid") 
      ret <- list(x = ans$x, y = ans$y, z = matrix(ans$z, 
                                                   nx, ny))
    else ret <- list(x = ans$x, y = ans$y, z = ans$z)
  }
  ret
}
