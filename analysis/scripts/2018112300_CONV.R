library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
source(here::here("temp_R/postprocesamiento.R"))

geom_mapa <- function(fill = NA) {
  list(geom_sf(data = map_arg, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       geom_sf(data = map_limitrofes, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
       coord_sf(ylim = c(-34, -29), xlim = c(-66, -63)),
       scale_x_longitude(ticks = ),
       scale_y_latitude(ticks = 1))
}


diag <- read_diag_conv("/home/paola.corrales/datosmunin/EXP/E4/ANA/20181123000000/diagfiles/asim_conv_20181123000000.ensmean", exp = "CONV") %>%
  .[, ":="(lon = ConvertLongitude(lon),
           obs = fcase(var == "t", obs - 273.15,
                       var %in% c("q", "v", "u"), obs),
           run = "guess")]


diag[usage.flag == 1 & lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5), .N, by = .(var)]

diag[usage.flag == 1 & lat %between% c(-32, -30) & lon %between% c(-66, -64)] %>%
  .[var %in% c("t","q")] %>%
  ggplot(aes(obs.guess, pressure)) +
  geom_point(aes(color = factor(type))) +
  scale_y_level() +
  facet_wrap(~var, scales = "free_x")


diag_ana <- read_diag_conv("/home/paola.corrales/datosmunin/asim_conv_20181123000000.ensmean", exp = "CONV") %>%
  .[, ":="(lon = ConvertLongitude(lon),
           obs = fcase(var == "t", obs - 273.15,
                       var %in% c("q", "v", "u"), obs),
           run = "analisis")]


rbind(diag, diag_ana) %>%
  .[usage.flag == 1 & lat %between% c(-32, -30) & lon %between% c(-66, -64)] %>%
  .[var %in% c("q") & type == 120] %>%
  .[, guess := obs - obs.guess] %>%
  # .[]
  # .[, td := td(obs, pressure)] %>%
  melt(measure.vars = c("obs", "guess")) %>%
  ggplot(aes(pressure, value*1000)) +
  geom_line(data = ~.x[variable == "obs"]) +
  geom_point(data = ~.x[variable == "obs"], aes(shape = "obs")) +
  geom_line(data = ~.x[variable != "obs"], aes(color = run)) +
  geom_point(data = ~.x[variable != "obs"], aes(color = run)) +
  coord_flip() +
  scale_x_level() +
  labs(shape = NULL, color = NULL, y = "q (g/Kg)") +
  theme_minimal()
facet_wrap(~variable, scales = "free_x")


rbind(diag, diag_ana) %>%
  .[usage.flag == 1 & lat %between% c(-32, -30) & lon %between% c(-66, -64)] %>%
  .[var %in% c("q") & type == 120] %>%
  .[, guess := obs - obs.guess] %>%
  .[, ":="(td = td(obs, pressure*100),
           td_guess = td(guess, pressure*100))] %>%

  melt(measure.vars = c("td", "td_guess")) %>%
  ggplot(aes(pressure, value)) +
  geom_line(data = ~.x[variable == "td"]) +
  geom_point(data = ~.x[variable == "td"], aes(shape = "obs")) +
  geom_line(data = ~.x[variable != "td"], aes(color = run)) +
  geom_point(data = ~.x[variable != "td"], aes(color = run)) +
  coord_flip() +
  scale_x_level() +
  scale_y_continuous(breaks = seq(-40, 15, 5)) +
  labs(shape = NULL, color = NULL, y = "td (°C)") +
  theme_minimal()
facet_wrap(~variable, scales = "free_x")




diag_guess_sounding <- diag[stationID == 87344 & type == 120] %>%
  .[, guess := fifelse(var == "q", obs - obs.guess*1000, obs - obs.guess)]

diag_guess_sounding <- rbind(diag_guess_sounding, diag_guess_sounding[var == "q"] %>% .[, ":="(var = "td",
                                                                                       obs = td(obs/1000, pressure*100),
                                                                                       guess = td(guess/1000, pressure*100))] %>%
                               .[, obs.guess := obs - guess]) %>%
  .[, c("xp", "yp") := wrf_project(lon, lat, round = FALSE)]


guess <- ReadNetCDF("/home/paola.corrales/datosmunin/EXP/E4/GUESS/20181123000000/wrfarw.ensmean",
                    vars = c(p = "P", "PB", t = "T", qv = "QVAPOR",
                               lon = "XLONG", lat = "XLAT")) %>%
  .[, p := p + PB] %>%
  .[, t := tk(t, p)] %>%
  .[, rh := rh(qv, p, t)] %>%
  .[, td := td(qv, p) + 273.15] %>%
  .[, ":="(Time = NULL,
           west_east = NULL,
           south_north = NULL,
           PB = NULL)] %>%
  .[, ":="(time = ymd_hms(20181123000000))]


rx <- range(unique(diag_guess_sounding$lon), na.rm = TRUE) + c(-1, 1)
ry <- range(unique(diag_guess_sounding$lat), na.rm = TRUE) + c(-1, 1)

interp_guess <- guess %>%
  .[lat %between% ry & lon %between% rx] %>%
  melt(id.vars = c("bottom_top", "lon", "lat", "time")) %>%
  .[, c("xp", "yp") := wrf_project(lon, lat)] %>%
  .[, interp_lite(xp, yp, value,
                  xo = unique(diag_guess_sounding$xp),
                  yo = unique(diag_guess_sounding$yp),
                  output = "points"),
    by = .(bottom_top, variable, time)]

interp_guess <- interp_guess[variable != "p"] %>%
  .[interp_guess[variable == "p"], on = c("bottom_top", "time", "x", "y")] %>%
  setnames(c("x", "y", "z", "i.z", "variable"), c("xp", "yp", "value", "pressure", "var")) %>%
  .[, i.variable := NULL]


diag_ana_sounding <- diag_ana[stationID == 87344 & type == 120] %>%
  .[, guess := fifelse(var == "q", obs - obs.guess*1000, obs - obs.guess)]

diag_ana_sounding <- rbind(diag_ana_sounding, diag_ana_sounding[var == "q"] %>% .[, ":="(var = "td",
                                                                                       obs = td(obs/1000, pressure*100),
                                                                                       guess = td(guess/1000, pressure*100))] %>%
                               .[, obs.guess := obs - guess]) %>%
  .[, c("xp", "yp") := wrf_project(lon, lat, round = FALSE)]


ana <- ReadNetCDF("/home/paola.corrales/datosmunin/EXP/E4/ANA/20181123000000/analysis.ensmean",
                    vars = c(p = "P", "PB", t = "T", qv = "QVAPOR",
                             lon = "XLONG", lat = "XLAT")) %>%
  .[, p := p + PB] %>%
  .[, t := tk(t, p)] %>%
  .[, rh := rh(qv, p, t)] %>%
  .[, td := td(qv, p) + 273.15] %>%
  .[, ":="(Time = NULL,
           west_east = NULL,
           south_north = NULL,
           PB = NULL)] %>%
  .[, ":="(time = ymd_hms(20181123000000))]


rx <- range(unique(diag_ana_sounding$lon), na.rm = TRUE) + c(-1, 1)
ry <- range(unique(diag_ana_sounding$lat), na.rm = TRUE) + c(-1, 1)

interp_ana <- ana %>%
  .[lat %between% ry & lon %between% rx] %>%
  melt(id.vars = c("bottom_top", "lon", "lat", "time")) %>%
  .[, c("xp", "yp") := wrf_project(lon, lat)] %>%
  .[, interp_lite(xp, yp, value,
                  xo = unique(diag_ana_sounding$xp),
                  yo = unique(diag_ana_sounding$yp),
                  output = "points"),
    by = .(bottom_top, variable, time)]

interp_ana <- interp_ana[variable != "p"] %>%
  .[interp_ana[variable == "p"], on = c("bottom_top", "time", "x", "y")] %>%
  setnames(c("x", "y", "z", "i.z", "variable"), c("xp", "yp", "value", "pressure", "var")) %>%
  .[, i.variable := NULL]


diag_guess_sounding[var == "td" & pressure > 300] %>%
  ggplot(aes(pressure, obs)) +
  geom_point() +
  geom_line() +
  geom_line(data = interp_guess[var == "td" & pressure > 30000], aes(x = pressure/100, y = value - 273.15), color = "darkorange") +
  geom_line(data = interp_ana[var == "td" & pressure > 30000], aes(x = pressure/100, y = value - 273.15), color = "cyan4") +
  geom_point(data = interp_guess[var == "td" & pressure > 30000], aes(x = pressure/100, y = value - 273.15), color = "darkorange") +
  geom_point(data = interp_ana[var == "td" & pressure > 30000], aes(x = pressure/100, y = value - 273.15), color = "cyan4") +
  coord_flip() +
  scale_x_level()


diag_ana_sounding[var == "q", c("lon", "lat")] %>% unique(by = c("lon", "lat"))



ana[lat %between% c(-31.5, -31.1) & lon %between% c(-64.41, 64.01) & bottom_top %in% c(1:4)] %>%
  .[, c("xp", "yp") := wrf_project(lon, lat)] %>%
  .[, c("xref", "yref") := wrf_project(-64.21, -31.31)] %>%
  .[, dist := sqrt((xp -xref)^2 + (yp - yref)^2)] %>%
  .[which.min(dist)]

diag_ana_sounding[var == "q" & pressure > 300] %>%
  ggplot(aes(pressure, obs)) +
  geom_point() +
  geom_line() +
  geom_point(aes(y = guess), color = "cyan4") +
  geom_line(aes(y = guess), color = "cyan4") +
  scale_x_level() +
  coord_flip()


diag_guess_sounding[var == "q" & pressure > 300] %>%
  ggplot(aes(pressure, obs)) +
  geom_point() +
  geom_line() +
  geom_line(data = ana[lat %~% -31.32425 & lon %~% -64.19876 & p > 30000], aes(x = p/100, y = qv*1000), color = "cyan4") +
  geom_point(data = ana[lat %~% -31.32425 & lon %~% -64.19876 & p > 30000], aes(x = p/100, y = qv*1000), color = "cyan4") +
  scale_x_level() +
  coord_flip()





# ana - guess -------------------------------------------------------------

wrf_path <- "/home/paola.corrales/datosmunin/EXP/"

ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
             length.out = ciclos)

files_ana <- Sys.glob(paste0(wrf_path, "E[4-7]/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean"))

files_gue <- Sys.glob(paste0(wrf_path, "E[4-7]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean"))

ana <- purrr::map(files_ana, function(f) {

  metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/ANA/{fecha}/analysis.ensmean")


  ReadNetCDF(f, vars = c("QVAPOR", "P", "PB",
                         lon = "XLONG", lat = "XLAT"),
             subset = list(bottom_top = c(1:15))) %>%
    .[, ":="(td = td(QVAPOR, P+PB),
             exp = metadatos[[1]][["exp"]],
             date = metadatos[[1]][["fecha"]])]
  # %>%
  #   .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
}) %>%
  rbindlist() %>%
  .[, c("x", "y") := wrf_project(lon, lat)]

guess <- purrr::map(files_gue, function(f) {

  metadatos <- unglue(f, "/home/paola.corrales/datosmunin/EXP/{exp}/GUESS/{fecha}/wrfarw.ensmean")

  ReadNetCDF(f, vars = c(QVAPOR_g = "QVAPOR", "P", "PB",
                         lon = "XLONG", lat = "XLAT"),
             subset = list(bottom_top = c(1:15))) %>%
    .[, ":="(td_g = td(QVAPOR_g, P+PB),
             exp = metadatos[[1]][["exp"]],
             date = metadatos[[1]][["fecha"]])]
  # %>%
  #   .[, .(td2 = mean(td)), by = .(lon, lat, exp, date)]
}) %>%
  rbindlist() %>%
  .[, c("x", "y") := wrf_project(lon, lat)]
# %>%
#   setnames("td2", "td2_guess")



guess %>%
  .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = QVAPOR_g*1000, fill = stat(level_d)),
                    proj = norargentina_lambert,
                    breaks = seq(-6, 26, 2)) +
  scale_fill_viridis_c(super = ScaleDiscretised,
                       limits = c(-6, 26),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  geom_mapa() +
  facet_wrap(~exp) +
  labs(caption = paste0("ANA | ", dates[d]), fill = "~Td2m") +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))

cbind(ana[, c("QVAPOR")], guess) %>%
  # .[, QVAPOR_g := QVAPOR] %>%
  # .[, QVAPOR := NULL] %>%
  .[, diff := QVAPOR - QVAPOR_g] %>%
  # .[, ":="(x = NULL, y = NULL, lat = NULL, lon = NULL)] %>%
  .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = diff*1000, fill = stat(level_d)),
                    proj = norargentina_lambert) +
  scale_fill_divergent(super = ScaleDiscretised,
                       #                      # limits = c(1, 30),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  geom_mapa() +
  labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
  facet_grid(exp ~ bottom_top) +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))

patchwork::plot_layout(ncol = 2, widths = c(1, 1))

cbind(ana[, c("td")], guess) %>%
  # .[, QVAPOR_g := QVAPOR] %>%
  # .[, QVAPOR := NULL] %>%
  .[, diff := td - td_g] %>%
  # .[, ":="(x = NULL, y = NULL, lat = NULL, lon = NULL)] %>%
  .[lat %between% c(-34.5, -28.5) & lon %between% c(-66.5, -62.5)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = td_g, fill = stat(level_d)),
                    proj = norargentina_lambert) +
  scale_fill_divergent(super = ScaleDiscretised,
                       #                      # limits = c(1, 30),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  geom_mapa() +
  labs(caption = paste0("ANA - GUESS | ", dates[d]), fill = "~Td2m") +
  facet_grid(exp ~ bottom_top) +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))



