library(tidyverse)
library(lubridate)
library(data.table)
library(unglue)
library(metR)
library(mesoda)
source(here::here("temp_R/postprocesamiento.R"))

fisica <- data.table(mem = as.character(formatC(1:60, flag = "0", width = 3)),
                     fisica = rep(c("KF-YSU",
                                    "BMJ-YSU",
                                    "GF-YSU",
                                    "KF-MYJ",
                                    "BMJ-MYJ",
                                    "GF-MYJ",
                                    "KF-MYNN2",
                                    "BMJ-MYNN2",
                                    "GF-MYNN2"), length.out = 60)) %>% setDT()
# geom_mapa <- function(fill = NA) {
#   list(geom_sf(data = map_arg, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
#        geom_sf(data = map_limitrofes, fill = fill, color = "black", size = 0.1, inherit.aes = FALSE),
#        coord_sf(ylim = c(-34, -29), xlim = c(-66, -63)),
#        scale_x_longitude(ticks = 1.5),
#        scale_y_latitude(ticks = 1.5))
# }

coord <- ReadNetCDF("/home/paola.corrales/datosmunin3/EXP/E2/ANA/20181120180000/analysis.ensmean",
                    vars = c(lon = "XLONG", lat = "XLAT")) %>%
  .[, Time := NULL]

wrf_path <- "/home/paola.corrales/datosmunin3/EXP/"
ini_date <- ymd_hms("20181120180000")
ciclos <- 67

dates <- seq(ini_date, by = "hour",
             length.out = ciclos)

cor <- list()
cor_fisicas <- list()

for (d in seq_along(dates)) {

  print(dates[d])


  files_ana <- Sys.glob(paste0(wrf_path, "E8/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.mem*"))

  # files_gue <- Sys.glob(paste0(wrf_path, "E[2,5,6,8]/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.mem*"))

  ana <- purrr::map(files_ana, function(f) {

    metadatos <- unglue(f, "/home/paola.corrales/datosmunin3/EXP/{exp}/ANA/{fecha}/analysis.mem{mem}")


    ReadNetCDF(f, vars = c("QVAPOR", "P", "PB", "T",
                           lon = "XLONG", lat = "XLAT"),
               subset = list(south_north = c(40, 299),
                            west_east  = c(70, 180))) %>%
      .[, ":="(td = td(QVAPOR, P+PB),
               t = tk(T, P+PB, T_BASE = 290),
               exp = metadatos[[1]][["exp"]],
               mem = metadatos[[1]][["mem"]],
               date = metadatos[[1]][["fecha"]])] %>%
      .[, ":="(P = NULL,
               PB = NULL,
               T = NULL,
               QVAPOR = NULL)] %>%
      fisica[., on = .NATURAL]
      # .[, .(td = mean(td)), by = .(lon, lat, exp, mem, date)]
  }) %>%
    rbindlist()

  # cor[[d]] <- ana[, .(cor = cor(t, td)), by = .(bottom_top, exp, date)]
  # cor_fisicas[[d]] <- ana[, .(cor = cor(t, td)), by = .(bottom_top, exp, date, fisica)]

  ana[bottom_top == 1, .(cor = cor(t, td)), by = .(bottom_top, south_north, west_east, exp, date)] %>%
    coord[., on = .NATURAL] %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = cor, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-1, 1, 0.2)) +
    geom_contour2(aes(z = cor),
                  color = "grey70", size = 0.1,
                  proj = norargentina_lambert,
                  breaks = seq(-1, 1, 0.2)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("RAD | sigma level = 1 | ", dates[d]), fill = NULL) +
    # facet_wrap(~exp, labeller = labeller(exp = c(E2 = "CONV", E5 = "AUT",
    #                                              E6 = "SATWND", E8 = "RAD"))) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2))

  ggsave(paste0("../campos/cor_E8_", dates[d], ".png"))

  ana[bottom_top == 1, .(cor = cor(t, td)), by = .(bottom_top, south_north, west_east, exp, date, fisica)] %>%
    coord[., on = .NATURAL] %>%
    .[, c("x", "y") := wrf_project(lon, lat)] %>%
    ggplot(aes(x, y)) +
    geom_contour_fill(aes(z = cor, fill = stat(level_d)),
                      proj = norargentina_lambert,
                      breaks = seq(-1, 1, 0.2)) +
    geom_contour2(aes(z = cor),
                  color = "grey70", size = 0.1,
                  proj = norargentina_lambert,
                  breaks = seq(-1, 1, 0.2)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
    geom_mapa() +
    labs(caption = paste0("RAD | sigma level = 1 | ", dates[d]), fill = "cor(t, td)") +
    facet_wrap(~fisica) +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2))
  ggsave(paste0("../campos/cor_E8_param_", dates[d], ".png"), height = 5, width = 5)
}

write_rds(cor, "/home/paola.corrales/datosmunin3/EXP/derived_data/cor_E8_perfil.rds")
write_rds(cor_fisicas, "/home/paola.corrales/datosmunin3/EXP/derived_data/cor_E8_param_perfil.rds")

cor %>%
  rbindlist() %>%
  .[, date := ymd_hms(date)] %>%
  .[!is.na(cor)] %>%
  ggplot(aes(date, bottom_top)) +
  geom_contour_fill(aes(z = cor, fill = stat(level_d)),
                    breaks = seq(-1, 1, 0.1)) +
    geom_contour2(aes(z = cor),
                  color = "grey70", size = 0.1,
                  breaks = seq(-1, 1, 0.1)) +
    scale_fill_divergent(super = ScaleDiscretised,
                         #                      # limits = c(1, 30),
                         guide = guide_colorsteps(barwidth = 0.5,
                                                  barheight = 15)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_date() +
  labs(fill = "cor(t,td)",
       caption = "RAD",
       x = NULL,
       y = "Sigma level") +
    theme_minimal(base_size = 10) +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(linetype = 3, size = 0.2))

ggsave(paste0("../campos/cor_E8_hov.png"))

cor_fisicas %>%
  rbindlist() %>%
  .[, date := ymd_hms(date)] %>%
  .[!is.na(cor)] %>%
  ggplot(aes(date, bottom_top)) +
  geom_contour_fill(aes(z = cor, fill = stat(level_d)),
                    breaks = seq(-1, 1, 0.1)) +
  geom_contour2(aes(z = cor),
                color = "grey70", size = 0.1,
                breaks = seq(-1, 1, 0.1)) +
  scale_fill_divergent(super = ScaleDiscretised,
                       #                      # limits = c(1, 30),
                       guide = guide_colorsteps(barwidth = 0.5,
                                                barheight = 15)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_date() +
  facet_wrap(~fisica) +
  labs(fill = "cor(t,td)",
       caption = "RAD",
       x = NULL,
       y = "Sigma level") +
  theme_minimal(base_size = 10) +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3, size = 0.2))

ggsave(paste0("../campos/cor_E8_param_hov.png"), height = 5, width = 5)
