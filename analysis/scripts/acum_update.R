exp <- "E7"
ini_date <- ymd_hms("20181120180000")
ciclos <- 50

dates <- seq.POSIXt(ini_date, by = "hour",
                    length.out = ciclos)




for (d in seq_along(dates)) {

  print(d)

  path_ana <- paste0("~/datosmunin/EXP/", exp, "/ANA/", format(dates[d], "%Y%m%d%H%M%S"), "/analysis.ensmean")
  path_gue <- paste0("~/datosmunin/EXP/", exp, "/GUESS/", format(dates[d], "%Y%m%d%H%M%S"), "/wrfarw.ensmean")

  update <- rbind(ReadNetCDF(path_ana,
                             vars = c(p = "P", "PB", qv = "QVAPOR",
                                      lon = "XLONG", lat = "XLAT")) %>%
                    .[, p := p + PB] %>%
                    .[, ":="(Time = NULL,
                             west_east = NULL,
                             south_north = NULL,
                             PB = NULL,
                             run = "ana")],

                  ReadNetCDF(path_gue,
                             vars = c(p = "P", "PB", qv = "QVAPOR",
                                      lon = "XLONG", lat = "XLAT")) %>%
                    .[, p := p + PB] %>%
                    .[, ":="(Time = NULL,
                             west_east = NULL,
                             south_north = NULL,
                             PB = NULL,
                             run = "guess")]) %>%
    # .[, ":="(p_lev = cut_round(p/100, breaks = seq(0, 1050, 50)),
    .[, ":="(exp = exp,
             date = dates[d])] %>%
    .[, .(qv = mean(qv)), by = .(lon, lat, bottom_top, run, exp, date)] %>%
    dcast(bottom_top + lon + lat + exp + date ~ run, value.var = "qv") %>%
    .[, ":="(qv = ana - guess)] %>%
    .[, c("x", "y") := wrf_project(lon, lat)]

  mean_qv <- update[, .(qv = mean(qv)), by = .(exp, date, bottom_top)]

  if (d == 1) {
    global_qv <- mean_qv
    # espacial_qv <- update
  } else {
    global_qv <- rbind(global_qv, mean_qv)
    # espacial_qv$qv <- espacial_qv$qv + update$qv
  }


}


rbind(global_qv_E6, global_qv_E7) %>%
  .[, nivel := fcase(bottom_top %in% c(1:8), "1 to 8",
                     bottom_top %in% c(9:13), "9 to 13",
                     bottom_top %in% c(14:20), "14 to 20",
                     bottom_top %in% c(21:38), "21 to 38")] %>%
  .[, acum := cumsum(qv), by = .(bottom_top, exp)] %>%
  # .[bottom_top %in% c(1:18)] %>%
  ggplot(aes(date, acum*1000)) +
  geom_line(aes(color = bottom_top, linetype = exp, group = interaction(bottom_top, exp))) +
  scale_date() +
  scale_linetype_discrete(labels = c(E4 = "CONV", E5 = "AUT",
                                     E6 = "SATWND", E7 = "RAD")) +
  scale_color_viridis_c() +
  facet_wrap(~nivel, scales = "free_y") +
  labs(x = NULL, y = "Accumulated specific humidity (g/Kg)",
       color = "Sigma level", linetype = NULL) +
  theme_minimal()
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H")

espacial_qv %>%
  .[bottom_top %in% c(1:10)] %>%
  ggplot(aes(x, y)) +
  geom_contour_fill(aes(z = qv*1000), proj = norargentina_lambert) +
  scale_fill_divergent() +
  geom_mapa() +
  facet_wrap(~bottom_top, ncol = 5) +
  theme_minimal()

