files <- list.files(derived_data, recursive = TRUE, full.names = TRUE,
                    pattern = "perfiles_ana")

perfiles <- purrr::map(files, function(f) {
  exp <- unglue(basename(f), "perfiles_{run}_{exp}.csv")
  fread(f) %>%
    .[, exp := exp[[1]][["exp"]]]

}) %>%
  rbindlist() %>%
  .[, date := as_datetime(date)] %>%
  .[date %between% c(as_datetime("2018-11-20 18:00:00"),
                     as_datetime("2018-11-23 12:00:00"))]


perfiles %>%
  dcast(lev + date ~ exp, value.var = "T") %>%
  .[, ":="(E5_E2 = E5 - E2,
           E6_E5 = E6 - E5,
           E8_E6 = E8 - E6)] %>%
  melt(measure.vars = c("E5_E2", "E6_E5", "E8_E6")) %>%
  .[variable == "E6_E5"] %>%
  ggplot(aes(date, lev)) +
  geom_contour_fill(aes(z = value, fill = stat(level)),
                    breaks = c(seq(-0.2, 0.2, 0.02), Inf)) +
  geom_contour2(aes(z = value), color = "white", size = 0.1,
                breaks = c(seq(-0.2, 0.2, 0.02), Inf)) +
  scale_fill_divergent_discretised(guide = guide_colorsteps(barwidth = 25,
                                                            barheight = 0.3,
                                                            title.position = "left",
                                                            title.vjust = 1,
                                                            frame.colour = "black")) +
  labs(fill = "Temperature (K)",
       x = NULL,
       y = NULL) +
  scale_y_level(breaks = c(1000, 850, 750, 500, 300, 200, 100)) +
  scale_date(ini = 20181121000000, break_bin = "12 hours") +
  facet_wrap(~variable, ncol = 3,
             labeller = labeller(variable = c("E5_E2" = "AUT - CONV",
                                              "E6_E5" = "SATWND - AUT",
                                              "E8_E6" = "RAD - SATWND"))) +
  tag_facets(position = list(x = 0.1, y = 0.96)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        tagger.panel.tag.text = element_text(size = 8),
        panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3)) +


  perfiles %>%
  dcast(lev + date ~ exp, value.var = "QVAPOR") %>%
  .[, ":="(E5_E2 = E5 - E2,
           E6_E5 = E6 - E5,
           E8_E6 = E8 - E6)] %>%
  melt(measure.vars = c("E5_E2", "E6_E5", "E8_E6")) %>%
  .[variable == "E6_E5"] %>%
  ggplot(aes(date, lev)) +
  geom_contour_fill(aes(z = value*1000, fill = stat(level)),
                    breaks = c(seq(-0.6, 0.6, 0.05), Inf)) +
  geom_contour2(aes(z = value), color = "white", size = 0.1,
                breaks = c(seq(-0.6, 0.6, 0.05), Inf)) +
  scale_fill_divergent_discretised(
                                   guide = guide_colorsteps(barwidth = 25,
                                                            barheight = 0.3,
                                                            title.position = "left",
                                                            title.vjust = 1,
                                                            frame.colour = "black")) +

  scale_y_level(breaks = c(1000, 850, 750, 500, 300, 200, 100)) +
  scale_date(ini = 20181121000000, break_bin = "12 hours") +
  labs(fill = latex2exp::TeX("Specific \nhumidity ($gKg^{-1}$)"),
       y = NULL,
       x = NULL) +
  facet_wrap(~variable, ncol = 3,
             labeller = labeller(variable = c("E5_E2" = "AUT - CONV",
                                              "E6_E5" = "SATWND - AUT",
                                              "E8_E6" = "RAD - SATWND"))) +
  tag_facets(tag_pool = c("d", "e", "f"), position = list(x = 0.1, y = 0.96)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        tagger.panel.tag.text = element_text(size = 8),
        panel.ontop = TRUE,
        panel.grid = element_line(linetype = 3)) +

  plot_layout(ncol = 1, heights = c(1, 1))
