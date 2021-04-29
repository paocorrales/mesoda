library(tidyverse)
library(data.table)
library(mesoda)
library(metR)

source(here::here("temp_R", "help_functions.R"))
path <- "/home/paola.corrales/datosmunin/EXP/E6/ANA/*/diagfiles/asim_conv_2018112*"

perfiles <- read_diag(path, variable = c("uv")) %>%
  .[, c("error", "nivel.error") := input_obs_error(var, type, pressure, path_to_errtable = "analysis/data/raw_data/errtable.csv")]

perfiles <- perfiles[type %between% c(240, 260)] %>%
  .[, var := "v"] %>%
  setnames("nivel.error", "error.level")

RCRV <- get_RCRV(perfiles[type %between% c(240, 260)], tipo.var = c("perfil"), calculo = "total") %>%
  .[, sat_type := case_when(
    type == 240 ~ "GOES SW winds",
    type == 241 ~ "India",
    type == 242 ~ "JMA Visible",
    type == 243 ~ "EUMETSAT visible",
    type == 244 ~ "AVHRR winds",
    type == 245 ~ "GOES IR",
    type == 246 ~ "GOES WV cloud top",
    type == 247 ~ "GOES WV deep layer",
    type == 248 ~ "GOES cloud top (sounder)",
    type == 249 ~ "GOES deep layer (sounder)",
    type == 250 ~ "JMA WV deep layer",
    type == 251 ~ "GOES visible",
    type == 252 ~ "JMA IR winds",
    type == 253 ~ "EUMETSAT IR winds",
    type == 254 ~ "EUMETSAT WV deep layer winds",
    type == 257 ~ "MODIS IR",
    type == 258 ~ "MODIS WV cloud top",
    type == 259 ~ "MODIS WV deep layer winds",
    type == 260 ~ "VIIR IR winds")]

write_csv(na.omit(RCRV), here::here("analysis", "data", "derived_data", "rcrv_satwind.csv"))

RCRV %>%
  melt(measure.vars = c("mean.y", "sd.y")) %>%
  ggplot(aes(error.level, value)) +
  geom_hline(yintercept = c(0, 1), color = "darkgrey") +
  geom_line(aes(color = factor(sat_type), linetype = variable)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_level() +
  coord_flip() +
  # facet_wrap(~ var, scales = "free_x") +
  labs(title = "Reduced Centered Random Variable",
       subtitle = "Satellite wind",
       linetype = "",
       color = "Obs type") +
  theme_minimal()
