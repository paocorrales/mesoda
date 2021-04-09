library(mesoda)
library(data.table)
library(here)
library(tidyverse)
library(metR)

sensor <- "iasi"

path <- paste0("/home/paola.corrales/datosmunin/EXP/E8/ANA/*/diagfiles/asim_", sensor, "*ensmean*")

files <- Sys.glob(path)

dates <- unglue::unglue(basename(files), "asim_iasi_{sensor}_{date}.ensmean")

dates <- unique(purrr::map_chr(dates, "date"))

# perfiles <- purrr::map(files, function(f)
#   meta <- unglue(basename(f), "asim_conv_{date}.mem{mem}")
#   read_diag_conv(files, variable = c("uv", "t"), exp = "E4", member = meta[[1]][["mem"]]) %>%
#   .[, c("error", "error.level") := input_obs_error(var, type, pressure,
#                                                    path_to_errtable = here("analysis", "data", "raw_data", "errtable.csv"))]
#
# }) %>%
#   rbindlist()

satinfo <- fread(here("analysis", "data", "derived_data", "satinfo.txt")) %>%
  setnames(c("!sensor/instr/sat", "chan"), c("sensor", "channel"))

perfiles <- purrr::map(dates, function(d) {

  files_date <- Sys.glob(paste0("/home/paola.corrales/datosmunin/EXP/E8/ANA/", d, "/diagfiles/asim_iasi*_", d, ".mem", formatC(1: 60, width = 3, flag = "0")))

  files_date <- files_date[str_detect(files_date, "metop-a")]
  print(files_date[1])

  out <- read_diag_rad(files_date, exp = "E8") %>%
    satinfo[., on = c("sensor", "channel")] %>%
    .[iuse == 1] %>%
    .[, ":="(level = cut_round(peakwt, breaks = c(seq(0, 500, 100), seq(550, 1050, 50))))] %>%
    .[qc == 0 & errinv %between% c(0.0000316227, 1000)] %>%
    .[, ":="(mean.guess = mean(tb_obs - tbc, na.rm = TRUE),
             sd.guess = sd(tb_obs - tbc, na.rm = TRUE)), by = .(id, date)] %>%
    .[, y := (tb_obs - mean.guess)/sqrt(sd.guess^2 + 1/errinv^2), by = .(id, date)]


  fwrite(out, paste0("iasi_", d, ".csv"))

  d

}) %>% rbindlist()


perfiles <- purrr::map(Sys.glob("iasi*"), ~fread(.x)) %>% rbindlist()


RCRV_level <- perfiles %>%
.[, .(mean.y = mean(y, na.rm = TRUE),
      sd.y = sd(y, na.rm = TRUE)), by = .(sensor, channel, level)]

RCRV_level %>%
  melt(measure.vars = c("mean.y", "sd.y")) %>%
  .[, .(value = mean(value, na.rm = TRUE)), by = .(sensor, level, variable)] %>%
  ggplot(aes(level, value)) +
  geom_line(aes(color = sensor, linetype = variable)) +
  coord_flip() +
  scale_x_level()

fwrite(RCRV_level, paste0("rcrv_", sensor, "_channels.csv"))

RCRV_level2 <- perfiles %>%
  .[, .(mean.y = mean(y, na.rm = TRUE),
        sd.y = sd(y, na.rm = TRUE)), by = .(level)]

RCRV_level2 %>%
  melt(measure.vars = c("mean.y", "sd.y")) %>%
  .[, .(value = mean(value, na.rm = TRUE)), by = .(level, variable)] %>%
  ggplot(aes(level, value)) +
  geom_line(aes(linetype = variable)) +
  coord_flip() +
  scale_x_level()

fwrite(RCRV_level2, paste0("rcrv_", sensor, "_perfil.csv"))


# All sensors -------------------------------------------------------------


files <- Sys.glob("analysis/data/derived_data/rcrv_*_perfil.csv")[1:6]

RCRV <- purrr::map(files, function(f) {
  meta <- unglue::unglue(basename(f), "rcrv_{sensor}_perfil.csv")

  fread(f) %>%
    .[, sensor := meta[[1]][["sensor"]]]
}) %>%
  rbindlist()

RCRV %>%
  # separate(sensor, into = c("sensor", "plat")) %>%
  # setDT() %>%
  melt(measure.vars = c("mean.y", "sd.y")) %>%
  .[, .(value = mean(value, na.rm = TRUE)), by = .(sensor, level, variable)] %>%
  ggplot(aes(level, value)) +
  geom_hline(yintercept = c(0, 1), color = "grey30") +
  geom_line(aes(color = sensor, linetype = variable)) +
  coord_flip() +
  scale_x_level() +
  labs(x = NULL, color = NULL) +
  theme_minimal()
