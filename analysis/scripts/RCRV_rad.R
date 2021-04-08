library(mesoda)
library(data.table)

sensor <- "airs"

path <- paste0("/home/paola.corrales/datosmunin/EXP/E8/ANA/*/diagfiles/asim_", sensor, "*mem*")

files <- Sys.glob(path)


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

perfiles <- read_diag_rad(files, exp = "E8") %>%
  satinfo[., on = c("sensor", "channel")] %>%
  .[iuse == 1]

RCRV_level <- perfiles[, ":="(level = cut_round(peakwt, breaks = c(seq(0, 500, 100), seq(550, 1050, 50))))] %>%
  .[qc == 0 & errinv %between% c(0.0000316227, 1000)] %>%
  .[, ":="(mean.guess = mean(tb_obs - tbc, na.rm = TRUE),
           sd.guess = sd(tb_obs - tbc, na.rm = TRUE)), by = .(id, date)] %>%
  .[, y := (tb_obs - mean.guess)/sqrt(sd.guess^2 + 1/errinv^2), by = .(id, date)] %>%
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

RCRV_level2 <- perfiles[, ":="(level = cut_round(peakwt, breaks = c(seq(0, 500, 100), seq(550, 1050, 50))))] %>%
  .[qc == 0 & errinv %between% c(0.0000316227, 1000)] %>%
  .[, ":="(mean.guess = mean(tb_obs - tbc, na.rm = TRUE),
           sd.guess = sd(tb_obs - tbc, na.rm = TRUE)), by = .(id, date)] %>%
  .[, y := (tb_obs - mean.guess)/sqrt(sd.guess^2 + 1/errinv^2), by = .(id, date)] %>%
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


files <- Sys.glob("analysis/data/derived_data/rcrv_*_perfil.csv")[1:4]

RCRV <- purrr::map(files, function(f) {
  fread(f)
}) %>%
  rbindlist()

RCRV %>%
  separate(sensor, into = c("sensor", "plat")) %>%
  setDT() %>%
  melt(measure.vars = c("mean.y", "sd.y")) %>%
  .[, .(value = mean(value, na.rm = TRUE)), by = .(sensor, level, variable)] %>%
  ggplot(aes(level, value)) +
  geom_line(aes(color = sensor, linetype = variable)) +
  coord_flip() +
  scale_x_level() +
  labs(x = NULL, color = NULL) +
  theme_minimal()
