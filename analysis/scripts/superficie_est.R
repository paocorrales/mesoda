
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

files <- Sys.glob(paste0(derived_data, "/interp_obs/2018112206/interp*new.rds"))

obs <- purrr::map(files, function(f) {
  print(f)
  read_rds(f) %>%
    .[, id := 1:.N, by = ens] %>%
    .[, fcst_exp := "2018112206"] %>%
    .[, mem := formatC(ens, flag = "0", width = 3)] %>%
    .[, exp := NULL] %>%
    melt(measure.vars = c("E2", "E5", "E6", "E8"),
         variable.name = "exp",
         value.name = "fcst") %>%
    .[, ":="(obs = if_else(usage.flag == 1, obs, NA_real_),
             fcst = if_else(usage.flag == 1, fcst, NA_real_))] %>%
    .[usage.flag == 1] %>%
    .[, .(RMSE = sqrt(mean((obs - fcst)^2, na.rm = TRUE)),
          BIAS = mean(obs - fcst, na.rm = TRUE)), by = .(var, exp, mem, fcst_exp, date)]
  # .[, c(list(RMSE = sqrt(mean((obs - fcst)^2, na.rm = TRUE)),
  #            BIAS = mean(obs - fcst, na.rm = TRUE)),
  #       .SD[mem == 1]), by = .(id, exp, mem), .SDcols = -c("obs", "fcst")] %>%


}) %>%
  rbindlist()

write_rds(obs, paste0(derived_data, "/interp_obs/interp_obs_2018112206_mem_new.rds"))



# mean --------------------------------------------------------------------

files <- Sys.glob(paste0(derived_data, "/interp_obs/2018112200/interp*"))

obs <- purrr::map(files, function(f) {
  print(f)
  a <- read_rds(f) %>%
    .[, id := 1:.N, by = ens] %>%
    .[, fcst_exp := "2018112206"] %>%
    .[, mem := formatC(ens, flag = "0", width = 3)] %>%
    .[, exp := NULL] %>%
    melt(measure.vars = c("E2", "E5", "E6", "E8"),
         variable.name = "exp",
         value.name = "fcst") %>%
    .[, ":="(obs = if_else(usage.flag == 1, obs, NA_real_),
             fcst = if_else(usage.flag == 1, fcst, NA_real_))] %>%
    .[usage.flag == 1]

  b <- a[, c("obs", "id", "exp", "var", "date")] %>% unique()

  out <- a[, .(fcst_mean = mean(fcst, na.rm = TRUE)), by = .(id, var, exp, date, fcst_exp)] %>%
    b[., on = .NATURAL] %>%
    .[, .(RMSE = sqrt(mean((obs - fcst_mean)^2, na.rm = TRUE)),
          BIAS = mean(obs - fcst_mean, na.rm = TRUE)), by = .(var, exp, date, fcst_exp)]

}) %>%
  rbindlist()

write_rds(obs, paste0(derived_data, "/interp_obs/interp_obs_2018112206_mean_new.rds"))




