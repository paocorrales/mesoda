library(here)

derived_data <- "/home/paola.corrales/datosmunin3/EXP/derived_data/"

dates <- c("20181122100000", "20181122130000",
           "20181122160000", "20181122190000")

derived_data_paper <- "/home/paola.corrales/datosmunin3/EXP/paper_ana_20181122-derived_data/"

dir.create(derived_data_paper)

# sample_obs
dir.create(paste0(derived_data_paper, "sample_obs"))
file.copy(
  c(here("analysis", "data", "raw_data", "E2_asim_conv_20181121120000.ensmean")
    ,here("analysis", "data", "raw_data", "E5_asim_conv_20181121120000.ensmean")
    ,here("analysis/data/derived_data/lista_sondeos.csv")
    ,here("analysis", "data", "derived_data", "dominio_hgt.csv")),
  paste0(derived_data_paper, "sample_obs/"))

# tables
dir.create(paste0(derived_data_paper, "tables"))
file.copy(
  c(here("analysis/data/derived_data/table1.csv")
    ,here("analysis", "data", "derived_data", "tabla_radianzas.csv")),
  paste0(derived_data_paper, "tables/"))

# omb_diagfiles (subfolders for each exp)
dir.create(paste0(derived_data_paper, "omb_diafiles"))
dir.create(paste0(derived_data_paper, "omb_diafiles/E2"))
dir.create(paste0(derived_data_paper, "omb_diafiles/E5"))
dir.create(paste0(derived_data_paper, "omb_diafiles/E6"))
dir.create(paste0(derived_data_paper, "omb_diafiles/E9"))

purrr::map(Sys.glob("/home/paola.corrales/datosmunin3/EXP/E2/ANA/*/diagfiles/asim*ensmean"), function(f) {
  file.copy(f, paste0(derived_data_paper, "omb_diagfiles/E2/"))
})
purrr::map(Sys.glob("/home/paola.corrales/datosmunin3/EXP/E5/ANA/*/diagfiles/asim*ensmean")[1:67], function(f) {
  file.copy(f, paste0(derived_data_paper, "omb_diagfiles/E5/"))
})
purrr::map(Sys.glob("/home/paola.corrales/datosmunin3/EXP/E6/ANA/*/diagfiles/asim*ensmean")[1:67], function(f) {
  file.copy(f, paste0(derived_data_paper, "omb_diagfiles/E6/"))
})
purrr::map(Sys.glob("/home/paola.corrales/datosmunin3/EXP/E9/ANA/*/diagfiles/asim*ensmean"), function(f) {
  file.copy(f, paste0(derived_data_paper, "omb_diagfiles/E9/"))
})


# RCRV
dir.create(paste0(derived_data_paper, "RCRV"))
file.copy(
  c(here("analysis", "data", "derived_data", "rcrv_V_box.csv")
    ,here("analysis", "data", "derived_data", "rcrv_t_box.csv")
    ,here("analysis", "data", "derived_data", "rcrv_V_perfil.csv")
    ,here("analysis", "data", "derived_data", "rcrv_t_perfil.csv")
    ,here("analysis", "data", "derived_data", "rcrv_satwind.csv")
    ,Sys.glob(here("analysis/data/derived_data/rcrv_*_perfil.csv"))[1:6]),
  paste0(derived_data_paper, "RCRV/"))

# observations
dir.create(paste0(derived_data_paper, "observations"))
file.copy(
  c(Sys.glob("/home/jruiz/datosmunin3/datos/DATOS_RADAR/MOSAICO/*.nc")[seq(1, 12, 3)]
    ,here("analysis/data/derived_data/IMERG_1h.rds")),
  paste0(derived_data_paper, "observations/"))

# FSS
dir.create(paste0(derived_data_paper, "FSS"))
file.copy(
c(Sys.glob(here("analysis/data/derived_data/fss_6h_fcst_ens_*_E[2,5,6,9]_ens.csv"))
,Sys.glob(here("analysis/data/derived_data/fss_6h_ana_ens_E[2569].csv"))),
paste0(derived_data_paper, "FSS/"))

# analysis_variables
dir.create(paste0(derived_data_paper, "analysis_variables"))
file.copy(
c(Sys.glob(paste0(derived_data, "perfiles_ana_E[2,5,6,9].csv"))
,Sys.glob(paste0(derived_data, "pw/pw_ana_E[2,5,6,9]_20181122000000.nc"))
,Sys.glob(paste0(derived_data, "mucape/mcape_ana_E[2,5,6,9]_20181122000000.nc"))
,Sys.glob(paste0(derived_data, "*_20181122000000_E[2569].nc"))
,Sys.glob(paste0(derived_data, "ppacum/E[2569]_ana*_pp_pmm.rds"))
,Sys.glob(paste0(derived_data, "dbz/E[29]_ana_", dates, "_dbz_pmm.rds"))),
paste0(derived_data_paper, "analysis_variables"))

# soundings
dir.create(paste0(derived_data_paper), "soundings")
file.copy(
c(Sys.glob(paste0(derived_data, "sondeos/ANA/sondeo_E[2569]_ana*"))),
paste0(derived_data_paper, "soundings/"))

bytes <- sum(file.size(files))
