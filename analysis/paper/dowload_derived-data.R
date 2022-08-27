download <- function(url, file, zipped) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  message("Downloading ", file, "...")
  utils::download.file(url, file)

  if (zipped) {
    message("Unzpping files...")
    utils::untar(file, exdir = dirname(file))
    unlink(file)
  }

  return(0)
}

datasets <- list(
  topo = list(
    url = "https://zenodo.org/record/7015913/files/26c009318f5b17bc6a5dc6648f6cfb2b2c11a5aa.txt?download=1",
    file = here::here("analysis/data/derived_data/26c009318f5b17bc6a5dc6648f6cfb2b2c11a5aa.txt"),
    zipped = FALSE
  ),
  analysis_variables = list(
    url = "https://zenodo.org/record/7015913/files/analysis_variables.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/analysis_variables.tar.gz"),
    zipped = TRUE
  ),
  FSS = list(
    url = "https://zenodo.org/record/7015913/files/FSS.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/FSS.tar.gz"),
    zipped = TRUE
  ),
  observations = list(
    url = "https://zenodo.org/record/7015913/files/observations.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/observations.tar.gz"),
    zipped = TRUE
  ),
  omb_diagfiles = list(
    url = "https://zenodo.org/record/7015913/files/omb_diagfiles.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/omb_diagfiles.tar.gz"),
    zipped = TRUE
  ),
  RCRV = list(
    url = "https://zenodo.org/record/7015913/files/RCRV.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/RCRV.tar.gz"),
    zipped = TRUE
  ),
  reanalysis = list(
    url = "https://zenodo.org/record/7015913/files/reanalysis.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/reanalysis.tar.gz"),
    zipped = TRUE
  ),
  sample_obs = list(
    url = "https://zenodo.org/record/7015913/files/sample_obs.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/sample_obs.tar.gz"),
    zipped = TRUE
  ),
  soundings = list(
    url = "https://zenodo.org/record/7015913/files/soundings.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/soundings.tar.gz"),
    zipped = TRUE
  ),
  tables = list(
    url = "https://zenodo.org/record/7015913/files/tables.tar.gz?download=1",
    file = here::here("analysis/data/derived_data/tables.tar.gz"),
    zipped = TRUE
  )
)
options(timeout = 60*3)

lapply(datasets, function(dataset) {
  download(dataset$url, dataset$file, dataset$zipped)
})
