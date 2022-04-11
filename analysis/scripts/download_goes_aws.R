library(mesoda)
library(metR)
library(tidyverse)
library(lubridate)
library(data.table)

year <- 2018
day <- 312
hour <- 00
keystart <- paste0("ABI-L1b-RadF/", year, "/", day, "/", hour, "/*")

goes16 <- aws.s3::get_bucket_df(bucket = 'noaa-goes16',
                                marker = keystart,
                                max = 20000)

goes16 <- goes16 %>%
  setDT() %>%
  .[, start := lubridate::parse_date_time(str_extract(str_extract(Key, "s\\d{14}"), "\\d{13}"), "%Y%j%H%%M%S") ] %>%
  .[, channel := as.numeric(str_extract(str_extract(Key, "C\\d{2}"), "\\d{2}"))] %>%
  .[minute(start) == 0 & channel %in% c(7:16) & day(start) < 20] %>%
  .[]


for (i in 1:nrow(goes16)) {

  key <- goes16$Key[i]
  file <- paste0("/home/paola.corrales/datosmunin3/DATA/GOES16/", key)
  dir <- dirname(file)

  dir.create(dir, recursive = TRUE)

  print(paste("Downloading file", i, "from", nrow(goes16)))

  aws.s3::save_object(key, bucket = 'noaa-goes16', file = file)

}
