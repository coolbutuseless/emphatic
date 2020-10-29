

# https://nsidc.org/data/g02135
# FTP: ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/data/


suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})


csvs <- list.files(here::here("data-raw", "southern-sea-ice"), pattern = "*.csv", full.names = TRUE)

extents <- lapply(csvs, read.csv)
extents <- as_tibble(dplyr::bind_rows(extents))

sea_ice_area <- extents %>%
  select(year, mo, area) %>%
  mutate(area = if_else(area < 0, NA_real_, area)) %>%
  spread(mo, area) %>%
  as.matrix()

rownames(sea_ice_area) <- sea_ice_area[,1]
sea_ice_area <- sea_ice_area[,-1]
colnames(sea_ice_area) <- month.abb


usethis::use_data(sea_ice_area, overwrite = TRUE)

