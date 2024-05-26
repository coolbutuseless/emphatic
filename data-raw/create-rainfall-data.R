
library(dplyr)


# Monthly rainfall. Centennial park, Sydney
# source www.bom.gov.au
rain_raw <- readr::read_csv("data-raw/IDCJAC0001_066160/IDCJAC0001_066160_Data12.csv", na = 'null')



sydney_rain <- rain_raw %>%
  select(-(1:2)) %>%
  select(-Annual) %>%
  filter(Year != 2024) %>%
  filter(Year >= 1960)


usethis::use_data(sydney_rain, overwrite = TRUE)


mean_rain <- sydney_rain %>%
  select(Jan:Dec) |>
  unlist() |>
  mean(na.rm = TRUE)


# https://en.wikipedia.org/wiki/Floods_in_Australia
# In sydney
flood_years <- c(1970, 1971, 1984, 1986, 1990, 1996, 1998, 2007, 2012, 2013, 2015, 2016, 2020, 2021, 2022)

sydney_rain |>
  as.data.frame() %>%
  hl(ggplot2::scale_colour_gradient2(na.value = NA, low = 'white', high = 'darkgreen', midpoint = mean_rain), cols = Jan:Dec) |>
  hl('skyblue', rows = Year %in% flood_years, cols = 'Year') |>
  hl_adjust(na = '') |>
  as_svg(browsable = TRUE, height = 1200)
