

suppressPackageStartupMessages({
  library(dplyr)
})


challenger <- tibble::tribble(
 ~flight,  ~temp, ~erosion, ~blowby, ~damage,      ~date,
     "1",    66L,       0L,      0L,      0L,  "4/12/81",
     "2",    70L,       1L,      0L,      4L, "11/12/81",
     "3",    69L,       0L,      0L,      0L,  "3/22/82",
     "5",    68L,       0L,      0L,      0L, "11/11/82",
     "6",    67L,       0L,      0L,      0L,   "4/4/83",
     "7",    72L,       0L,      0L,      0L,  "6/18/83",
     "8",    73L,       0L,      0L,      0L,  "8/30/83",
     "9",    70L,       0L,      0L,      0L, "11/28/83",
  "41-B",    57L,       1L,      0L,      4L,   "2/3/84",
  "41-C",    63L,       1L,      0L,      2L,   "4/6/84",
  "41-D",    70L,       1L,      0L,      4L,  "8/30/84",
  "41-G",    78L,       0L,      0L,      0L,  "10/5/84",
  "51-A",    67L,       0L,      0L,      0L,  "11/8/84",
  "51-C",    53L,       3L,      2L,     11L,  "1/24/85",
  "51-D",    67L,       0L,      0L,      0L,  "4/12/85",
  "51-B",    75L,       0L,      0L,      0L,  "4/29/85",
  "51-G",    70L,       0L,      0L,      0L,  "6/17/85",
  "51-F",    81L,       0L,      0L,      0L,  "7/29/85",
  "51-I",    76L,       0L,      0L,      0L,  "8/27/85",
  "51-J",    79L,       0L,      0L,      0L,  "10/3/85",
  "61-A",    75L,       0L,      2L,      4L, "10/30/85",
  "61-B",    76L,       0L,      0L,      0L, "11/26/85",
  "61-C",    58L,       1L,      0L,      4L,  "1/12/86"
)


challenger <- challenger %>%
  mutate(
    date = lubridate::mdy(date)
  )


challenger <- as.data.frame(challenger)


usethis::use_data(challenger, overwrite = TRUE)
