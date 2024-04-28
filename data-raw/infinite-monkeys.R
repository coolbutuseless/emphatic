

library(dplyr)
library(ggplot2)
library(emphatic)

text <- "mtcars %>%
  hl('red', rows = 1:5)"


monkey <- function(text) {
  chars <- strsplit(text, '')[[1]]

  delay <- min(0.04, 1/nchar(chars))
  pauses <- runif(1000, 0.005, delay)

  Sys.sleep(0.1)

  cat("\014")
  cat("\n>  ")

  for (i in seq_along(chars)) {
    cat("\010");
    cat(chars[i])
    if (i < length(chars) && chars[i+1] != '\n') cat("_")
    Sys.sleep(pauses[i])
  }
  Sys.sleep(0.1)

  cat("\010\010")
  cat("\n\n")

  print(eval(parse(text = text)))

  Sys.sleep(1.1)
}



codes <- c(
  "mtcars",
  "mtcars %>% hl('grey80')",
  "mtcars %>% hl(rainbow(32))",
  "mtcars %>% hl(c('blue', 'white'))",
  "mtcars %>% hl(c('hotpink', 'yellow'), rows = 10:15)",
  "mtcars %>% hl('skyblue', rows = seq(1, 32, 2))",
  "mtcars %>% hl('limegreen', rows = cyl == 6)",
  "mtcars %>% hl(scale_colour_viridis_c(), cols = mpg)",
  "mtcars %>% hl(scale_colour_viridis_c(), rows = cyl != 4, cols = mpg)",
  "mtcars %>% hl(scale_colour_viridis_c(), cols = mpg, scale_apply = mpg:qsec)",
  "mtcars %>% hl(scale_colour_viridis_c(), cols = mpg) %>%\n   hl(scale_colour_distiller(), cols = wt)"
)

system.time({
cat("\014")
for (code in codes) {
  monkey(code)
}
})


# ffmpeg -y -r 8 -i examples.mp4 examples-slow.mp4


# ffmpeg -y -i examples-slow.mp4 -filter_complex "fps=8,scale=640:-1:flags=lanczos[x];[x]split[x1][x2]; [x1]palettegen[p];[x2][p]paletteuse" examples.gif












