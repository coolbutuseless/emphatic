
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
![](https://img.shields.io/badge/dependencies-zero-blue.svg)
[![R-CMD-check](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

#### `{emphatic}` augments the output of data.frames, matrices and simple vectors in R by adding user-defined ANSI highlighting.

<img src="man/figures/examples.gif"/>

See the [online
documentation](https://coolbutuseless.github.io/package/emphatic/index.html)
for vignettes and more examples.

## What’s in the box

There are separate high-level functions for highlighting data.frames,
matrices and simple vectors. There is also a low-level highlghting
function which can be used on data.frames and matrices.

The `hl_` prefix can be read as `highlight`.

- `hl()` for highlighting data.frames
- `hl_mat()` for highlighting matrices
- `hl_vec()` for highlighting simple atomic vectors
- `hl_loc()` for low-level control of highlighting of both data.frames
  and matrices
- `hl_opt()` to set some local options on the current `emphatic` object
  e.g. `full_colour` option sets 24-bit colour mode.  
- `hl_opt_global()` sets global options for highlighting. These values
  will be the default unless overridden with a call to `hl_opt()` for
  the given `emphatic` object.
- `hl_str_diff()` for highlighting string differences.
- General highlighting for regular expressions.
  - `hl_grep(x, pattern)` where `x` is a character string.
  - `hl_grep_character()` where the general object `x` is converted to a
    string using `as.character()`.
  - `hl_grep_print()` as above, but converstion to character via
    `print()`
  - `hl_grep_deparse()` as above, but using `deparse1()` to create
    string representation of object.
  - `hl_grep_str()` as above, but using `str()` to create string
    representation of object.

|            | data.frame | matrix     | vector     |
|------------|------------|------------|------------|
| High Level | `hl()`     | `hl_mat()` | `hl_vec()` |
| Low Level  | `hl_loc()` | `hl_loc()` | `NA`       |

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/emphatic', ref = 'main')
```

## Warning

- This package calls `eval()` on user-supplied code and extreme caution
  should be taken before exposing functions in this package to the
  internet (e.g. via `shiny`)

## Vignettes

Intro

- [Highlighting
  data.frames](https://coolbutuseless.github.io/package/emphatic/articles/aaa-data-frames.html)
- [Highlighting
  matrices](https://coolbutuseless.github.io/package/emphatic/articles/aaa--matrices.html)
- [Highlighting
  vectors](https://coolbutuseless.github.io/package/emphatic/articles/aaa-vectors.html)

Specifying rows, columns and colours

- [Specifying
  rows](https://coolbutuseless.github.io/package/emphatic/articles/specify-rows.html)
- [Specifying
  columns](https://coolbutuseless.github.io/package/emphatic/articles/specify-columns.html)
- [Specifying
  colours](https://coolbutuseless.github.io/package/emphatic/articles/specify-colours.html)

Test cases on Real data

- [Space Shuttle O-ring dataset - Challenger
  Disaster](https://coolbutuseless.github.io/package/emphatic/articles/challenger.html)
- [Southern Sea Ice
  Area](https://coolbutuseless.github.io/package/emphatic/articles/example-sea-ice.html)
- [`volcano`
  dataset](https://coolbutuseless.github.io/package/emphatic/articles/example-volcano.html)
- [Correlation
  matrix](https://coolbutuseless.github.io/package/emphatic/articles/example-correlation.html)

Advanced:

- [Low level highlighting with
  `hl_loc()`](https://coolbutuseless.github.io/package/emphatic/articles/low-level-hl-loc.html)

## Example: Highlighting a data.frame with alternating row colours

``` r
library(emphatic)
emphatic::hl_opt_global(dark_mode = FALSE)

mtcars |>
  hl(c('red', 'white')) 
```

<img src="man/figures/example1.svg" width="100%">

## Example of highlighting a data.frame and include a legend

Use `{emphatic}` to highlight the `mtcars` dataset where:

- colour each row to indicate the miles-per-gallon rating
- do not colour the `gear` or `carb` columns
- highlight the car with the maximum miles per gallon in `hotpink`

``` r
mtcars |>
  hl(ggplot2::scale_colour_viridis_c(),
     cols = mpg, dest_cols = mpg:am, show_legend = TRUE) |>
  hl('hotpink', rows = mpg == max(mpg)) |>
  hl_opt(text_contrast = 0.25)
```

<img src="man/figures/example2.svg" width="100%">

## Example: Highlighting a data.frame with rainbows!

``` r
mtcars |> 
  hl(rainbow(32)) |>
  hl_opt(text_contrast = 0.5)
```

<img src="man/figures/example3.svg" width="100%">

## Example: Highlighting a matrix - Correlation matrix

Create a correlation matrix of some of the variables in `mtcars`.

Colour the values using red for negative correlations and blue for
positive correlations. Values in-between are coloured using a gradient
between red and blue. This colouring is applied using
`ggplot2::scale_colour_gradient2()`.

``` r
mtcars |>
  select(cyl, mpg, hp, disp, vs) |>
  cor() |>
  hl_mat(scale_colour_gradient2(), selection = abs(.x) > 0.7 & row(.x) != col(.x)) 
```

<img src="man/figures/example4.svg" width="100%">

## Example: Highlighting a numeric vector

Highlight locations in a numeric vector which match an expression.

``` r
sample(10, 30, replace = TRUE, prob = 1:10) |>
  hl_vec('green', .x < 3) |>
  hl_vec('blue', .x > 7)
```

<img src="man/figures/example5.svg" width="100%">

## Example: Highlighting the difference between strings

``` r
x <- 'hell there!'
y <- 'hello there?'
hl_str_diff(x, y)
```

<img src="man/figures/example-strdiff-1.svg" width="100%">

``` r
x <- 'hello there?'
y <- 'hell there!'
hl_str_diff(x, y)
```

<img src="man/figures/example-strdiff-2.svg" width="100%">

``` r
x <- "Paris in the the spring?"
y <- "Not Paris in the spring!"
hl_str_diff(x, y)
```

<img src="man/figures/example-strdiff-3.svg" width="100%">

# Highlighting `grep()` matches in character representations of objects

## Example: Highlight a string in a data.frame

``` r
mtcars |> 
  head(20) |>
  hl_grep_print("Merc")
```

<img src="man/figures/grep-df1.svg" />

### Highlight a row in a data.frame which matches a word

``` r
mtcars |> 
  head(20) |>
  hl_grep_print("(?m)^.*wood.*?$", fg = 'blue', bg = 'hotpink')
```

<img src="man/figures/grep-df2.svg" />

### Highlight text in a string

``` r
string <- 
"<xml>
   <this is='not'>a real XML doc</this>
   <this is='not'>a real HTML doc</this>
   <this is='not'>a real XML doc</this>
   <this is='not'>a real XML doc</this>
</xml>"

hl_grep(string, "html", ignore.case = TRUE)
```

<img src="man/figures/grep-char1.svg" />

### Highlighting within a character vector

``` r
vals <- c('hello', 'there', '#rstats', 'on', 'mastodon')

hl_grep(vals, "rstats")
```

<img src="man/figures/grep-vec-deparse.svg" />

### Highlighting within a numeric vector

``` r
hl_grep_character(pi, "589")
```

<img src="man/figures/grep-num-deparse.svg" />

## Related Software

- [crayon](https://cran.r-project.org/package=crayon) Colored terminal
  output on terminals that support ‘ANSI’ color and highlight codes. It
  also works in ‘Emacs’ ‘ESS’. ‘ANSI’ color support is automatically
  detected.
- [fansi](https://cran.r-project.org/package=fansi) Counterparts to R
  string manipulation functions that account for the effects of ANSI
  text formatting control sequences.

## Acknowledgements

- R Core for developing and maintaining the language.
- CRAN maintainers, for patiently shepherding packages onto CRAN and
  maintaining the repository
