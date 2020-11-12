
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
![](https://img.shields.io/badge/dependencies-zero-blue.svg)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/coolbutuseless/emphatic/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/emphatic/actions)
<!-- badges: end -->

#### `{emphatic}` augments the output of data.frames and matrices in R by adding user-defined ANSI highlighting.

<img src="man/figures/examples.gif"/>

See the [online
documentation](https://coolbutuseless.github.io/package/emphatic/index.html)
for vignettes and more examples.

## What’s in the box

There are separate high-level functions for highlighting data.frames and
matrices, and a low-level function which can be used on both.

The `hl_` prefix can be read as `highlight`.

-   `hl()` for highlighting data.frames
-   `hl_mat()` for highlighting matrices
-   `hl_vec()` for highlighting atomic vectors
-   `hl_loc()` for low-level control of highlighting of both data.frames
    and matrices
-   `hl_opt()` to set some local options on the current `emphatic`
    object e.g. `full_colour` option sets 24-bit colour mode.  
-   `hl_opt_global()` sets global options for highlighting. These values
    will be the default unless overridden with a call to `hl_opt()` for
    the given `emphatic` object.

|            | data.frame | matrix     | vector     |
|------------|------------|------------|------------|
| High Level | `hl()`     | `hl_mat()` | `hl_vec()` |
| Low Level  | `hl_loc()` | `hl_loc()` | `NA`       |

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/emphatic')
```

## Warning

-   This package calls `eval()` on user-supplied code and extreme
    caution should be taken before exposing functions in this package to
    the internet (e.g. via `shiny`)

## Vignettes

Intro

-   [Guided
    Example](https://coolbutuseless.github.io/package/emphatic/articles/intro.html)

Speicifying rows, columns and colours

-   [Specifying
    rows](https://coolbutuseless.github.io/package/emphatic/articles/row-specification.html)
-   [Specifying
    columns](https://coolbutuseless.github.io/package/emphatic/articles/col-specification.html)
-   [Specifying
    colours](https://coolbutuseless.github.io/package/emphatic/articles/colour-specification.html)

Test cases on Real data

-   [Space Shuttle O-ring dataset - Challenger
    Disaster](https://coolbutuseless.github.io/package/emphatic/articles/challenger.html)
-   [Southern Sea Ice
    Area](https://coolbutuseless.github.io/package/emphatic/articles/sea-ice.html)
-   [`volcano`
    dataset](https://coolbutuseless.github.io/package/emphatic/articles/volcano.html)
-   [Correlation
    matrix](https://coolbutuseless.github.io/package/emphatic/articles/correlation.html)

Advanced:

-   [Low level highlighting with
    `hl_loc()`](https://coolbutuseless.github.io/package/emphatic/articles/low-level-hl-loc.html)

## Example: Highlighting a data.frame with alternative row colours

``` r
library(emphatic)
emphatic::hl_opt_global(dark_mode = FALSE)

mtcars %>%
  hl(c('red', 'white')) 
```

<img src="man/figures/example1.svg" width="100%">

## Example of highlighting specific rows and columns

Use `{emphatic}` to highlight the `mtcars` dataset where:

-   6 and 8 cylinder cars only
-   colour each row to indicate the miles-per-gallon rating
-   do not colour the `gear` or `carb` columns
-   highlight the car with the maximum miles per gallon in `hotpink`
-   de-emphasise the numeric values to focus on the colour highlighting

``` r
mtcars %>%
  hl(ggplot2::scale_colour_viridis_c(), rows = cyl %in% c(6, 8), 
     cols = mpg, dest_cols = mpg:am) %>%
  hl('hotpink', rows = mpg == max(mpg)) %>%
  hl_opt(text_contrast = 0.25)
```

<img src="man/figures/example2.svg" width="100%">

## Example: Highlighting a data.frame with rainbows!

``` r
mtcars %>% 
  hl(rainbow(32)) %>%
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
mtcars %>%
  select(cyl, mpg, hp, disp, vs) %>%
  cor() %>%
  hl_mat(scale_colour_gradient2(), selection = abs(.x) > 0.7 & row(.x) != col(.x)) 
```

<img src="man/figures/example4.svg" width="100%">

## Example: Highlighting a numeric vector (1)

Highlight locations in a numeric vector which match an expression.

``` r
sample(10, 30, replace = TRUE, prob = 1:10) %>%
  hl_vec('green', .x < 3) %>%
  hl_vec('blue', .x > 7)
```

<img src="man/figures/example5.svg" width="100%">

## Example: Highlighting a numeric vector (2)

Colour a numeric vector using a ggplot colour scale.

``` r
sample(10, 30, replace = TRUE, prob = 1:10) %>%
  sort() %>%
  hl_vec(scale_colour_viridis_c(option = 'A'))
```

<img src="man/figures/example6.svg" width="100%">

## Example: Highlighting a character vector (3)

Highlight elements of a vector when they are identical to the previous
element.

``` r
sample(letters[1:5], 30, replace = TRUE, prob = 1:5) %>%
  hl_vec('red', .x == lag(.x))
```

<img src="man/figures/example7.svg" width="100%">

## Related Software

-   [crayon](https://cran.r-project.org/package=crayon) Colored terminal
    output on terminals that support ‘ANSI’ color and highlight codes.
    It also works in ‘Emacs’ ‘ESS’. ‘ANSI’ color support is
    automatically detected.
-   [fansi](https://cran.r-project.org/package=fansi) Counterparts to R
    string manipulation functions that account for the effects of ANSI
    text formatting control sequences.

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
