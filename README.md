
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

#### `{emphatic}` adds programmatic highlighting to data.frames, matrices and other R output using ANSI colours in the R terminal.

<!-- <img src="man/figures/examples.gif"/> -->

See the [online
documentation](https://coolbutuseless.github.io/package/emphatic/index.html)
for vignettes and more examples.

### What’s in the box

The `hl_` prefix can be read as `highlight`.

- `hl()` for programmatically highlighting data.frames
- `hl_mat()` for programmatically highlighting matrices
- `hl_diff()` for highlighting differences between two objects
- `hl_grep()` highlight regular expression matches in an object or
  string

### Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/emphatic', ref = 'main')
```

### Vignettes

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

### Test cases on real data

- [Space Shuttle O-ring dataset - Challenger
  Disaster](https://coolbutuseless.github.io/package/emphatic/articles/challenger.html)
- [Southern Sea Ice
  Area](https://coolbutuseless.github.io/package/emphatic/articles/example-sea-ice.html)
- [`volcano`
  dataset](https://coolbutuseless.github.io/package/emphatic/articles/example-volcano.html)
- [Correlation
  matrix](https://coolbutuseless.github.io/package/emphatic/articles/example-correlation.html)

## `hl()` - programatically highlight data.frames

- specify rows and columns you want to highlight
- specify a colour
  - a single colour
  - a vector of colours
  - a `ggplot2` “Scale” object e.g. `scale_colour_continuous()`

#### `hl()` simple example of highlighting a data.frame

By default, colouring will be applied to all rows and columns.

``` r
mtcars |>
  head(15) |>
  hl(c('red', 'white', 'blue')) 
```

<img src="man/figures/example1.svg" width="100%">

#### `hl()` complex example of highlighting a data.frame

A more complex example showing how to highlight the `mtcars` dataset
where:

- colour each row to indicate the miles-per-gallon rating
- do not colour the `gear` or `carb` columns
- highlight the car with the minimum horsepower (`hp`) in `hotpink`

Note also that `hl()` calls are cumulative, and you can build up the
highlighting you need step by step

``` r
mtcars |>
  head(15) %>%
  hl(
    colour      = ggplot2::scale_colour_viridis_c(),
    cols        = mpg,      # Where the colour scale is calculated
    dest_cols   = mpg:disp, # Where the colour scale is applied
    show_legend = TRUE
  ) |>
  hl('hotpink', rows = hp == min(hp), cols = hp:carb) 
```

<img src="man/figures/example2.svg" width="100%">

## `hl_mat()` highlight cells of a matrix

The following highlights a correlation matrix of some of the variables
in `mtcars`.

- Use `ggplot2::scale_colour_gradient2()` with its default colours to
  have `red` for negative correlations and `blue` for positive
  correlations
- Cells are only coloured if they satisfy
  - The value is above 0.7
  - The value is not long the diagonal.

Note:

- The `selection` variable uses `.x` as the placeholder representing the
  matrix input

``` r
mtcars |>
  select(cyl, mpg, hp, disp, vs) |>
  cor() |>
  hl_mat(scale_colour_gradient2(), selection = abs(.x) > 0.7 & row(.x) != col(.x)) 
```

<img src="man/figures/example4.svg" width="100%">

## `hl_diff()` highlight difference between two objects

Default colouring:

- `green` for addition
- `blue` for substitution
- `red` for deletion

``` r
x <- "Paris in the the spring?"
y <- "Not Paris in the spring!"
hl_diff(x, y)
```

<img src="man/figures/example-strdiff-3.svg" width="100%">

``` r
x <- c('apple', 'horse', 'battery', 'stapler')
y <- c('apple', 'horse', 'butter' , 'stable' , "widget")
hl_diff(x, y, coerce = 'deparse')
```

<pre><span><span>c("apple", "horse", "b</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>a</span></span><span><span>tter</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'>y</span></span><span><span>", "sta</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>p</span></span><span><span>le</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>r</span></span><span style='color:#000000;'><span style='background-color:#006400;'>         </span></span><span><span>")</span></span><br/><span><span>c("apple", "horse", "b</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>u</span></span><span><span>tter</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'> </span></span><span><span>", "sta</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>b</span></span><span><span>le</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>"</span></span><span style='color:#000000;'><span style='background-color:#006400;'>, "widget</span></span><span><span>")</span></span></pre>

## `hl_grep()` highlight regular expression matches in objects

#### Highlight regular expression matches in a character string

``` r
gettysburg <- c("Four score and seven years ago our fathers brought forth on",
"this continent, a new nation, conceived in Liberty, and dedicated to the", 
"proposition that all men are created equal.")


hl_grep(gettysburg, "men.*equal")
```

<img src="man/figures/grep-char1.svg" />

#### Highlighting regular expression matches within R objects

`hl_grep()` can coerce R objects to character and then to regular
expression matching on that output.

- `coerce = "default"` - use the output from `print()`
- `coerce = "character"` - use the output from `as.character()`
- `coerce = "print"` - use the output from `print()`
- `coerce = "deparse"` - use the output from `deparse1()`
- `coerce = "str"` - use the output from `str()`

``` r
ll <- as.list(setNames(sample(7), LETTERS[1:7]))
hl_grep(ll, "e", ignore.case = TRUE)
```

<img src="man/figures/grep-vec-deparse.svg" />

#### Highlighting regular expression matches within a numeric vector

``` r
values <- runif(20)
hl_grep(values, "123.*?4", coerce = 'deparse')
```

<img src="man/figures/grep-num-deparse.svg" />

## Options

- `hl_opts()` create a named list of default options accepted by the
  functions in this package
- `hl_adjust()` to adjust options after creation.
- Set the following options to control global behaviour within a
  session.
  - `HL_NA`
  - `HL_DARK`
  - `HL_FULL_COLOUR`
  - `HL_TEXT_MODE`
  - `HL_TEXT_CONTRAST`
  - `HL_UNDERLINE_HEADER`
- The above R options are initialised using `Sys.getenv()` during
  package start, and otherwise use a default value. Set these values as
  environment variables in your `.Rprofile` to save your preferred
  settings across different sessions. e.g.
  - `Sys.setenv(HL_DARK = FALSE)` prior to loading package
  - `options(HL_DARK = FALSE)` at any time
