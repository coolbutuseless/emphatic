
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
![](https://img.shields.io/badge/developing-rapidly-orange.svg)
[![R-CMD-check](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{emphatic}` uses ANSI colouring in the terminal to add user-controlled
highlighting to data.frames and other R output.

<!-- <img src="man/figures/examples.gif"/> -->

#### What’s in the box

- `hl()` for user-controlled highlighting of data.frames
- `hl_diff()` for highlighting differences between two objects
- `hl_grep()` highlight regular expression matches in an object or
  string
- Conversion of the “emphatic” object to formats for rendering
  Rmarkdown/Quarto documents, and saving to file:
  - `as_html()`
  - `as_svg()` and animated `as_svg_anim()`
  - `as_typst()`
  - `write_xlsx()` - Excel document

<span style="font-size:smaller">`hl_` prefix can be read as
`highlight`</span>

#### Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.packages('remotes')
remotes::install_github('coolbutuseless/emphatic', ref = 'main')
```

## `hl()` - user-controlled highlighting of data.frames

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
  head(15) |>
  hl(
    palette     = ggplot2::scale_colour_viridis_c(),
    cols        = mpg,      # Where the colour scale is calculated
    scale_apply = mpg:disp, # Where the colour scale is applied
    show_legend = TRUE
  ) |>
  hl('hotpink', rows = hp == min(hp), cols = hp:carb) 
```

<img src="man/figures/example2.svg" width="100%">

## `hl_diff()` highlight difference between two objects

The Levenshtein edit distance is calculated between the string
representation of two objects and these edits are then coloured 🟢 =
insert, 🔴 = delete, 🔵 = substitute.

``` r
x <- "Paris in the the spring?"
y <- "Not Paris in the spring!"
hl_diff(x, y)
```

<img src="man/figures/example-strdiff-3.svg" width="100%">

Levenshtein’s edit distance naturally applies to strings, but
`hl_diff()` can visualise the difference between arbitrary objects by
first converting them to a string representation. Coercion to a string
is controlled by the `coerce` argument, and defaults to the output if
the objects were `print()`ed.

In this example, the difference between the `mean()` and `median()`
function definitions is highlighted.

``` r
hl_diff(mean, median, coerce = 'print', sep = " ")
```

<img src="man/figures/example-strdiff-4.svg" width="100%">

## `hl_grep()` highlight regular expression matches in objects

`hl_grep()` highlights the regular expression matches within a string or
objects coerced into a string representation.

#### Highlight regular expression matches in a character string

``` r
gettysburg <- c(
  "Four score and seven years ago our fathers brought forth on",
  "this continent, a new nation, conceived in Liberty, and dedicated to the", 
  "proposition that all men are created equal."
)
hl_grep(gettysburg, "men.*equal")
```

<img src="man/figures/example-hlgrep-1.svg" width="100%">

#### Highlight regular expression matches within an object

Other R objects (functions, lists, data.frames, etc) can also be
highlighted with regular expressions. How an object is coerced into
string representation is controlled by the `coerce` argument.

In this example, the function body for `mode()` is searched for the word
`switch`:

``` r
hl_grep(mode, 'switch')
```

<img src="man/figures/example-hlgrep-2.svg" width="100%">

# Animated SVG

Multiple *emphatic* objects may be rendered to an svg animation using
`as_svg_anim()`

``` r
objs <- list(
  hl_grep("hello", "there"),
  hl_grep("goodbye", "good boy")
) 

svg <- as_svg_anim(objs, width = 600, height = 300, duration = 3, 
                   playback = 'infinite')
```

<img src="man/figures/example-svg-anim.svg" width="100%">

## Options

- `hl_opts()` create a named list of default options accepted by the
  functions in this package
- `hl_adjust()` to adjust options after creation.
- Set the following options to control global behaviour within a
  session.
  - `HL_NA`
  - `HL_FULL_COLOUR`
  - `HL_TEXT_MODE`
  - `HL_TEXT_CONTRAST`
- The above R options are initialised using `Sys.getenv()` during
  package start, and otherwise use a default value. Set these values as
  environment variables in your `.Rprofile` to save your preferred
  settings across different sessions. e.g.
  - `Sys.setenv(HL_NA = FALSE)` prior to loading package
  - `options(HL_NA = FALSE)` at any time

## Vignettes

See the [online
documentation](https://coolbutuseless.github.io/package/emphatic/index.html)
for vignettes and more examples.

- [Highlighting
  data.frames](https://coolbutuseless.github.io/package/emphatic/articles/aaa-data-frames.html)
- Specifying rows, columns and colours
  - [Specifying
    rows](https://coolbutuseless.github.io/package/emphatic/articles/specify-rows.html)
  - [Specifying
    columns](https://coolbutuseless.github.io/package/emphatic/articles/specify-columns.html)
  - [Specifying
    colours](https://coolbutuseless.github.io/package/emphatic/articles/specify-colours.html)
- Worked Examples
  - [Space Shuttle O-ring dataset - Challenger
    Disaster](https://coolbutuseless.github.io/package/emphatic/articles/challenger.html)
