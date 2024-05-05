
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
![](https://img.shields.io/badge/developing-rapidly-orange.svg)
[![R-CMD-check](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{emphatic}` allows for user-defined highlighting of data.frames.

This highlighting works

- in the console
- rendered to HTML (e.g. in Rmarkdown documents)
- rendered to latex
- rendered to [typst](https://typst.app) (e.g. in Quarto documents)
- rendered to SVG and animated SVG

<img src="data-raw/figures/examples.gif"/>

#### What’s in the box

- `hl()` for user-defined highlighting of data.frames
- `hl_diff()` for highlighting differences between two objects
- `hl_grep()` highlight regular expression matches in an object or
  string
- Output from `hl_*()` functions will automatically be rendered in
  Rmarkdown and Quarto documents with output formats of html, PDF and
  typst.
- The highlighted output can be explicitly converted to other formats
  using:
  - `as_html()`
  - `as_svg()` and animated `as_svg_anim()`
  - `as_typst()`
  - `write_xlsx()` - Excel document

#### Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.packages('remotes')
remotes::install_github('coolbutuseless/emphatic', ref = 'main')
```

## User-controlled highlighting of data.frames with `hl()`

- specify rows and columns you want to highlight
- specify a palette
  - a single colour
  - a vector of colours
  - a `ggplot2` “Scale” object e.g. `scale_colour_continuous()`

#### Simple highlighting a data.frame

By default, colouring will be applied to all rows and columns.

``` r
mtcars |>
  head(15) |>
  hl(c('red', 'white', 'blue')) 
```

<img src="data-raw/figures/example1.svg" width="100%">

#### Complex example of highlighting a data.frame

A more complex example showing how to highlight the `mtcars` dataset
where:

- Determine the colours using `scale_colour_viridis_c()` applied to
  `mpg`
- Apply the scale’s colouring to all columns from `mpg` to `disp`
- highlight the row for the car with the minimum horsepower (`hp`) in
  `hotpink`

`hl()` calls are cumulative - you can build up the highlighting you need
step-by-step

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

<img src="data-raw/figures/example2.svg" width="100%">

## Highlight difference between two objects with `hl_diff()`

The Levenshtein edit distance is calculated between the string
representation of two objects and these edits are then coloured 🟢 =
insert, 🔴 = delete, 🔵 = substitute.

``` r
x <- "Paris in the the spring?"
y <- "Not Paris in the spring!"
hl_diff(x, y)
```

<img src="data-raw/figures/example-strdiff-3.svg" width="100%">

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

<img src="data-raw/figures/example-strdiff-4.svg" width="100%">

## Highlight regular expression matches in objects with `hl_grep()`

`hl_grep()` highlights the regular expression matches within a string or
objects coerced into a string representation.

#### Highlight regular expression matches in a character string

``` r
txt <- "Among the few possessions he left to his heirs was a set of 
Encyclopedia Britannica in storage at the Lindbergh Palace Hotel under the 
names Ari and Uzi Tenenbaum. No-one spoke at the funeral, and Father 
Petersen's leg had not yet mended, but it was agreed among them that Royal 
would have found the event to be most satisfactory.
[Chas, now wearing a black Adidas tracksuit, nods to his sons]"

hl_grep(txt, "event.*satisfactory", coerce = 'character')
```

<img src="data-raw/figures/example-hlgrep-1.svg" width="100%">

#### Highlight regular expression matches within an object

Other R objects (functions, lists, data.frames, etc) can also be
highlighted with regular expressions. How an object is coerced into
string representation is controlled by the `coerce` argument.

In this example, the function body for `mode()` is searched for the word
`switch`:

``` r
hl_grep(mode, 'switch')
```

<img src="data-raw/figures/example-hlgrep-2.svg" width="100%">

# Animated SVG

Multiple *emphatic* objects may be rendered to an svg animation using
`as_svg_anim()`

``` r
objs <- list(
  hl_grep("hello", "there"),
  hl_grep("goodbye", "good boy")
) 

svg <- as_svg_anim(objs, width = 600, height = 300, duration = 2, 
                   playback = 'infinite')
```

<img src="data-raw/figures/example-svg-anim.svg">

## Options

- `hl_opts()` create a named list of default options accepted by the
  functions in this package
- `hl_adjust()` to adjust options after creation.
- These options are initialised at package start time using
  `Sys.getenv()`. Set these values as environment variables in your
  `.Rprofile` to save your preferred settings across different sessions.
  e.g.
  - `Sys.setenv(HL_NA = "<none>")` prior to loading package or in
    `.Rprofile`
  - `options(HL_NA = FALSE)` at any time

| Option                                   | Description                                                                                                                                                                             |
|:-----------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `HL_NA`                                  | String to use for NA values. Default “NA”                                                                                                                                               |
| `HL_FULL_COLOUR`                         | Should full colour ANSI codes be used when outputting to the console? Default: FALSE on Rstudio, but TRUE on all other R consoles                                                       |
| `HL_TEXT_MODE`                           | How to handle text if no text colour has been explicitly specified by the user                                                                                                          |
|                                          | `"contrast"` (default) automatically select a colour which contrasts with the background                                                                                                |
|                                          | `"asis"` do not change the colour from the console’s default                                                                                                                            |
|                                          | `"remove"` remove all text without a user-defined colour                                                                                                                                |
| `HL_TEXT_CONTRAST`                       | When `text_mode = "contrast"` this numeric value (in range \[0, 1\]) adjusts the visibility of the text. Default: 1 (high contrast)                                                     |
| `HL_GREP_COL`                            | The fill colour to use with `hl_grep()` if no colour is specified. Default: “\#0F19F0”                                                                                                  |
| `HL_SUB_COL`, `HL_INS_COL`, `HL_DEL_COL` | the default colours to use with `hl_diff()` for substitution, insertion and deletion (respectively). Defaults: `dodgerblue` (substitute), `darkgreen` (insert) and `firebrick` (delete) |

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
