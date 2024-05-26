
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/emphatic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{emphatic}` is a tool for exploratory analysis of tabular data. It
allows the user to visually colour elements of the data, yet still keep
all values visible.

Conceptually, `{emphatic}` highlighting could be considered to lie
between *tabular output* and *graphical output* - like a table it shows
all values, but like graphs these values are used to control appearance
(i.e. colour).

`{emphatic}` follows the conventions of `{dplyr}` for selecting rows and
columns; advanced colouring is via colour scales provided by
`{ggplot2}`.

#### Supported output

This highlighting works

- in the **console**
- when output to **Excel** (using `write_xlsx()`)
- rendered in **Rmarkdown** documents (to `HTML`, `latex` and `typst`
  outputs)
- rendered in **Quarto** documents (to `HTML`, `latex` and `typst`
  outputs)
- rendered to **SVG**
- rendered as multiple frames to **animated SVG**

<details open>
<summary style="font-size: large;">
Click here to show/hide gif demo
</summary>
<img src="man/figures/examples.gif"/>
</details>

#### What’s in the box

- `hl()` for user-defined highlighting of data.frames
- `hl_diff()` for highlighting differences between two objects
- `hl_grep()` highlight regular expression matches in an object or
  string
- Manual conversion to other formats using:
  - `as_html()`
  - `as_svg()` and animated `as_svg_anim()`
  - `as_typst()`
  - `write_xlsx()` - Excel document
- Conversion to the correct output format is performed automatically
  when knitting an Rmarkdown or Quarto document.

#### Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.packages('remotes')
remotes::install_github('coolbutuseless/emphatic')
```

## Highlighting of data.frames with `hl()`

`hl()` lets you specify a palette, and the rows/columns of the
data.frame the palette should apply to.

`hl()` calls are cumulative - so the required highlighting can be built
up one-step-at-a-time.

To add highlighing to a data.frame:

- **Specify a palette**
  - a single colour or vector of colours
  - a `ggplot2` *Scale* object e.g. `scale_colour_continuous()`
- **Specify rows and columns**
  - default: highlight all rows and all columns.
  - numeric vector giving row/column indices e.g. `c(1, 2, 8)`, `1:8`
  - character vector giving row/column names e.g. `c('mpg', 'wt')`
  - vector of bare column names e.g. `c(mpg, wt)`, `mpg:wt`
  - tidyselect-style selectors: `starts_with()`, `ends_with()`,
    `everything()`, `all_of()`, `any_of()`, `matches()`, `contains()`,
    `row_number()`, `n()`
  - row selection using filtering operations e.g. `cyl == 6 & mpg > 20`

#### Simple example

By default, colouring will be applied to all rows and columns, and the
supplied vector of colours will be recycled to meet the required length.

``` r
mtcars |>
  head(15) |>
  hl(c('red', 'white', 'blue')) 
```

<img src="man/figures/example1.svg" width="100%">

#### Complex example

A more complex example showing how to highlight the `mtcars` dataset
where:

- Highlight the row for the car with the minimum horsepower
- Determine an expressive colouring for `mpg` using
  `scale_colour_viridis_c()` - where low values are darker, and high
  values are a bright yellow
- Apply the scale’s colouring to all columns from `mpg` to `disp`

``` r
mtcars |>
  head(15) |>
  hl('hotpink', rows = hp == min(hp)) |>
  hl(
    palette     = ggplot2::scale_colour_viridis_c(option = 'A'),
    cols        = mpg,      # Where the colour scale is calculated
    scale_apply = mpg:disp, # Where the colour scale is applied
    show_legend = TRUE
  ) 
```

<img src="man/figures/example2.svg" width="100%">

## Highlight difference between two objects with `hl_diff()`

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

## Highlight regular expression matches in objects with `hl_grep()`

`hl_grep()` highlights the regular expression matches within a string or
objects coerced into a string representation.

#### Highlight regular expression matches in a character string

``` r
txt <- "Among the few possessions he left to his heirs was a set of 
Encyclopedia Britannica in storage at the Lindbergh Palace Hotel under
the  names Ari and Uzi Tenenbaum. No-one spoke at the funeral, and 
Father Petersen's leg had not yet mended, but it was agreed among them 
that Royal would have found the event to be most satisfactory.
[Chas, now wearing a black Adidas tracksuit, nods to his sons]"

hl_grep(txt, "event.*satisfactory", coerce = 'character')
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

svg <- as_svg_anim(objs, width = 600, height = 300, duration = 2, 
                   playback = 'infinite', font_size = "2em")
```

<img src="man/figures/example-svg-anim.svg">

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
  - `options(HL_NA = ".")` at any time

| Option                                   | Description                                                                                                                         |
|:-----------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------|
| `HL_NA`                                  | String to use for NA values. Default “NA”                                                                                           |
| `HL_FULL_COLOUR`                         | Should full colour ANSI codes be used when outputting to the console? Default: FALSE on Rstudio, but TRUE on all other R consoles   |
| `HL_TEXT_MODE`                           | How to handle text if no text colour has been explicitly specified by the user                                                      |
|                                          | `"contrast"` (default) automatically select a colour which contrasts with the background                                            |
|                                          | `"asis"` do not change the colour from the console’s default                                                                        |
|                                          | `"remove"` remove all text without a user-defined colour                                                                            |
| `HL_TEXT_CONTRAST`                       | When `text_mode = "contrast"` this numeric value (in range \[0, 1\]) adjusts the visibility of the text. Default: 1 (high contrast) |
| `HL_GREP_COL`                            | The fill colour to use with `hl_grep()` if no colour is specified. Default: “\#0F19F0”                                              |
| `HL_SUB_COL`, `HL_INS_COL`, `HL_DEL_COL` | the default colours to use with `hl_diff()` for substitution, insertion and deletion (respectively).                                |

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
