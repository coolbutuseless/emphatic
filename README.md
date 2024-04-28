
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emphatic

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
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

<span style="font-size:smaller">`hl_` prefix can be read as
`highlight`</span>

#### Installation

You can install from
[GitHub](https://github.com/coolbutuseless/emphatic) with:

``` r
# install.package('remotes')
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

## `hl_diff()` highlight difference between two objects

The Levenshtein edit distance is calculated between the string
representation of two objects and these edits are then coloured for
<code style="color:green">insert</code>,
<code style="color:red">delete</code> and
<code style="color:blue">substitute</code>.

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

<pre><span><span>function (x</span></span><span style='color:#000000;'><span style='background-color:#006400;'>               </span></span><span><span>, ...) 
UseMethod("me</span></span><span style='color:#000000;'><span style='background-color:#006400;'>  </span></span><span><span>an")
<bytecode: 0x15</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>1</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'>6</span></span><span><span>a</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>b498</span></span><span style='color:#000000;'><span style='background-color:#006400;'> </span></span><span><span>>
<environment: namespace:</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>b</span></span><span style='color:#000000;'><span style='background-color:#006400;'> </span></span><span><span>a</span></span><span style='color:#000000;'><span style='background-color:#006400;'> </span></span><span><span>s</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'>e</span></span><span><span>></span></span><br/><span><span> </span></span><br/><span><span>function (x</span></span><span style='color:#000000;'><span style='background-color:#006400;'>, na.rm = FALSE</span></span><span><span>, ...) 
UseMethod("me</span></span><span style='color:#000000;'><span style='background-color:#006400;'>di</span></span><span><span>an")
<bytecode: 0x15</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>0</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'> </span></span><span><span>a</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>97c4</span></span><span style='color:#000000;'><span style='background-color:#006400;'>0</span></span><span><span>>
<environment: namespace:</span></span><span style='color:#000000;'><span style='background-color:#1e90ff;'>s</span></span><span style='color:#000000;'><span style='background-color:#006400;'>t</span></span><span><span>a</span></span><span style='color:#000000;'><span style='background-color:#006400;'>t</span></span><span><span>s</span></span><span style='color:#000000;'><span style='background-color:#cd2626;'> </span></span><span><span>></span></span></pre>

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

<pre><span><span>[1] "Four score and seven years ago our fathers brought forth on"             
[2] "this continent, a new nation, conceived in Liberty, and dedicated to the"
[3] "proposition that all </span></span><span style='color:#ffff00;'><span style='background-color:#000000;'>men are created equal</span></span><span><span>."                             </span></span></pre>

#### Highlight regular expression matches within an object

Objects such as functions, lists, data.frames and lists can also be
highlighted with regular expressions. How an object is coerced into
string representation is controlled by the `coerce` argument.

In this example, the function body for `mode()` is searched for the word
`switch`:

``` r
hl_grep(mode, 'switch')
```

<pre><span><span>function (x) 
{
    if (is.expression(x)) 
        return("expression")
    if (is.call(x)) 
        return(</span></span><span style='color:#ffff00;'><span style='background-color:#000000;'>switch</span></span><span><span>(deparse(x[[1L]])[1L], `(` = "(", "call"))
    if (is.name(x)) 
        "name"
    else </span></span><span style='color:#ffff00;'><span style='background-color:#000000;'>switch</span></span><span><span>(tx <- typeof(x), double = , integer = "numeric", 
        closure = , builtin = , special = "function", tx)
}
<bytecode: 0x1501e31d8>
<environment: namespace:base></span></span></pre>

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
