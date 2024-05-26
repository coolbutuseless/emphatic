
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening latex code for background colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of latex opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_typst <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    '#[`',  # do nothing
    paste0('#highlight(fill: rgb("', colours, '"))[`')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening latex code for text colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of latex opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text_typst <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    paste0('#['),
    paste0('#text(fill: rgb("', colours, '"))[')
  )

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reset styling ready for next text block
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset_typst         <- "`]]"
underline_on_typst  <- "#underline["
underline_off_typst <- "]"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Approximately of what the output in 'typst' code looks like
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  #highlight(fill: rgb("aaaaaa"))[
#    #text(font: "Courier New", fill: rgb("#0000ff"))[
#      Hello\u{00a0}\u{00a0}\u{00a0}\u{00a0}there \
#    ]
#  ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape typst by replacing special characters
# Since export is using 'raw' blocks, don't need to escape anything
# except for the backtick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_typst <- function(x) {
  x <- enc2utf8(x)
  x <- gsub("`"  , '`#raw("`")`', x, useBytes = TRUE)
  Encoding(x) <- 'UTF-8'
  x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to typst
#'
#' @param x emphatic object
#' @param ... other arguments passed to \code{as.character.emphatic()}
#' @param font name of font. Default: NA means to just use the default raw
#'        font
#' @param font_size font size in points. default: 10
#' @param line_spacing line spacing in \code{em} units. Default: 0.3
#'
#' @return Character string containing \code{typst} representation
#'
#' @export
#' @examples
#' hl_diff("hello", "there") |>
#'   as_typst() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_typst <- function(x, ..., font_size = 10, font = NA, line_spacing = 0.3) {

  res <- as.character(x, ..., mode = 'typst')

  set_font <- NULL
  if (!is.na(font)) {
    set_font <- paste0('#show raw: set text(font: "', font, '")')
  }

  res <- paste(
    "\n```{=typst}",
    "#[",
    paste0('#set text(size: ', font_size, 'pt, hyphenate: false)'),
    paste0('#set par(leading: ', line_spacing, 'em)'),
    set_font,
    res,
    "]",
    "```\n",
    sep = "\n"
  )

  class(res) <- unique(c('knit_asis', class(res)))
  attr(res, 'knit_cacheable') <- NA
  res
}
