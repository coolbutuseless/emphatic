
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert R colours to hex colours
#'
#' @param colours Character vector of R colours
#'
#' @return Character vector of 6-char hex colours
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2hex_latex <- function(colours) {

  colours <- grDevices::col2rgb(colours)
  colours <- structure(sprintf("%02X", colours), dim = dim(colours))
  colours <- apply(colours, 2, paste0, collapse = '')

  colours
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening latex code for background colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of latex opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_latex <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex_latex(colours)

  ifelse(
    no_colour,
    "{",
    paste0("\\colorbox[HTML]{", colours, "}{")
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
col2text_latex <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex_latex(colours)

  ifelse(
    no_colour,
    "{",
    paste0("\\textcolor[HTML]{", colours, "}{")
  )

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 'reset' text ready for next text block
# Turn underline on/off
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset_latex         <- "\\vrule height 3mm depth 1.25mm width 0mm}}"
underline_on_latex  <- r"(\underline{)"
underline_off_latex <- "}"

# \texttt{
# \colorbox{BurntOrange}{\textcolor[HTML]{AFFE90}{hello  }}
# \colorbox[HTML]{AFFE90}{\textcolor{blue}{there}}
# }"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape latex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_latex <- function (x, newlines = FALSE, spaces = TRUE) {
  x = gsub("\\\\", "\\\\textbackslash", x)
  x = gsub("([#$%&_{}])", "\\\\\\1", x)
  x = gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
  x = gsub("~", "\\\\textasciitilde{}", x)
  x = gsub("\\^", "\\\\textasciicircum{}", x)
  x = gsub("\n", "\\\\\\\\ \n", x, perl = TRUE)

  # I really struggled finding a way to insert significant whitespace at
  # the start of a line. This is the best I could come up with.
  x = gsub(" ", "\\\\hspace*{0.52em}", x, perl = TRUE)
  x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to Latex
#'
#' @param x emphatic object
#' @param ... other arguments passed to \code{as.character.emphatic()}
#' @param font_size Integer value indicating font size measured in points.
#'        Default: NULL.
#'
#' @return single character string containing a latex representation
#'
#' @export
#' @examples
#' hl_diff("hello", "there") |>
#'   as_latex() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_latex <- function(x, ..., font_size = NULL) {

  font_text <- NULL
  if (!is.null(font_size)) {
    if (!is.numeric(font_size)) {
    stop("as_latex(): 'font_size' must be a numeric indicating font size in points")
    }
    font_size <- as.integer(font_size)
    font_text <- sprintf("\\fontsize{%ipt}{%ipt}\\selectfont\n", font_size, font_size + 2)
  }

  res <- paste0(
    "\\begingroup\n",
    font_text,
    "\\setlength{\\fboxsep}{0pt}\n",
    "\\texttt{\n",
    as.character(x, ..., mode = 'latex'),
    "\n}\n",
    "\\endgroup"
  )

  class(res) <- unique(c('knit_asis', class(res)))

  res
}
