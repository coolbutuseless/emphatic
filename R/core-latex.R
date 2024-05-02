
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

reset_latex         <- "\\vrule height 3mm depth 1.25mm width 0mm}}"
underline_on_latex  <- r"(\underline{)"
underline_off_latex <- "}"

# res <- r"(\texttt{
# \colorbox{BurntOrange}{\textcolor[HTML]{AFFE90}{hello  }}\colorbox[HTML]{AFFE90}{\textcolor{blue}{there}}
# })"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape latex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_latex <- function (x, newlines = FALSE, spaces = TRUE) {
  x = gsub("\\\\", "\\\\textbackslash", x)
  x = gsub("([#$%&_{}])", "\\\\\\1", x)
  x = gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
  x = gsub("~", "\\\\textasciitilde{}", x)
  x = gsub("\\^", "\\\\textasciicircum{}", x)
  # if (newlines)
  #   x = gsub("(?<!\n)\n(?!\n)", "\\\\\\\\", x, perl = TRUE)
  x = gsub("\n", "\\\\\\\\\n", x, perl = TRUE)
  x = gsub(" ", "\\\\hspace*{1ex}", x, perl = TRUE)
  # if (spaces)
    # x = gsub("(?<= ) ", "\\\\ ", x, perl = TRUE)
  x
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to Latex
#'
#' @param x emphatic object
#' @param ... other arguments passed to \code{as.character.emphatic}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_latex <- function(x, ...) {

  res <- paste0(
    "\\setlength{\\fboxsep}{0pt}\n",
    "\\texttt{",
    as.character(x, ..., mode = 'latex'),
    "}"
  )

  class(res) <- unique(c('knit_asis', class(res)))

  res
}
