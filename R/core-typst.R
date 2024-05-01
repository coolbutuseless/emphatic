


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
    '#[',  # do nothing
    paste0('#highlight(fill: rgb("', colours, '"))[')
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

reset_typst         <- "]]"
underline_on_typst  <- "#underline["
underline_off_typst <- "]"

#  #highlight(fill: rgb("aaaaaa"))[
#    #text(font: "Courier New", fill: rgb("#0000ff"))[
#      Hello\u{00a0}\u{00a0}\u{00a0}\u{00a0}there \
#    ]
#  ]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape HTML by replacing special characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_typst <- function(x) {
  x <- enc2utf8(x)
  x <- gsub(" ", "\\\\u{00a0}", x, useBytes = TRUE)
  x <- gsub("<"  , "\\\\<", x, useBytes = TRUE)
  x <- gsub("\n"  , "\\\\\n", x, useBytes = TRUE)
  Encoding(x) <- 'UTF-8'
  x
}
