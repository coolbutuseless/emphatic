
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert R colours to hex colours
#'
#' @param colours Character vector of R colours
#'
#' @return Character vector of 6-char hex colours
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2hex <- function(colours) {

  colours <- grDevices::col2rgb(colours)
  colours <- structure(sprintf("%02x", colours), dim = dim(colours))
  colours <- apply(colours, 2, paste0, collapse = '')

  paste0('#', colours)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening html spans for background colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of HTML opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_html <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    "<span>",
    paste0("<span style='background-color:", colours, ";'>")
  )

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening html spans for text colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of HTML opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text_html <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    "<span>",
    paste0("<span style='color:", colours, ";'>")
  )

}

reset_html         <- "</span></span>"
underline_on_html  <- "<span style='text-decoration:underline;'>"
underline_off_html <- "</span>"




html_replacement <- c(
  `&` = "&amp;",
  `<` = "&lt;",
  `>` = "&gt;",
  `"` = "&quot;",
  `'` = "&#39;"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape HTML by replacing special characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_html <- function(x) {
  x <- enc2utf8(x)
  for (orig in names(html_replacement)) {
    x <- gsub(orig, html_replacement[[orig]], x, fixed = TRUE, useBytes = TRUE)
  }
  Encoding(x) <- 'UTF-8'
  x
}
