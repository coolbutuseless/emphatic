
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extra tags for
#   - closing a section of same coloured text
#   - turning underline on/off
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset_html         <- "</span></span>"
underline_on_html  <- "<span style='text-decoration:underline;'>"
underline_off_html <- "</span>"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape HTML by replacing special characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_html <- function(x) {

  html_replacement <- c(
    `&` = "&amp;",
    `<` = "&lt;",
    `>` = "&gt;",
    `"` = "&quot;",
    `'` = "&#39;"
  )

  x <- enc2utf8(x)
  for (orig in names(html_replacement)) {
    x <- gsub(orig, html_replacement[[orig]], x, fixed = TRUE, useBytes = TRUE)
  }
  Encoding(x) <- 'UTF-8'
  x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to HTML
#'
#' @param x emphatic object
#' @param style html tag styling to apply to the \code{<pre>} wrapper for the
#'        returned HTML
#' @param ... other arguments passed to \code{as.character.emphatic()}
#' @param complete logical. Default: FALSE.  If TRUE, then add DOCTYPE and
#'        the tags for 'html', 'body' and 'head' to make a complete standalone
#'        html file.
#' @inheritParams as_svg_anim
#' @param font_size CSS font-size. Default: NULL means to not adjust font size.
#'        Otherwise, use valid CSS \code{font-size} specification e.g.
#'        "3em", "22px" etc.
#'
#' @return Character string containing HTML representation
#'
#' @export
#' @examples
#' hl_diff('hello', 'there') |>
#'   as_html() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_html <- function(x, ..., font_size = NULL, style = list(), complete = FALSE, browsable = FALSE) {

  if (!is.list(style)) {
    stop("'style' must a named list of CSS style options")
  }

  if (!is.null(font_size)) {
    style$`font-size` <- font_size
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Style the <pre> tag
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  style <- paste(names(style), style, collapse = "; ", sep = ":")
  pre <- paste0("<pre style='", style, "'>\n")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create HTML
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- paste0(pre, as.character(x, ..., mode = 'html'), "\n</pre>")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Should the HTML be 'complete'?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(complete)) {
    res <- paste0("<!DOCTYPE html>\n<html>\n<head></head>\n<body>", res, "\n</body>\n</html>")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set as browsable to show in the Rstudio viewer instead of the console
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(browsable)) {
    attr(res, "html") <- TRUE
    attr(res, "browsable_html") <- TRUE
  }
  class(res) <- union(c('knit_asis', 'html', 'character'), class(res))


  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Show HTML or SVG content in the rstudio viewer pane
#'
#' @param x svg or html
#' @param viewer function which activates viewer
#' @return None
#' @export
#' @examplesIf interactive()
#' # This example will try and spawn an external viewer for HTML content
#' hl_grep(mode, "switch") |>
#'   as_html() |>
#'   show_html()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_html <- function(x, viewer = getOption("viewer", utils::browseURL)) {

  if (Sys.getenv("RSTUDIO", "0") != "1" || is.null(viewer)) {
    message("No viewer available")
    invisible(NULL)
  }

  dir <- tempfile('html')
  dir.create(dir, showWarnings = FALSE)
  index_file <- file.path(dir, "index.html")
  writeLines(x, index_file)

  viewer(index_file)

  invisible(index_file)
}




