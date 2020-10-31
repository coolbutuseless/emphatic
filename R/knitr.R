

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert ANSI to HTML
#'
#' @param x emphatic object
#' @param style html tag styling to apply to the \code{<pre>} wrapper for the
#'        returned HTML
#' @param ... other arguments passed to \code{as.character.emphatic}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_html <- function(x, style = NULL, ...) {

  if (!is.null(style)) {
    pre <- paste0("<pre style='", style, "'>")
  } else {
    pre <- "<pre>"
  }

  res <- paste0(pre, as.character(x, ..., mode = 'html'), "</pre>")
  class(res) <- unique(c('knit_asis', class(res)))

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Automatically output emphatic matrices and data.frames to HTML knitted documents.
#'
#' @inheritParams as_html
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
knit_print.emphatic <- function(x, style = NULL, ...) {
  as_html(x, style = style, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object into an SVG for display in a github README.md
#'
#' Idea borrowed from pointblank: \url{https://github.com/rich-iannone/pointblank/blob/master/scripts/generate_roadmap.R}
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#'
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg <- function(x, width = 1200, height = 900,...) {

  res <- as_html(x, ...)


  svg_text <- paste0(
      "<svg fill=\"none\" viewBox=\"0 0 ", width, " ", height,
      "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">
      <foreignObject width=\"100%\" height=\"100%\">
      <div xmlns=\"http://www.w3.org/1999/xhtml\">",
    res,
    "</div>
    </foreignObject>
    </svg>
    ")


  svg_text <- gsub("style>", ">", svg_text, fixed = TRUE)

  svg_text
}
