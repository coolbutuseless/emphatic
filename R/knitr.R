

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to HTML
#'
#' @param x emphatic object
#' @param style html tag styling to apply to the \code{<pre>} wrapper for the
#'        returned HTML
#' @param ... other arguments passed to \code{as.character.emphatic}
#' @param complete logical. Default: FALSE.  If TRUE, then add DOCTYPE and
#'        the tags for 'html', 'body' and 'head' to make a complete standalone
#'        html file.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_html <- function(x, style = NULL, ..., complete = FALSE) {

  if (!is.null(style)) {
    pre <- paste0("<pre style='", style, "'>")
  } else {
    pre <- "<pre>"
  }

  res <- paste0(pre, as.character(x, ..., mode = 'html'), "</pre>")

  if (isTRUE(complete)) {
    res <- paste0("<!DOCTYPE html>\n<html>\n<head></head>\n<body>", res, "\n</body>\n</html>")
  }


  class(res) <- unique(c('knit_asis', class(res)))

  res
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to typst
#'
#' @param x emphatic object
#' @param ... other arguments passed to \code{as.character.emphatic}
#' @param font name of font. Default: 'Courier New'
#' @param font_size font size in points. default: 10
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_typst <- function(x, ..., font = 'Courier New', font_size = 10) {

  res <- as.character(x, ..., mode = 'typst')

  res <- paste(
    "\n```{=typst}\n",
    "#[",
    paste0('#set text(font: "', font, '", size: ', font_size, 'pt)'),
    res,
    "]",
    "\n```\n",
    sep = "\n"
  )

  class(res) <- unique(c('knit_asis', class(res)))
  attr(res, 'knit_cacheable') <- NA
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Automatically output emphatic objects to HTML knitted documents.
#'
#' @inheritParams as_html
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
knit_print.emphatic <- function(x, style = NULL, ...) {

  if (requireNamespace('knitr', quietly = TRUE) && knitr::is_latex_output()) {
    as_latex(x, ...)
  } else if (requireNamespace('knitr', quietly = TRUE) && knitr::pandoc_to() == 'typst') {
    as_typst(x, ...)
  } else {
    as_html(x, style = style, ...)
  }

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object into an SVG for display in a github README.md
#'
#' Idea borrowed from pointblank
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#'
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg <- function(x, width = 1200, height = 900, ...) {

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
