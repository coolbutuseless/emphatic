
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Automatically output emphatic objects to HTML knitted documents.
#'
#' @inheritParams as_html
#'
#' @return a character vector suitable for output during an rmarkdown render
#'
#' @export
#' @examples
#' mtcars |>
#'   hl('red') |>
#'   knit_print.emphatic()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
knit_print.emphatic <- function(x, style = list(), ...) {

  if (requireNamespace('knitr', quietly = TRUE)) {
    if (knitr::is_latex_output()) {
      as_latex(x, ...)
    } else if (!is.null(knitr::pandoc_to()) && knitr::pandoc_to() == 'typst') {
      as_typst(x, ...)
    } else {
      as_html(x, style = style, ...)
    }
  } else {
    stop("knit_print(): Can't knit because knitr' not installed.")
  }

}
