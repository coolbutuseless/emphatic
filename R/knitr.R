
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Automatically output emphatic objects to HTML knitted documents.
#'
#' @inheritParams as_html
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
knit_print.emphatic <- function(x, style = list(), ...) {

  if (requireNamespace('knitr', quietly = TRUE)) {
    if (knitr::is_latex_output()) {
      as_latex(x, ...)
    } else if (knitr::pandoc_to() == 'typst') {
      as_typst(x, ...)
    } else {
      as_html(x, style = style, ...)
    }
  } else {
    stop("knit_print(): Can't knit because knitr' not installed.")
  }

}
