

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A helper function to set global options
#'
#' Any global options set in this manner can be overridden by setting them
#' as function arguments to the \code{primt()} methods defined for \code{emphatic}
#' matrices and data.frames.
#'
#' @param na Character string to display for NA values. Default 'NA'
#' @param full_colour Use 24bit ANSI escape codes?  default: FALSE - use 8bit colour.
#'        Note: RStudio only supports 8 bit ANSI output (24bit ANSI is
#'        rendered invisibly in Rstudio).  For 24bit colour output, try R in the terminal
#'        e.g. 'iTerm' on OSX.
#' @param text_mode How to handle textif no text colour has been
#'        explicitly specified by the user.
#'        \describe{
#'        \item{contrast}{(default) automatically select a contrasting colour for enhanced readability.}
#'        \item{asis}{render text in the default text
#'             colour for the output device, unless the user has already specified
#'             a text colour at this location}
#'        \item{remove}{remove all text without a user-defined colour}
#'        }
#' @param text_contrast When \code{text_mode='contrast'} this numeric value in
#'        range [0, 1] adjusts the visibility. Default: 1 (high contrast)
#' @param dark_mode Output terminal is in 'dark mode'? default: TRUE means that
#'        the terminal display is light coloured text on a dark background.
#'        If your terminal displays dark text on a light background, set
#'        \code{dark_mode = FALSE}
#' @param underline_header Draw an underline separating the column header from
#'        the data? Default: TRUE
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_opt_global <- function(na, dark_mode, full_colour, text_mode, text_contrast, underline_header) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find any arguments that were actually set
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_options <- find_args()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(new_options$text_mode)) {
    stopifnot(new_options$text_mode %in% c('asis', 'contrast', 'remove'))
  }

  if (!is.null(new_options$text_contrast)) {
    stopifnot(
      is.numeric(new_options$text_contrast),
      new_options$text_contrast >= 0,
      new_options$text_contrast <= 1
    )
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update the current global options with any new options the user specified
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  current_options <- getOption('emphatic', default = list())
  new_options     <- modifyList(current_options, new_options)

  options(emphatic = new_options)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set options for printing on the emphatic matrix or data.frame
#'
#' @inheritParams hl_opt_global
#' @param .data emphatic matrix or data.frame
#'
#' @return emphatic object with updated options
#'
#' @importFrom utils modifyList
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_opt <- function(.data, na, dark_mode, full_colour, text_mode, text_contrast, underline_header) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is_emphatic(.data))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 'magically' get any arguments the user has provided. Ignore ".data"
  # from the list of options
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_options <- find_args()
  new_options$.data <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the current options attributes on this emphatic object and
  # update them with any user-specified values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  current_options <- attr(.data, 'options', exact = TRUE) %||% list()
  new_options <- modifyList(current_options, new_options)
  attr(.data, 'options') <- new_options

  .data
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Private function: Code inspired by ggplot2 for dealing with calls to theme()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_args <- function () {

  is_missing_arg <- function(x) identical(x, quote(expr = ))

  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
  vals
}







