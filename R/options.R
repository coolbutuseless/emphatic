
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set options for printing on the emphatic matrix or data.frame
#'
#' @inheritParams hl_opts
#' @param .data emphatic matrix or data.frame
#'
#' @return emphatic object with updated options
#'
#' @importFrom utils modifyList
#' @export
#' @examples
#' mtcars |>
#'   hl('red') |>
#'   hl_adjust(text_contrast = 0.3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_adjust <- function(.data, na, full_colour, text_mode, text_contrast) {

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a set of options
#'
#' @param na Character string to display for NA values. Default 'NA'
#' @param full_colour Use 24bit ANSI escape codes?  default: FALSE - use 8bit colour.
#'        Note: RStudio only supports 8 bit ANSI output (24bit ANSI is
#'        rendered invisibly in Rstudio).  For 24bit colour output, try R in the terminal
#'        e.g. 'iTerm' on OSX.
#' @param text_mode How to handle text if no text colour has been
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
#'
#' @return named list of standard options
#'
#' @export
#' @examples
#' # Generate a standard set of options
#' hl_opts()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_opts <- function(na               = getOption("HL_NA", "NA"),
                    full_colour      = getOption("HL_FULL_COLOUR", FALSE),
                    text_mode        = getOption("HL_TEXT_MODE", "contrast"),
                    text_contrast    = getOption("HL_TEXT_CONTRAST", 1)) {

  stopifnot(exprs = {
    is.character(na)
    length(na) == 1
    !is.na(na)
  })

  stopifnot(exprs = {
    is.logical(full_colour)
    length(full_colour) == 1
    !is.na(full_colour)
  })

  stopifnot(exprs = {
    is.character(text_mode)
    length(text_mode) == 1
    !is.na(text_mode)
    text_mode %in% c('contrast', 'asis', 'remove')
  })

  stopifnot(exprs = {
    is.numeric(text_contrast)
    length(text_contrast) == 1
    !is.na(text_contrast)
    text_contrast >= 0
    text_contrast <= 1
  })

  list(
    na               = na,
    full_colour      = full_colour,
    text_mode        = text_mode,
    text_contrast    = text_contrast
  )
}
