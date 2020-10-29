


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Highlight elements within an \code{emphatic} matrix or data.frame by location.
#'
#' @inheritParams hl
#' @param row_ids,col_ids numeric indices of which rows and columns to highlight
#' @param major which is the direction in which to first replicate colours?
#'        i.e. should colour be replicated to match the number of rows first,
#'        or the number of columns? Default: 'row'. Possible values: 'row', 'column'
#' @param expand_grid How should the vectors of row and column indices be combined
#'        to create location coorinates? If \code{expand_grid = TRUE} (the default)
#'        then a full outer join is done
#'        on rows and cols using \code{expand.grid()} - this means that all possible
#'        combinations of the specified rows and columns will be highlighted.
#'        Otherwise the locations are
#'        created using a simpler call to \code{cbind()}.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_loc <- function(.data, colour, row_ids, col_ids, elem = 'fill', major = 'row', expand_grid = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(elem %in% c('text', 'fill'))
  stopifnot(major %in% c('row', 'column'))
  stopifnot(is.numeric(row_ids))
  stopifnot(is.numeric(col_ids))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote to emphatic if necessary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is_emphatic(.data)) {
    .data <- as_emphatic(.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If expand.grid=FALSE, then each x and y is taken as a set of paris
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!isTRUE(expand_grid)) {
    stopifnot(length(row_ids) == length(col_ids) || length(row_ids) == 1 || length(col_ids) == 1)
    ids <- cbind(row_ids, col_ids)
  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if expand.grid = TRUE, then full cross join.
    # Depending on which of rows/columns are set, replicate colours accordingly
    #  If major='row'
    #    - fill colour through the rows first and then duplicate to the columns
    #  if major = 'col'
    #    - fill colour across the columns first, and then duplicate to the rows
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ids <- as.matrix(expand.grid(row_ids, col_ids))
    if (nrow(ids) != length(colour)) {
      if (major == 'row') {
        colour <- rep(colour, length.out = length(row_ids))
        colour <- rep(colour, length.out = nrow(ids))
      } else if (major == 'column') {
        colour <- rep(colour, length.out = length(col_ids))
        colour <- rep(colour, each       = length(row_ids))
      } else {
        stop("no such 'major': ", major)
      }
    } else {
      # user supplied enough colours to completely fill grid
      if (major == 'column') {
        colour <- matrix(colour, nrow = nrow(.data), ncol = ncol(.data),
                         byrow = TRUE)
        colour <- as.vector(colour)
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat      <- get_colour_matrix(.data, elem)
  mat[ids] <- colour
  .data    <- set_colour_matrix(.data, elem, mat)


  .data
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Private inner function for highlighting
#'
#' This function assumes a lot of pre-processing has gone on and that the 'rows'
#' and 'column' are now numeric indices (not expressions, or names or anything else)
#'
#' @inheritParams hl
#' @inheritParams hl_loc
#' @param column Single numeric index of the column to colour
#' @param dest_col_ids column ids to apply highlighting to
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_inner <- function(.data, colour, row_ids, column, dest_col_ids, elem) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This inner function only accpt a single source column.
  # but results can be applied to multiple 'dest_cols' columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(length(column) == 1)
  stopifnot(is.numeric(column))
  stopifnot(is.numeric(row_ids))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the colouring. One of:
  #  - continuous scale
  #  - discrete sale
  #  - valid R colour. Either a single value or a vector which will be
  #    replicated
  #  - Anything NA is treated as resetting the particular location
  #  - Anything else is also treated as resetting the particular location.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(colour, 'ScaleContinuous')) {
    stopifnot(all(colour$aesthetics %in% c('colour', 'color', 'fill')))
    vals         <- .data[[column]][row_ids]
    colour$train(vals)
    final_colour <- colour$map(vals)
  } else if (inherits(colour, 'ScaleDiscrete')) {
    stopifnot(all(colour$aesthetics %in% c('colour', 'color', 'fill')))
    vals <- .data[[column]][row_ids]
    colour$train(vals)
    final_colour <- colour$map(vals)
  } else if (is.character(colour)) {
    final_colour <- colour
  } else {
    final_colour <- NA_character_
  }

  hl_loc(.data, colour = final_colour, row_ids = row_ids, col_ids = dest_col_ids, elem = elem)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Highlight elements in a data.frame
#'
#' Highlight elements in a data.frame by specifying rows and columns, and the
#' colour to be applied.  The colour can be either a vector of colours expressed
#' as characters (e.g. 'red', '#ff0000'), or a \code{ggplot2} Scale object
#' e.g. \code{scale_colour_viridis_c()}.
#'
#' @section Row and Column Specifications:
#'
#' Specifying rows and columns can be done in a number of ways.  These methods
#' are similar to the ideas of \code{tidyselect} and \code{dplyr} commands such
#' as \code{filter()} and \code{select()}
#'
#' \describe{
#' \item{numeric vector}{row or column indices specified as a numeric vector
#'      e.g. \code{c(1, 2, 8)}}
#' \item{character vector}{vector of names matching row or column names
#'      e.g. \code{c('mpg', 'wt')}}
#' \item{vector of symbols/names}{vector of symbols which will be evaluated as
#'      column names e.g. \code{c(mpg, wt)}}
#' \item{numeric range}{range of indices specified using the \code{:} operator
#'      e.g. \code{1:8}}
#' \item{symbolic range}{range of columns specified using the \code{:} operator
#'      e.g. \code{mpg:wt}}
#' \item{tidyselect-style selectors}{\code{starts_with()}, \code{ends_with()},
#'      \code{everything()}, \code{all_of()}, \code{any_of()}, \code{matches()}
#'       \code{contains()}, \code{row_number()}, \code{n()}.
#'      These work similar to \code{dplyr} and \code{tidyselect} but are bespoke
#'      implementations so there may be some differences}
#' \item{NULL}{specifying \code{NULL} means that all rows/columns will be
#'      selected}
#' \item{.all}{specifying \code{.all} means that all rows/columns will be
#'      selected}
#' \item{code that will evaluate to row positions}{For \emph{row} selection only, the user
#'      can specify code which will evaluate to a logical vector of rows which
#'      the highlighting should apply to.  These will look like statements used
#'      in \code{dplyr::filter()}. E.g. \code{cyl == 6 & mpg > 20}}
#' }
#'
#'
#' @param .data \code{emphatic} data.frame
#' @param colour colour to use for highlighting.  This may be an R colour,
#'        a vector of R colours, or
#'        a \code{ggplot2} style "Scale" object e.g. \code{scale_colour_continuous()}.
#' @param rows,cols specification for rows and columns to target.  Default is NULL
#'        for both rows and columns,
#'        which will target all columns/rows. See documentation for \code{hl()}
#'        for the valid types of row/column specifcations.
#' @param dest_cols specification of destination columns to colour. If
#'        missing (the default), this function
#'        will only colour the columns specified in the \code{cols} argument.
#'        Use NULL to colour all columns.  See documentation for \code{hl()}
#'        for the valid types of column specifcations.
#' @param calc_scale If \code{colour} is a \code{ggplot2} "Scale" object, this
#'        option defines how the scale should be applied.
#'        \describe{
#'          \item{first}{(default)the colours to use are calculated using the scale applied
#'          to the first specified column in \code{cols}.  The colours calculated
#'          on this first column are then copied to the other columns specified
#'          in \code{dest_cols}}.
#'          \item{each}{the colour scale is applied individually to each column
#'          in turn. \code{calc_scale = 'each'} can only be applied if
#'          \code{dest_cols} is identical to \code{cols}}.
#'        }
#' @param elem Apply the highlighting to the 'fill' (the background) or the 'text'.
#'        Default: 'fill'
#'
#' @examples
#' \dontrun{
#' hl(mtcars, ggplot2::scale_colour_viridis_c(), rows = cyl == 6, cols = mpg, dest_cols = c(mpg, cyl))
#' }
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl <- function(.data, colour, rows = NULL, cols = NULL, dest_cols, calc_scale = 'first', elem = 'fill') {

  stopifnot(is.data.frame(.data))
  stopifnot(elem %in% c('text', 'fill'))
  stopifnot(calc_scale %in% c('first', 'each'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote to 'emphatic' object if necessary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is_emphatic(.data)) {
    .data <- as_emphatic(.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Unpack which rows/columns are being highlighted
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  .x             <- .data
  col_ids <- loc_expr_to_ids(.data, expr = substitute(cols), axis = 'column')
  row_ids <- loc_expr_to_ids(.data, expr = substitute(rows), axis = 'row'   )

  if (length(col_ids) == 0) {
    stop("hl(): No valid columns specified")
  }

  if (length(row_ids) == 0) {
    stop("hl(): No valid rows specified.")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign a sensible set of destination columns.
  # If the user has not specified any (i.e. missing()) then consider the
  # destination indices to be the same as \code{cols}
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (missing(dest_cols)) {
    dest_col_ids <- col_ids
  } else {
    dest_col_ids <- loc_expr_to_ids(.data, expr = substitute(dest_cols), axis = 'column')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each column spec, run the internal function to highlight a single column
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(colour) || (length(colour) == 1 && is.na(colour))) {
    .data <- hl_loc(
      .data,
      colour  = colour,
      row_ids = row_ids,
      col_ids = col_ids,
      elem    = elem
    )
  } else if (inherits(colour, "Scale") && calc_scale == 'first') {
    .data <- hl_inner(
      .data,
      colour       = colour,
      row_ids      = row_ids,
      column       = col_ids[1],
      dest_col_ids = dest_col_ids,
      elem         = elem
    )
  } else if (inherits(colour, "Scale") && calc_scale == 'each') {
    if (!identical(col_ids, dest_col_ids)) {
      stop("calc_scale = 'each' can only be used if 'dest_cols' is identical to 'columns'")
    }
    for (col_id in col_ids) {
      .data <- hl_inner(
        .data,
        colour       = colour,
        row_ids      = row_ids,
        column       = col_id,
        dest_col_ids = col_id,
        elem         = elem
      )
    }
  } else {
    stop("colour not understood: ", deparse(colour))
  }


  .data
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  library(dplyr)
  library(emphatic)

  head(mtcars) %>%
    hl('red', rows = cyl == 6, cols = mpg) %>%
    hl('pink', cols = seq(2, 8, 2)) %>%
    hl('limegreen', rows = matches('hornet'), cols = ends_with('t')) %>%
    hl_loc('skyblue', row_ids = 1:3, col_ids = 4:6, expand_grid = FALSE)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  mtcars %>%
    hl(rainbow(nrow(.))) %>%
    hl(NA, rows = seq(2, 10, 2)) %>%
    hl(NA, cols = 'drat')


  # This example seems to not colour some thing
  mtcars %>%
    hl(ggplot2::scale_fill_viridis_c(), cols = mpg, dest_cols = NULL)


  mtcars %>%
    hl(ggplot2::scale_fill_viridis_c(), cols = mpg, dest_cols = NULL, rows = row_number() > n()/2)

}









