
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Highlight elements by location within an \code{emphatic} data.frame or matrix.
#'
#' @inheritParams hl
#' @param .data \code{emphatic} data.frame or matrix.
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
#' @return emphatic object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_loc <- function(.data, palette, row_ids, col_ids, elem = 'fill', major = 'row', expand_grid = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(elem %in% c('text', 'fill'))
  stopifnot(major %in% c('row', 'column'))
  stopifnot(is.numeric(row_ids))
  stopifnot(is.numeric(col_ids))
  stopifnot(is.data.frame(.data) || is_matrix(.data)) # must be a matrix or data.frame

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
    if (nrow(ids) != length(palette)) {
      if (major == 'row') {
        palette <- rep(palette, length.out = length(row_ids))
        palette <- rep(palette, length.out = nrow(ids))
      } else if (major == 'column') {
        palette <- rep(palette, length.out = length(col_ids))
        palette <- rep(palette, each       = length(row_ids))
      } else {
        stop("no such 'major': ", major)
      }
    } else {
      # user supplied enough colours to completely fill grid
      if (major == 'column') {
        palette <- matrix(palette, nrow = nrow(.data), ncol = ncol(.data),
                         byrow = TRUE)
        palette <- as.vector(palette)
      }
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assign colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat      <- get_colour_matrix(.data, elem)
  mat[ids] <- palette
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
#'
#' @return emphatic object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_inner <- function(.data, palette, row_ids, column, dest_col_ids, elem, show_legend) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This inner function only accpt a single source column.
  # but results can be applied to multiple 'scale_apply' columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  if (inherits(palette, 'ScaleContinuous')) {
    stopifnot(all(palette$aesthetics %in% c('colour', 'color', 'fill')))
    vals <- unlist(.data[row_ids, column])
    palette$train(vals)
    final_colour <- palette$map(vals)

    if (isTRUE(show_legend)) {
      legend <- list(
        scale  = palette,
        values = vals,
        label  = colnames(.data)[column]
      )
      .data <- add_legend(.data, legend)
    }


  } else if (inherits(palette, 'ScaleDiscrete')) {
    stopifnot(all(palette$aesthetics %in% c('colour', 'color', 'fill')))
    vals <- unlist(.data[row_ids, column])
    palette$train(vals)
    final_colour <- palette$map(vals)

    if (isTRUE(show_legend)) {
      legend <- list(
        scale  = palette,
        values = vals,
        label  = colnames(.data)[column]
      )
      .data <- add_legend(.data, legend)
    }
  } else if (is.character(palette)) {
    final_colour <- palette
  } else {
    final_colour <- NA_character_
  }

  hl_loc(.data, palette = final_colour, row_ids = row_ids, col_ids = dest_col_ids, elem = elem)
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
#' \item{all()}{specifying \code{all()} means that all rows/columns will be
#'      selected}
#' \item{code that will evaluate to row positions}{For \emph{row} selection only, the user
#'      can specify code which will evaluate to a logical vector of rows which
#'      the highlighting should apply to.  These will look like statements used
#'      in \code{dplyr::filter()}. E.g. \code{cyl == 6 & mpg > 20}}
#' }
#'
#'
#' @param .data \code{emphatic} data.frame
#' @param palette colours to use for highlighting.  This may be a single R colour,
#'        a vector of R colours, or
#'        a \code{ggplot2} style "Scale" object e.g. \code{scale_colour_continuous()}.
#' @param rows,cols specification for rows and columns to target.  Default is NULL
#'        for both rows and columns, which will target all columns/rows.
#'        When \code{palette} argument is a \code{scale} object, then \code{cols}
#'        indicates the columns which will be used to calculate the extents of
#'        the scale.
#' @param scale_apply Only valid when palette is a \code{scale} object, specify
#'        the target columns to colour. If missing (the default), this function
#'        will only colour the column specified in the \code{cols} argument.
#'        Use NULL to colour all columns.
#' @param elem Apply the highlighting to the 'fill' (the background) or the 'text'.
#'        Default: 'fill'
#' @param show_legend if a scale object is used for colour, and \code{show_legend = TRUE},
#'        then a colourbar legend will be added to the bottom of the output.
#'        Default: FALSE
#' @inheritParams hl_grep
#'
#' @return An emphatic object suitable to output to console (for example)
#'
#' @export
#' @examples
#' # Simple
#' mtcars |>
#'   head() |>
#'   hl(c('red', 'blue'))
#'
#' # More involved example
#' mtcars |>
#'   head() |>
#'   hl(
#'     ggplot2::scale_colour_viridis_c(),
#'     rows = cyl == 6,
#'     cols = mpg,
#'     scale_apply = c(mpg, cyl)
#'   )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl <- function(.data, palette,
               rows = NULL, cols = NULL,
               scale_apply,
               elem = 'fill',
               show_legend = FALSE,
               opts = hl_opts()) {

  stopifnot(is.data.frame(.data))
  stopifnot(elem %in% c('text', 'fill'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote to 'emphatic' object if necessary
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is_emphatic(.data)) {
    .data <- as_emphatic(.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Unpack which rows/columns are being highlighted
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  .x      <- .data  # used for lookup within loc_expr_to_ids()
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
  if (missing(scale_apply)) {
    dest_col_ids <- col_ids
  } else {
    dest_col_ids <- loc_expr_to_ids(.data, expr = substitute(scale_apply), axis = 'column')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each column spec, run the internal function to highlight a single column
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(palette) || (length(palette) == 1 && is.na(palette))) {
    .data <- hl_loc(
      .data,
      palette  = palette,
      row_ids  = row_ids,
      col_ids  = col_ids,
      elem     = elem
    )
  } else if (inherits(palette, "Scale")) {
    if (length(col_ids) > 1) {
      if (!identical(dest_col_ids, col_ids)) {
        stop("Can't specify 'scale_apply' when 'palette' is a scale applied to more than 1 column")
      }
    }
    .data <- hl_inner(
      .data,
      palette      = palette,
      row_ids      = row_ids,
      column       = col_ids,
      dest_col_ids = dest_col_ids,
      elem         = elem,
      show_legend  = show_legend
    )
  } else {
    stop("'palette' not understood: ", deparse1(palette))
  }

  attr(.data, "options") <- opts

  .data
}
