
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Highlight elements of a matrix or atomic vector
#'
#' \code{hl_mat()} and \code{hl_mat()} are identical functions which both work on
#' matrices and atomic vectors.
#'
#' @inheritParams hl
#' @param .data \code{emphatic} matrix or atomic vector.
#' @param selection specify the locations in the matrix which will be highlighted.
#'        \describe{
#'        \item{NULL}{(default) Apply highlighting to all elements}
#'        \item{numeric vector}{Numeric vector of indices. e.g. \code{c(1, 2, 3)}}
#'        \item{logical vector}{Either length 1, or a length which matches the
#'             total number of elements in the matrix/vector e.g. \code{TRUE}}
#'        \item{expression}{An expression which evaluates to a logical vector,
#'             or vector of indices.
#'             The matrix/vector itself can be referenced in these expressions
#'             using the variable \code{.x}
#'             E.g. \code{abs(.x)> 0.5 & .x != 1}  or \code{row(.x) == 3}}
#'        }
#' @param byrow if replication of the selection is required, how should the data be replicated?
#' @inheritParams hl_grep
#'
#' @return emphatic object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_mat <- function(.data, colour, selection = NULL, elem = 'fill',
                   byrow = FALSE,
                      show_legend = FALSE, opts = hl_opts()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(elem %in% c('text', 'fill'))
  stopifnot(is_matrix(.data) || is_atomic(.data))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote to
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is_emphatic(.data)) {
    .data <- as_emphatic(.data)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Capture expression and evaluate with the matrix in the evaluation
  # environment as '.x'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  .x <- .data
  mat_ids <- eval(substitute(selection))
  # print(mat_ids)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If logical, then ensure it is off the correct length i.e. 1 or length(.data)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.logical(mat_ids)) {
    if (length(mat_ids) == 1) {
      mat_ids <- rep(mat_ids, length(.data))
    } else {
      mat_ids <- matrix(mat_ids, nrow = NROW(.data), ncol = NCOL(.data), byrow = byrow)
    }
    mat_ids <- which(mat_ids)
  } else if (is.null(mat_ids)) {
    x <- matrix(seq_along(.data), nrow = NROW(.data), ncol = NCOL(.data), FALSE)
    # print(x)
    if (byrow) {
      mat_ids <- as.vector(t(x))
    } else {
      mat_ids <- as.vector(x)
    }
  } else if (!is.numeric(mat_ids)) {
    stop("hl_mat() `selection` must by a numeric vector or be an expression with evaluates to a logical vector")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # No NAs allowed in mat_ids
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (anyNA(mat_ids)) {
    stop("hl_mat() `selection`  must not contain NAs: ", deparse(mat_ids))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply the colouring. One of:
  #  - continuous scale
  #  - discrete sale
  #  - valid R colour. Either a single value or a vector which will be
  #    replicated
  #  - NA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(colour, 'ScaleContinuous')) {
    stopifnot(all(colour$aesthetics %in% c('colour', 'color', 'fill')))
    vals         <- .data[mat_ids]
    colour$train(vals)
    final_colour <- colour$map(vals)

    if (isTRUE(show_legend)) {
      legend <- list(
        scale  = colour,
        values = vals,
        label  = 'legend'
      )
      .data <- add_legend(.data, legend)
    }

  } else if (inherits(colour, 'ScaleDiscrete')) {
    stopifnot(all(colour$aesthetics %in% c('colour', 'color', 'fill')))
    vals <- .data[mat_ids]
    colour$train(vals)
    final_colour <- colour$map(vals)

    if (isTRUE(show_legend)) {
      legend <- list(
        scale  = colour,
        values = vals,
        label  = 'legend'
      )
      .data <- add_legend(.data, legend)
    }

  } else if (is.character(colour)) {
    final_colour <- colour
  } else {
    final_colour <- NA
  }

  # print(final_colour)
  # print(mat_ids)
  final_colour <- rep_len(final_colour, length.out = length(mat_ids))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Attach the updated colour attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat          <- get_colour_matrix(.data, elem)
  mat[mat_ids] <- final_colour
  .data        <- set_colour_matrix(.data, elem, mat)

  attr(.data, "options") <- opts

  .data
}
