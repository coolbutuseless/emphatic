


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Highlight elements of a matrix or atomic vector
#'
#' \code{hl_mat()} and \code{hl_vec()} are identical functions which both work on
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
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_mat <- function(.data, colour, selection = NULL, elem = 'fill',
                      show_legend = FALSE) {

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If logical, then ensure it is off the correct length i.e. 1 or length(.data)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.logical(mat_ids)) {
    if (length(mat_ids) == 1) {
      mat_ids <- rep(mat_ids, length(.data))
    } else if (length(mat_ids) != length(.data)) {
      stop("logical result must be length 1 or ", length(.data), ", not ", length(mat_ids))
    }
    mat_ids <- which(mat_ids)
  } else if (is.null(mat_ids)) {
    mat_ids <- seq_len(length(.data))
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Attach the updated colour attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat          <- get_colour_matrix(.data, elem)
  mat[mat_ids] <- final_colour
  .data        <- set_colour_matrix(.data, elem, mat)

  .data

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hl_mat
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_vec <- hl_mat


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example 1  Matrices
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  library(dplyr)

  matrix(round(runif(100), 3), 10, 10) %>%
    hl_mat('limegreen', row(.x) == 5)

  matrix(round(runif(100), 3), 10, 10) %>%
    hl_mat(colour = ggplot2::scale_color_viridis_c())


  matrix(round(runif(100), 3), 10, 10)  %>%
    hl_mat(colour = ggplot2::scale_color_viridis_c(), selection = .x > 0.5,
              show_legend = TRUE)

}


if (FALSE) {

  v <- as.character(1:50)
  v
  v %>% hl_vec('blue')

  v <- rep(Sys.Date(), 50)
  v
  v %>% hl_vec('blue')

  v <- complex(50)
  v
  v %>% hl_vec('blue')

  v <- logical(50)
  v
  v %>% hl_vec('blue')


}






















