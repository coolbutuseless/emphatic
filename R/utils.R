

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard NULL operator
#'
#' @param x,y R objects
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Really atomic vector. Not a matrix or array!
#'
#' @param x R object
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_atomic <- function(x) {
  is.atomic(x) && is.null(dim(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Really is a 2d matrix, not an array
#'
#' @param x R object
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_matrix <- function(x) {
  is.matrix(x) && length(dim(x)) == 2
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Split a sequence (1, total_len) into chunks each with 'chunk_len' elements.
#'
#' @param total_len the total length of the sequence
#' @param chunk_len the length of each chunk
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chunked_indices <- function(total_len, chunk_len) {
  groups <- ceiling(seq_len(total_len)/chunk_len)
  split(seq_len(total_len), groups)
}


if (FALSE) {
  chunked_indices(4, 6)
}
