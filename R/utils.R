
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard NULL operator
#'
#' @param x,y R objects
#'
#' @return x if not null else y
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
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_atomic <- function(x) {
  is.atomic(x) && is.null(dim(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Really is a 2d matrix, not an array
#'
#' @param x R object
#' @return logical
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
#' @return list of chunked indices
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chunked_indices <- function(total_len, chunk_len) {
  groups <- ceiling(seq_len(total_len)/chunk_len)
  split(seq_len(total_len), groups)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an arbitrary R object to a string
#'
#' @param x object
#' @param coerce How should non-character arguments be coerced to character strings?
#' \describe{
#'   \item{default}{ - the given object \code{x} must already be a character string}
#'   \item{character}{ - performs the matching after first calling
#'           \code{as.character(x)}}
#'   \item{print}{ - performs the matching against the default
#'            \code{print(x)} output}
#'   \item{deparse}{ - performs the matching after first calling
#'           \code{deparse1(x)}}
#'   \item{str}{ - performs the matching on the output of calling
#'           \code{str(x)}}
#' }
#'
#' @return single character string representing the object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coerce_to_string <- function(x, coerce) {
  switch(
    coerce,
    character = {
      x <- capture.output(cat(as.character(x)))
      x <- paste(x, collapse = "\n")
    },
    print = {
      x <- capture.output(x)
      x <- paste(x, collapse = "\n")
    },
    deparse = {
      x <- deparse1(x)
    },
    str = {
      x <- capture.output(str(x, vec.len = 200))
      x <- paste(x, collapse = "\n")
    },
    {
      x <- capture.output(x)
      x <- paste(x, collapse = "\n")
    }
  )

  x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Update a list
#'
#' @param current,new current list and new list. 'new' may be NULL
#'
#' @return updated list
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modify_list <- function (current, new) {

  for (i in names(new)) {
    current[[i]] <- new[[i]]
  }

  current
}
