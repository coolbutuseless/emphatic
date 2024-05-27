
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidyselect style functions.
#
# Using function factories to generate these such that the relevant row
# and/or column names will be in scope during the evaluation of row and
# column specifications.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gen_starts_with <- function(data_names) {
  function(match, ignore.case = TRUE) {
    if (ignore.case) {
      ids <- startsWith(tolower(data_names), tolower(match))
    } else {
      ids <- startsWith(data_names, match)
    }
    which(ids)
  }
}

gen_ends_with <- function(data_names) {
  function(match, ignore.case = TRUE) {
    if (ignore.case) {
      ids <- endsWith(tolower(data_names), tolower(match))
    } else {
      ids <- endsWith(data_names, match)
    }
    which(ids)
  }
}

gen_everything <- function(data_names) {
  function() {
    seq_along(data_names)
  }
}

gen_all_of <- function(data_names) {
  function(x) {
    if (!all(x %in% data_names)) {
      stop("all_of() excpects all names to be present in data")
    }
    which(data_names %in% x)
  }
}

gen_any_of <- function(data_names) {
  function(x) {
    which(data_names %in% x)
  }
}

gen_matches <- function(data_names) {
  function(match, ignore.case = TRUE) {
    grep(match, data_names, ignore.case = ignore.case)
  }
}

gen_row_number <- function(.data) {
  function() {
    seq_len(nrow(.data))
  }
}

gen_col_number <- function(.data) {
  function() {
    seq_len(ncol(.data))
  }
}

gen_n_row <- function(.data) {
  function() {
    nrow(.data)
  }
}

gen_n_col <- function(.data) {
  function() {
    ncol(.data)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a column or row expression into a vector of index locations
#'
#' @param .data data.frame
#' @param expr an expression wihch will be interpreted as indices
#' @param axis 'row' or 'column' expression?
#' @return IDs matching the given expression
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loc_expr_to_ids <- function(.data, expr, axis) {
  stopifnot(is.data.frame(.data))
  stopifnot(axis %in% c('row', 'column'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For rows, the column names will reference individual vectors of values
  # For columns, the column names will reference indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (axis == 'row') {
    labels <- rownames(.data)
    len    <- nrow(.data)
    env    <- list2env(.data, parent = parent.frame())
    gen_n  <- gen_n_row
  } else {
    labels <- colnames(.data)
    len    <- ncol(.data)
    env    <- new.env(parent = parent.frame())
    gen_n  <- gen_n_col
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add name->index lookup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (all(labels == seq_len(len))) {
    # the rownames are all 1, 2, 3, ... nrow()
    # so don't add any lookup for name to index
  } else {
    for (i in seq_along(labels)) {
      assign(labels[i], i, envir = env)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an environment for evaluation which includes some standard
  # row helpers from the tidyverse/tidyselect.
  # This is a bit hackish, but works in a limited fashion.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  env$starts_with <- gen_starts_with(labels)
  env$ends_with   <- gen_ends_with  (labels)
  env$everything  <- gen_everything (labels)
  env$all_of      <- gen_all_of     (labels)
  env$any_of      <- gen_any_of     (labels)
  env$matches     <- gen_matches    (labels)
  env$contains    <- gen_matches    (labels)
  env$row_number  <- gen_row_number (.data)
  env$col_number  <- gen_col_number (.data)
  env$n           <- gen_n          (.data)
  env$all         <- function(...) TRUE
  env$.x          <- .data

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate the expression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ids  <- eval(expr, envir = env)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  Handle the following results
  #  - single logical -> all rows/cols if TRUE, otherwise none
  #  - logical vector of the exact length to match nrow/ncol
  #  - NULL value means all rows/cols
  #  - numeric result is interpreted as actual indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.logical(ids)) {
    if (length(ids) == 1) {
      ids <- rep(ids, len)
    } else if (length(ids) != len) {
      stop("logical result must be length 1 or ", len, ", not ", length(ids))
    }
    ids <- which(ids)
  } else if (is.null(ids)) {
    ids <- seq_len(len)
  } else if (is.character(ids)) {
    ids <- which(labels %in% ids)
  } else if (is.numeric(ids)) {
    ids <-  ids[ids >= 1 & ids <= len]
  } else {
    stop("Don't know how to handle an expression which evaluates to type: ",
         deparse(class(ids)))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # No NAs allowed in row/col ids
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (anyNA(ids)) {
    stop(axis, " specification must not contain NAs:", deparse(ids))
  }

  ids
}
