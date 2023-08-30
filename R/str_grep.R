

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour highlighting a regular expression search
#'
#' @description
#' Highlight text within an R object which matches a given regex. This
#' only works in a terminal which supports ANSI colour codes.
#'
#' There are slightly different versions of the highlighting function depending
#' upon which text version of the object you'd like to match against:
#'
#' \describe{
#'   \item{hl_grep}{ - the given object \code{x} must already be a character string}
#'   \item{hl_grep_character}{ - performs the matching after first calling
#'           \code{as.character(x)}}
#'   \item{hl_grep_print}{ - performs the matching against the default
#'            \code{print(x)} output}
#'   \item{hl_grep_deparse}{ - performs the matching after first calling
#'           \code{deparse1(x)}}
#'   \item{hl_grep_str}{ - performs the matching on the output of calling
#'           \code{str(x)}}
#' }
#'
#' @param x character string
#' @param pattern regular expression string. Note: don't get too fancy here
#' @param fg,bg any valid R colour specification e.g. 'hotpink', '#335588'
#' @param ... extra args passed to \code{gsub}
#' @param perl logical. use perl style regex. default: TRUE
#'
#' @importFrom utils capture.output str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep <- function(x, pattern, fg = 'black', bg = 'yellow', ..., perl = TRUE) {

  stopifnot(is.character(x))

  if (length(x) > 1) {
    x <- deparse(x)
  }

  # matches <- gregexpr(pattern, x)[[1]]
  matches <- gregexpr(pattern, x, ..., perl = perl)[[1]]
  matches

  match_starts <- matches; attributes(match_starts) <- NULL
  match_ends   <- match_starts + attr(matches, "match.length") - 1

  if (match_starts[1] == -1) {
    return(x) # no matches found
  }

  # add some fake matches outside the string.
  match_starts <- c(-Inf, match_starts, nchar(x) + 1)
  match_ends   <- c(   0, match_ends  , Inf)

  # Expand to include non-matching sections
  starts <- c()
  ends   <- c()

  for (i in seq_len(length(match_starts) - 1)) {
    starts <- c(starts, match_starts[i])
    ends   <- c(ends  , match_ends[i])

    starts <- c(starts, match_ends[i] + 1)
    ends   <- c(ends  , match_starts[i + 1] - 1)
  }

  # Drop the first segment which is known to be out of bounds
  starts <- starts[-1] # tail(starts, -1)
  ends   <- ends  [-1] # tail(ends  , -1)

  # Drop any redundant segments
  keep   <- ends >= starts
  starts <- starts[keep]
  ends   <- ends[keep]

  # Is this segment a match to be highlighted?
  is_match <- starts %in% matches


  bits  <- character(length(match_starts))

  for (i in seq_along(starts)) {
    bits[i] <- substr(x, starts[i], ends[i])
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine text colour and fill for each segment
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text <- ifelse(is_match, fg, NA_character_)
  fill <- ifelse(is_match, bg, NA_character_)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build emphatic structure:  raw vector + text + fill
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- structure(
    bits,
    class = c('emphatic', 'compact'),
    text = t(as.matrix(text)), fill = t(as.matrix(fill))
  )

  res
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hl_grep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep_character <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(cat(as.character(x)))
  x <- paste(x, collapse = "\n")
  hl_grep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hl_grep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep_print <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(x)
  x <- paste(x, collapse = "\n")
  hl_grep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hl_grep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep_deparse <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- deparse1(x)
  hl_grep(x, pattern, fg, bg, ...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname hl_grep
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep_str <- function(x, pattern, fg = 'black', bg = 'yellow', ...) {
  x <- capture.output(str(x, vec.len = 200))
  x <- paste(x, collapse = "\n")
  hl_grep(x, pattern, fg, bg, ...)
}
