

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
#' @param x character string
#' @param pattern regular expression string. Note: don't get too fancy here
#' @param ... extra args passed to \code{gsub}
#' @param perl logical. use perl style regex. default: TRUE
#' @inheritParams coerce_to_string
#' @inheritParams hl_diff
#' @param opts create options list
#'
#' @importFrom utils capture.output str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep <- function(x,
                    pattern,
                    coerce = "default",
                    opts = hl_opts(),
                    fg = NULL,
                    bg = NULL,
                    ..., perl = TRUE) {

  x <- coerce_to_string(x, coerce)

  if (length(x) > 1) {
    x <- deparse(x)
  }

  if (is.null(fg)) {
    fg <- ifelse(opts$dark_mode, "black", "yellow")
  }
  if (is.null(bg)) {
    bg <- ifelse(opts$dark_mode, "yellow", "black")
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

  attr(res, 'options') <- opts


  res
}



