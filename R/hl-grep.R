
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
#' @param fill solid colour for background.  If \code{NULL} (the default),
#'        then the default colour will be selected
#' @param text text colour. If \code{NULL} (the default), then a colour
#'        will be seleted which contrasts with the \code{fill} colour.
#' @param ... extra args passed to \code{gsub}
#' @param perl logical. use perl style regex. default: TRUE
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
#' @param opts create options list
#'
#' @return An emphatic object suitable to output to console (for example)
#'
#' @importFrom utils capture.output str
#' @export
#' @examples
#' hl_grep(mode, 'switch')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_grep <- function(x,
                    pattern,
                    coerce = "default",
                    opts = hl_opts(),
                    fill = NULL,
                    text = NULL,
                    ..., perl = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Choose colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(fill)) {
    fill <- getOption("HL_GREP_COL", "#0F19F0")
  }
  if (is.null(text)) {
    text <- calc_contrasting_text(
      fill,
      text_contrast = opts$text_contrast
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coerge to string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- coerce_to_string(x, coerce)
  if (length(x) > 1) {
    x <- deparse(x)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Where are the matches? Where do they start and finish?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  matches      <- gregexpr(pattern, x, ..., perl = perl)[[1]]
  match_starts <- matches; attributes(match_starts) <- NULL
  match_ends   <- match_starts + attr(matches, "match.length") - 1

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Early exit if there are no matches for this string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (match_starts[1] == -1) {
    return(x) # no matches found
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # add some dummy matches outside the string to make the logic for
  # colouring easier
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  starts <- starts[-1]
  ends   <- ends  [-1]

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
  text_grep <- ifelse(is_match, text, NA_character_)
  fill_grep <- ifelse(is_match, fill, NA_character_)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build emphatic structure:  raw vector + text + fill
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- structure(
    bits,
    class = c('emphatic', 'compact'),
    text = t(as.matrix(text_grep)), fill = t(as.matrix(fill_grep))
  )

  attr(res, 'options') <- opts
  res
}
