
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour the differences between character representations of objects
#'
#' Highlight the differences between two strings in
#' terms of substitutions, insertions and deletions calculated by
#' the generalized Levenshtein (edit) distance (using \code{adist()})
#'
#' This works character-by-character, so the displayed difference for multiline
#' strings can be quite busy if there are a lot of changes.
#'
#' @param x,y each argument is a single string. vectors of strings not currently
#'        supported.
#' @param fill named list of colours for substitutions, insertions and
#'        deletions with names 'sub', 'ins' and 'del'.  If set to NULL (the
#'        default) then default colours will be used.
#' @param text named list of colours for the text for 'sub', 'ins' and 'del'
#'        operations. If \code{NULL}, then colours which contrast with \code{fill} will
#'        be chosen automatically
#' @param ... further arguments passed to \code{adist()}
#' @param sep character string of the  line separating the two objects. Default: \code{NULL}
#'        for no separation. Use the empty string to insert an empty line.
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
#' @inheritParams hl_grep
#'
#' @return list of 'emphatic' objects which could be rendered to ANSI (for example)
#'
#' @importFrom grDevices col2rgb
#' @importFrom utils adist
#' @importFrom utils head
#' @export
#' @examples
#' hl_diff('hello', 'there')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_diff <- function(x, y,
                    coerce = "default",
                    fill   = NULL,
                    text   = NULL,
                    opts   = hl_opts(),
                    sep   = NULL,
                    ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Default colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_default_dark  <- list(
    sub = getOption("HL_SUB_COL", 'dodgerblue') ,
    ins = getOption("HL_INS_COL", 'darkgreen' ) ,
    del = getOption("HL_DEL_COL", 'firebrick' )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Work hard to ensure we have a full complement of colours for both
  # 'fill' and 'text'.  and 'text' colours are chosen as contrasting if
  # they are not specified
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill <- modify_list(fill_default_dark, fill)

  if (is.null(text)) {
    text <- list(
      sub = calc_contrasting_text(fill$sub, text_contrast = opts$text_contrast),
      ins = calc_contrasting_text(fill$ins, text_contrast = opts$text_contrast),
      del = calc_contrasting_text(fill$del, text_contrast = opts$text_contrast)
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coerce
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- coerce_to_string(x, coerce)
  y <- coerce_to_string(y, coerce)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(
    is.character(x),
    is.character(y),
    !is.na(x),
    !is.na(y),
    length(x) == 1,
    length(y) == 1
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the levenshtein distance and the transformation sequence
  # to turn the first string into the second.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lev <- utils::adist(x, y, counts = TRUE, ...)
  lev <- attr(lev, 'trafos')[1]
  lev <- strsplit(lev, '')[[1]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adjust input strings to account for deletions and insertions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xbits <- strsplit(x  , '')[[1]]
  ybits <- strsplit(y  , '')[[1]]
  dels <- which(lev == 'D')
  for (del in dels) {
    if (del == 1) {
      ybits <- c(' ', ybits)
    } else if (del > length(ybits)) {
      ybits <- c(ybits, ' ')
    } else {
      lower <- seq(1, (del - 1))
      upper <- seq(del, length(ybits))
      ybits <- c(ybits[lower], ' ', ybits[upper])
    }
  }

  inserts <- which(lev == 'I')
  for (ins in inserts) {
    if (ins == 1) {
      xbits <- c(' ', xbits)
    } else if (ins > length(xbits)) {
      xbits <- c(xbits, ' ')
    } else {
      lower <- seq(1, (ins - 1))
      upper <- seq(ins, length(xbits))
      xbits <- c(xbits[lower], ' ', xbits[upper])
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # copy across "\n"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xcr <- endsWith(xbits, '\n')
  ycr <- endsWith(ybits, '\n')
  xcr2 <- xcr & !ycr
  ycr2 <- ycr & !xcr

  xbits <- ifelse(ycr2, paste0(xbits, "\n"), xbits)
  ybits <- ifelse(xcr2, paste0(ybits, "\n"), ybits)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the beginning and end of each run of the same edit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rl    <- base::rle(lev)
  N     <- length(rl$values)
  end   <- cumsum(rl$lengths)
  begin <- c(0, head(end, -1) + 1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Break apart string into these "same edit operation" chunks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xv <- character(N)
  yv <- character(N)
  for (i in seq(N)) {
    xv[i] <- paste(xbits[begin[i]:end[i]], collapse = "")
    yv[i] <- paste(ybits[begin[i]:end[i]], collapse = "")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Construct a vector of colours for each chunk
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xtext <- c(S = text$sub, I = text$ins, D = text$del, M = NA)[rl$values]
  xfill <- c(S = fill$sub, I = fill$ins, D = fill$del, M = NA)[rl$values]
  ytext <- c(S = text$sub, I = text$ins, D = text$del, M = NA)[rl$values]
  yfill <- c(S = fill$sub, I = fill$ins, D = fill$del, M = NA)[rl$values]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the 'emphatic' object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(sep)) {
    structure(
      list(
        structure(xv, text = t(as.matrix(xtext)), fill = t(as.matrix(xfill)), class = c("emphatic", "compact"), options = opts),
        structure(yv, text = t(as.matrix(ytext)), fill = t(as.matrix(yfill)), class = c("emphatic", "compact"), options = opts)
      ),
      class = "emphatic"
    )
  } else {
    if (nchar(sep) == 0) sep <- " "
    sept <- NA_character_
    structure(
      list(
        structure(xv , text = t(as.matrix(xtext)), fill = t(as.matrix(xfill)), class = c("emphatic", "compact"), options = opts),
        structure(sep, text = t(as.matrix (sept)), fill = t(as.matrix (sept)), class = c("emphatic", "compact"), options = opts),
        structure(yv , text = t(as.matrix(ytext)), fill = t(as.matrix(yfill)), class = c("emphatic", "compact"), options = opts)
      ),
      class = "emphatic"
    )
  }

}

