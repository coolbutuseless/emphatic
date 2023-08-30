
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Colour the differences between two strings
#'
#' Highlight the differences between two strings in
#' terms of substitutions, insertions and deletions.
#'
#' @param x,y each argument is a single string. vectors of strings not currently
#'        supported.
#' @param fs,bs,fi,bi,fd,bd the (f)oreground and (b)ackground colouring for
#'        string (s)ubstitutions, (i)insertions and (d)eletions.
#' @param ... further arguments passed to \code{adist()}
#'
#' @return list of 'emphatic' objects which could be renderd to ANSI or HTML
#'
#' @importFrom grDevices col2rgb
#' @importFrom utils adist
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_str_diff <- function(x, y,
                        bs = 'dodgerblue', bi = 'darkgreen', bd = 'firebrick',
                        fs = 'white'     , fi = 'white'    , fd = 'white',
                        ...) {

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


  rl <- base::rle(lev)
  N  <- length(rl$values)
  end <- cumsum(rl$lengths)
  begin <- c(0, head(end, -1) + 1)

  xv <- character(N)
  yv <- character(N)
  for (i in seq(N)) {
    xv[i] <- paste(xbits[begin[i]:end[i]], collapse = "")
    yv[i] <- paste(ybits[begin[i]:end[i]], collapse = "")
  }

  xtext <- c(S = fs, I = fi, D = fd, M = NA)[rl$values]
  xfill <- c(S = bs, I = bi, D = bd, M = NA)[rl$values]
  ytext <- c(S = fs, I = fi, D = fd, M = NA)[rl$values]
  yfill <- c(S = bs, I = bi, D = bd, M = NA)[rl$values]

  structure(
    list(
      structure(xv, text = t(as.matrix(xtext)), fill = t(as.matrix(xfill)), class = c("emphatic", "compact")),
      structure(yv, text = t(as.matrix(ytext)), fill = t(as.matrix(yfill)), class = c("emphatic", "compact"))
    ),
    class = "emphatic"
  )
}






