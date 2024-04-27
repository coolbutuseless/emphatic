


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
#' @param bg,fg named list of colours for substitutions, insertions and
#'        deletions with names 'sub', 'ins' and 'del'.  If set to NULL (the
#'        default) then colours will be chosen automatically depending on the
#'        \code{dark_mode} argument
#' @param ... further arguments passed to \code{adist()}
#' @inheritParams coerce_to_string
#' @param sep what to output on the line separating the two objects. Default: NULL
#'        for no separation. Use the empty string to insert an empty line.
#' @inheritParams hl_grep
#'
#' @return list of 'emphatic' objects which could be rendered to ANSI or HTML
#'
#' @importFrom grDevices col2rgb
#' @importFrom utils adist
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hl_diff <- function(x, y,
                    coerce = "default",
                    bg = NULL, fg = NULL,
                    opts = hl_opts(),
                    sep = NULL,
                    ...) {


  if (is.null(bg)) {
    if (opts$dark_mode) {
      bg <- list(sub = 'dodgerblue', ins = 'darkgreen', del = 'firebrick')
    } else {
      bg <- list(sub = 'dodgerblue1', ins = 'darkgreen', del = 'firebrick3')
    }
  }
  if (is.null(fg)) {
    if (opts$dark_mode) {
      fg <- list(sub = 'white', ins = 'white', del = 'white')
    } else {
      fg <- list(sub = 'black', ins = 'black', del = 'black')
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coerce
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (!is.character(x)) {
    x <- coerce_to_string(x, coerce)
  # } else {
  #   x <- capture.output(x)
  #   x <- paste(x, collapse = "\n")
  # }

  # if (!is.character(y)) {
    y <- coerce_to_string(y, coerce)
  # } else {
  #   y <- capture.output(y)
  #   y <- paste(y, collapse = "\n")
  # }

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

  xtext <- c(S = fg$sub, I = fg$ins, D = fg$del, M = NA)[rl$values]
  xfill <- c(S = bg$sub, I = bg$ins, D = bg$del, M = NA)[rl$values]
  ytext <- c(S = fg$sub, I = fg$ins, D = fg$del, M = NA)[rl$values]
  yfill <- c(S = bg$sub, I = bg$ins, D = bg$del, M = NA)[rl$values]


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


if (FALSE) {
  x <- "abcdx"
  y <- "abcd\nef"
  lev <- utils::adist(x, y, counts = TRUE)
  hl_diff(x, y)

  coerce = "default"
  bg = NULL
  fg = NULL
  opts = hl_opts()

  hl_diff(head(mtcars, 2), head(mtcars, 3), sep = " ")


  x <- "hi\nhi aa"
  y <- "hi\nhi bb"
  lev <- utils::adist(x, y, counts = TRUE)
  hl_diff(x, y, sep = "----------------")
}



