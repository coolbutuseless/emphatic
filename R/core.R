

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a matrix or data.frame to an emphatic version
#'
#' This usually does not need to be called explicitly by the user.
#'
#' The function adds the attributes necessary for keeping track of the
#' colours assigned to each cell.  This consists of 2 character matrices - one for
#' the text colour and one for the background colour.
#'
#' Colour information is stored as R colour names (e.g. 'red') or 6 character
#' hex colours (e.g. '#ff0000').
#'
#' @param .data \code{emphatic} matrix or data.frame
#'
#' @return .data with added attributes for text and fill colours
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_emphatic <- function(.data) {
  stopifnot(is.matrix(.data) || is.data.frame(.data))

  .data <- set_colour_matrix(.data,  'text', matrix(NA_character_, nrow(.data), ncol(.data)))
  .data <- set_colour_matrix(.data,  'fill', matrix(NA_character_, nrow(.data), ncol(.data)))

  class(.data) <- unique(c('emphatic', class(.data)))
  .data
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Private function for fetching the colour matrix for the given element type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_colour_matrix <- function(.data, elem) {
  stopifnot(elem %in% c('fill', 'text'))
  attr(.data, elem, exact = TRUE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Private function for setting the colour matrix for the given element type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_colour_matrix <- function(.data, elem, mat) {
  stopifnot(elem %in% c('fill', 'text'))
  stopifnot(is.matrix(mat))
  stopifnot(is.character(mat))
  stopifnot(identical(dim(mat), dim(.data)))
  attr(.data, elem) <- mat
  .data
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' check if data.frame is a emphatic data.frame of matrix
#'
#' @param x Object to test
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_emphatic <- function(x) {
  text <- get_colour_matrix(x, 'text')
  fill <- get_colour_matrix(x, 'fill')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # I'm being *very* paranoid here as this package is mutating at a very
  # fast rate, and I want to be certain
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inherits(x, 'emphatic') &&
    (is.matrix(x) || is.data.frame(x)) &&
    !is.null(text) &&
    !is.null(fill) &&
    identical(dim(x), dim(text)) &&
    identical(dim(x), dim(fill)) &&
    is.matrix(text) &&
    is.matrix(fill) &&
    is.character(text) &&
    is.character(fill)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print an \code{emphatic} data.frame or matrix
#'
#' The printed output contains ANSI escape codes to colour the elements in the
#' data.frame or matrix using the colour information for each cell stored
#' in the attributes.
#'
#' @inheritParams hl_opt_global
#' @inheritParams as.character.emphatic
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.emphatic <- function(x, ...) {

  res <- as.character.emphatic(x, ...)

  cat(res)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an \code{emphatic} data.frame or matrix to a string.
#'
#'
#' The output contains ANSI escape codes to colour the elements in the
#' data.frame or matrix. This string would then be suitable to pass on to \code{fansi}
#' for further manipulation e.g. conversion to HTML for displaying in a vignette.
#'
#' @param x emphatic data.frame or matrix
#' @param ... other arguments passed on to \code{format()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.emphatic <- function(x, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is_emphatic(x))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build full options by combining global and local options
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  global_opt <- getOption('emphatic', default = list())
  local_opt  <- attr(x, 'options', exact = TRUE) %||% list()
  opt        <- modifyList(default_options, global_opt)
  opt        <- modifyList(opt, local_opt)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # format the data.frame as character matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat <- as.matrix(format(x, ...))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set the NA to whatever the user requests
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[is.na(x)] <- opt$na

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If rownames or colnames not present, manually create them.
  #  Just of the format "[1,]" for rows and "[,1]" for columns
  # For data.frames, there will always be columnnames, and if rownames are
  # missing they're Just a left-aligned integer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.data.frame(x) && is.null(rownames(x))) {
    new_rownames <- as.character(seq_len(nrow(x)))
    max_nchar <- max(nchar(new_rownames))
    fmt <- paste0("-%", max_nchar, 's')
    new_rownames <- sprintf(fmt, new_rownames)
    rownames(mat)  <- new_rownames
  } else if (is.matrix(x)) {
    if (is.null(colnames(x))) {
      new_colnames <- paste0("[,", seq_len(ncol(x)), "]")
      colnames(mat) <- new_colnames
    }

    if (is.null(rownames(x))) {
      new_rownames <- paste0("[", seq_len(nrow(x)), ",]")
      max_nchar <- max(nchar(new_rownames))
      fmt <- paste0("%", max_nchar, 's')
      new_rownames <- sprintf(fmt, new_rownames)
      rownames(mat)  <- new_rownames
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Print the character matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- as_character_inner(
    mat,
    text             = get_colour_matrix(x, 'text'),
    fill             = get_colour_matrix(x, 'fill'),
    text_mode        = opt$text_mode,
    text_contrast    = opt$text_contrast,
    full_colour      = opt$full_colour,
    dark_mode        = opt$dark_mode,
    underline_header = opt$underline_header
  )


  res
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Private function for converting an emphatic data.frame or matrix to a string
#'
#' @param m character matrix
#' @param text,fill matrices of colours to apply to each given cell.
#'        Dimensions must match that of \code{m}
#' @inheritParams hl_opt_global
#'
#' @return NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_character_inner <- function(m,
                               text, fill,
                               text_mode        = 'contrast',
                               text_contrast    = 1,
                               full_colour      = FALSE,
                               dark_mode        = TRUE,
                               underline_header = TRUE) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix form assumed to be character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(m))
  stopifnot(identical(dim(m), dim(text)))
  stopifnot(identical(dim(m), dim(fill)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # After each cell we will add the ansi RESET code to revert
  # text and fill attributes to the terminal default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- matrix(reset_code, nrow = nrow(m), ncol = ncol(m))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Automatic constrasting text for foreground?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (text_mode == 'contrast') {
    new_text <- calc_contrasting_text(fill, text_contrast = text_contrast, dark_mode = dark_mode)
    text[] <- ifelse(is.na(text) | text == '', new_text, text)
  } else if (text_mode == 'asis') {
    # do nothing. use the default text colour for the output
  } else if (text_mode == 'remove') {
    m[is.na(text) | text == ''] <- ''
  } else {
    stop("print_emphatic_inner(): text_mode not understood: ", deparse(text_mode))
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert matrices of R colours to matrices of ANSI codes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(full_colour)) {
    text[] <- col2fg24(text)
    fill[] <- col2bg24(fill)
  } else {
    text[] <- col2fg(text)
    fill[] <- col2bg(fill)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine the full width for each column
  #
  # Pad every column in 'm' to correct width that encompasses the
  # width of all values as well as the width of the column header.
  #
  # Todo for matrix inputs without names, might need to dummy up
  # a width for the actual printed representation which is something like
  # " [ ,1]  [ ,2] ... "
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  col_widths <- apply(m, 2, function(x) {max(nchar(x))})
  col_widths <- pmax(col_widths, nchar(colnames(m)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do the padding. Pad both the contents and the column heading
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  col_names <- colnames(m)

  for (i in seq_along(col_widths)) {
    width <- col_widths[i]
    fmt   <- sprintf(" %%%is", width)
    m[,i] <- sprintf(fmt, m[,i])
    fmt   <- sprintf("%%%is", width)
    col_names[i] <- sprintf(fmt, col_names[i])
  }


  final <- paste0(text, fill, m, end)
  final <- matrix(final, nrow = nrow(text), ncol = ncol(text))

  if (!is.null(rownames(m)) && !is.null(colnames(m))) {
    this_rownames <- rownames(m)
    max_nchar <- max(nchar(this_rownames))
    fmt <- paste0("%-", max_nchar + 1, "s ")
    this_rownames <- sprintf(fmt, this_rownames)
    final <- cbind(this_rownames, final)

    col_names <- c(sprintf(fmt, ''), col_names)
  } else {
    col_names <- c('', col_names)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble single text string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  header <- paste(col_names, collapse = " ")
  if (isTRUE(underline_header)) {
    header <- paste0(underline_on_code, header, underline_off_code)
  }
  body   <- apply(final, 1, paste, collapse = '')
  res    <- paste(c(header, body), collapse = "\n")
  res    <- paste0(res, "\n")


  res
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate a contrasting text colours for vector of fill colours
#'
#' The function calculates the luma of each fill colour and then picks either
#' the text colour to be either white or black in order to contrast with it.
#'
#' There is a further call to 'interp_colour' in order to control the contrast.
#'
#' @param fill vector of background colours
#' @inheritParams  hl_opt_global
#'
#' @return contrasting text colour for readable text
#'
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_contrasting_text <- function(fill, text_contrast, dark_mode) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # There will be elements where the user has not set a fill colour.
  # Keep track of them
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_not_set <- is.na(fill) | fill == ''

  if (dark_mode) {
    fill[fill_not_set]  <- 'black'
  } else {
    fill[fill_not_set]  <- 'white'
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert fill colour to matrix representation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgb <- col2rgb(fill)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the perceptive luminance (aka luma) - human eye favors green color...
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  luma <- ((0.299 * rgb[1,]) + (0.587 * rgb[2,]) + (0.114 * rgb[3,])) / 255;

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Contrasting colour is black for bright colors, white for dark colors
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  contrast_colour <- ifelse(luma > 0.5, 'black', 'white')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User-specified 'text_contrast' level is used to soften the contrast
  # i.e. if text_contrast = 0, then final text colour should be the same
  # as the 'fill' colour in order to be nearly invisible.  If text_contrast
  # is 1, then text colour should be fully black or white.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  contrast_colour <- interp_colour(fill, contrast_colour, text_contrast)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If there wasn't an actual fill colour set, then don't set a text colour.
  # i.e. this will use the default console colouring
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # text[fill_not_set] <- ''

  contrast_colour
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Interpolate between 2 colour vectors
#'
#' @param colour1,colour2 character vectors of R colours
#' @param frac fraction linear interpolation between the two
#'
#' @importFrom grDevices col2rgb convertColor rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interp_colour <- function(colour1, colour2, frac) {

  stopifnot(length(frac) == 1)
  stopifnot(length(colour1) == length(colour2))

  if (frac == 1) {
    return(colour2)
  } else if (frac == 0) {
    return(colour1)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert colours to matrices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  m1 <- t(col2rgb(colour1)/255)
  m2 <- t(col2rgb(colour2)/255)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to Lab colour space for better interpolation results
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  convertColor(m1, from = 'sRGB', to = 'Lab')
  convertColor(m2, from = 'sRGB', to = 'Lab')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Interpolate
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  m <- m1 + frac * (m2 - m1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert back to RGB colour space and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  convertColor(m, from = 'Lab', to = 'sRGB')

  rgb(m[,1], m[,2], m[,3])
}








