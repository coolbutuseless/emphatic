
backends <- c('ansi', 'html', 'latex', 'typst')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How should the raw text be escaped in order to handle special characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape <- list(
  ansi  = identity,
  html  = escape_html,
  latex = escape_latex,
  typst = escape_typst
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If multiple outputs are being collapsed together, what should they
# be collapsed with
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collapse = list(
  ansi  = "\n",
  html  = "<br/>",
  latex = "\\\\\n",
  typst = "\\\n"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How does the current block of text get set back to the default
# e.g. closing all spans, ending all subcommands
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset = list(
  ansi  = reset_ansi,
  html  = reset_html,
  latex = reset_latex,
  typst = reset_typst
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colouring of foreground (text)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text <- list(
  ansi   = col2text_ansi,
  ansi24 = col2text_ansi24,
  html   = col2text_html,
  latex  = col2text_latex,
  typst  = col2text_typst
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Colouring of background (fill)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill <- list(
  ansi   = col2fill_ansi,
  ansi24 = col2fill_ansi24,
  html   = col2fill_html,
  latex  = col2fill_latex,
  typst  = col2fill_typst
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Underline markup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
underline_on <- list(
  ansi  = underline_on_ansi,
  html  = underline_on_html,
  latex = underline_on_latex,
  typst = underline_on_typst
)

underline_off <- list(
  ansi  = underline_off_ansi,
  html  = underline_off_html,
  latex = underline_off_latex,
  typst = underline_off_typst
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a data.frame, matrix or atomic vector into an emphatic version
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
#' @param .data data.frame, matrix or atomic vector
#'
#' @return An \code{emphatic} version of the given .data with added attributes for text and fill colours
#'
#' @export
#' @examples
#' mtcars |>
#'   head() |>
#'   as_emphatic()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_emphatic <- function(.data) {
  stopifnot(is_matrix(.data) || is.data.frame(.data) || is_atomic(.data))

  if (is_atomic(.data)) {
    .data <- set_colour_matrix(.data,  'text', matrix(NA_character_, 1, length(.data)))
    .data <- set_colour_matrix(.data,  'fill', matrix(NA_character_, 1, length(.data)))
  } else {
    .data <- set_colour_matrix(.data,  'text', matrix(NA_character_, nrow(.data), ncol(.data)))
    .data <- set_colour_matrix(.data,  'fill', matrix(NA_character_, nrow(.data), ncol(.data)))
  }

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
  stopifnot(is_matrix(mat))
  stopifnot(is.character(mat))
  stopifnot((is_atomic(.data) && length(.data) == length(mat)) || identical(dim(mat), dim(.data)))
  attr(.data, elem) <- mat
  .data
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_legend <- function(.data, legend) {
  attr(.data, 'legends') <- c(attr(.data, 'legends', exact = TRUE), list(legend))
  .data
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_legends <- function(.data) {
  attr(.data, 'legends', exact = TRUE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check if data.frame, matrix or atomic vector is a valid emphatic version
#'
#' @param x Object to test
#'
#' @return Logical value
#' @export
#' @examples
#' mtcars |>
#'   hl('red') |>
#'   is_emphatic()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_emphatic <- function(x) {

  if (is.list(x) && inherits(x, 'emphatic')) {
    return(TRUE);
  }

  text <- get_colour_matrix(x, 'text')
  fill <- get_colour_matrix(x, 'fill')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # I'm being *very* paranoid here as this package is mutating at a very
  # fast rate, and I want to be certain
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inherits(x, 'emphatic') &&
    (is_matrix(x) || is.data.frame(x) || is_atomic(x)) &&
    !is.null(text) &&
    !is.null(fill) &&
    ((is_atomic(x) && length(x) == length(text)) || identical(dim(x), dim(text))) &&
    ((is_atomic(x) && length(x) == length(fill)) || identical(dim(x), dim(fill))) &&
    is_matrix(text) &&
    is_matrix(fill) &&
    is.character(text) &&
    is.character(fill)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print an \code{emphatic} data.frame, matrix or atomic vector
#'
#' @inheritParams hl_opts
#' @inheritParams as.character.emphatic
#'
#' @return None.
#' @export
#' @examples
#' mtcars |>
#'   head() |>
#'   hl('red') |>
#'   print()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.emphatic <- function(x, ...) {

  res <- as.character.emphatic(x, ...)

  cat(res)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an \code{emphatic} data.frame, matrix or atomic vector into a character string.
#'
#' The output contains ANSI escape codes to colour the elements in the
#' object. This string would then be suitable to pass on to \code{fansi}
#' for further manipulation e.g. conversion to HTML for displaying in a vignette.
#'
#' @param x \code{emphatic} data.frame, matrix or atomic vector
#' @param ... other arguments passed on to \code{format()}
#' @param mode Render mode 'ansi' (default) or 'html' determines how the colours will
#'        be represented in text. If you're in a terminal or console, then
#'        choose 'ansi'.
#'
#' @return A character string of the requested mode
#'
#' @export
#' @examples
#' mtcars |>
#'   as_emphatic() |>
#'   as.character()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.emphatic <- function(x, ..., mode = 'ansi') {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is_emphatic(x))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Lists of emphatic objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.list(x) && !is.data.frame(x)) {
    strs <- vapply(x, as.character, character(1), ..., mode = mode)
    return(paste(strs, collapse = collapse[[mode]]))
  }

  stopifnot(mode %in% backends)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove the 'emphatic' class here, so that if any subsequent operations
  # rely on 'as.character()' for this base class, they don't infinitely
  # call 'as.character.emphatic'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  class(x) <- setdiff(class(x), 'emphatic')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coerce tibbles into data.frames.
  # tibble:::print.tibble() already does some ANSI markup, and in doing so,
  # the results of calling "format()" are not the same as calling format()
  # on a data.frame.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(x, 'tbl_df')) {
    x <- as.data.frame(x)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build full options by combining global and local options
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  opt <- attr(x, 'options', exact = TRUE) %||% list()
  opt <- modify_list(hl_opts(), opt)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # format the data as character matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_atomic(x)) {
    fx <- x
    if ((!is.double(fx) && !is.integer(fx) && !is.raw(fx) && !is.complex(fx) && !is.logical(fx)) ||  inherits(fx, 'Date')) {
      if (!inherits(x, 'compact')) {
        fx <- dQuote(fx, FALSE)
      }
    }
    if (!inherits(x, 'compact')) {
      fx  <- format(fx)
    }
    mat <- matrix(fx, nrow = 1)
    if (!is.null(names(x))) {
      colnames(mat) <- names(x)
    } else if (!inherits(x, 'compact')) {
      mat[]  <- paste0(" ", mat) # need padding if no column names
    }
  } else {
    mat <- as.matrix(format(x, ...))
  }

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
  } else if (is_matrix(x)) {
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
    legends          = attr(x, 'legends', exact = TRUE),
    text_mode        = opt$text_mode,
    text_contrast    = opt$text_contrast,
    full_colour      = opt$full_colour,
    mode             = mode,
    atomic           = is_atomic(x),
    compact          = inherits(x, 'compact')
  )

  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Private function for converting an emphatic data.frame or matrix to a string
#'
#' @param m character matrix
#' @param text,fill matrices of colours to apply to each given cell.
#'        Dimensions must match that of \code{m}
#' @param legends any strings for legends to print. default NULL
#' @inheritParams hl_opts
#' @inheritParams as.character.emphatic
#' @param atomic was this originally an atomic vector?  if so use a slightly
#'        different printing method
#'
#' @return NULL
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_character_inner <- function(m,
                               text, fill, legends = NULL,
                               text_mode        = 'contrast',
                               text_contrast    = 1,
                               full_colour      = FALSE,
                               mode             = 'ansi',
                               atomic           = FALSE,
                               compact          = FALSE) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix form assumed to be character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(is.character(m))
  stopifnot((is_atomic(m) && length(m) == length(text)) || identical(dim(m), dim(text)))
  stopifnot((is_atomic(m) && length(m) == length(fill)) || identical(dim(m), dim(fill)))
  stopifnot(mode %in% backends)
  collapser <- collapse[[mode]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Automatic contrasting text for foreground?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (text_mode == 'contrast') {
    new_text <- calc_contrasting_text(fill, text_contrast = text_contrast)
    text[] <- ifelse(is.na(text) | text == '', new_text, text)
  } else if (text_mode == 'asis') {
    # do nothing. use the default text colour for the output
  } else if (text_mode == 'remove') {
    m[is.na(text) | text == ''] <- ''
  } else {
    stop("print_emphatic_inner(): text_mode not understood: ", deparse(text_mode))
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # After each cell we will add the ansi RESET code to revert
  # text and fill attributes to the terminal default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- matrix(reset[[mode]], nrow = nrow(m), ncol = ncol(m))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert matrices of R colours to matrices of ANSI codes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mode2 <- ifelse(mode == 'ansi' && isTRUE(full_colour), 'ansi24', mode)
  text[] <- col2text[[mode2]](text)
  fill[] <- col2fill[[mode2]](fill)


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
  # Do the padding. Pad both the contents and the column heading such that
  # the width of any item matches the maxiumum width of any item in that
  # column - including the column header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  col_names <- colnames(m)
  has_col_names <- !is.null(col_names)

  if (has_col_names) {
    for (i in seq_along(col_widths)) {
      width <- col_widths[i]
      fmt   <- sprintf(" %%%is", width)
      m[,i] <- sprintf(fmt, m[,i])
      fmt   <- sprintf("%%%is", width)
      col_names[i] <- sprintf(fmt, col_names[i])
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Escape text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  att <- attributes(m)
  m <- escape[[mode]](m)
  attributes(m) <- att

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse text markup, fill markup, actual contents and reset markup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ansi_mat <- paste0(text, fill, m, end)
  ansi_mat <- matrix(ansi_mat, nrow = nrow(text), ncol = ncol(text))


  if (atomic) {
    if (has_col_names) {
      col_names <- paste0(" ", col_names)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If the original scalar didn't have names, then we must print an idx
    # offset at the start of each row to match what R does.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    max_row_idx_digits <- nchar(as.character(ncol(ansi_mat)))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # What's the available width of the current terminal for printing
    # elements once the max_row_idx_digits are taken into account?
    # If there are column names then we don't need to make any adjustment
    # for width.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text_width <- (options('width')$width) %||% 80
    if (!has_col_names) {
      text_width <- text_width - (max_row_idx_digits + 2)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Given the width of the current terminal, how many element can I print
    # per line? And create a list of indices ('chunks') indicating the
    # values which get printed on each line.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    element_width <- nchar(m[1])
    n_per_line    <- floor(text_width/element_width)
    chunks        <- chunked_indices(ncol(ansi_mat), n_per_line)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate the row idx used to start each line if atomic vector
    # doesn't have names
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (has_col_names) {
      row_idx <- rep('', length(chunks))
    } else {
      fmt     <- paste0("[%", max_row_idx_digits, 'i] ')
      row_idx <- sprintf(fmt, (seq_along(chunks) - 1) * n_per_line + 1)
    }

    res <- c()
    for (i in seq_along(chunks)) {
      chunk_idx <- chunks[[i]]

      if (has_col_names) {
        header <- paste(col_names[chunk_idx], collapse = '')
        res    <- c(res, header)
      }

      new_row   <- paste(ansi_mat[chunk_idx], collapse = '')
      if (!compact) {
        # Add '[1]' in front of row
        new_row   <- paste0(row_idx[i], new_row)
      }
      res       <- c(res, new_row)
    }

    if (!compact) {
      res <- paste(res, collapse = collapser)
    } else {
      res <- paste0(res, collapse = '')
    }
  } else {

    if (!is.null(rownames(m)) && !is.null(colnames(m))) {
      this_rownames <- rownames(m)
      max_nchar     <- max(nchar(this_rownames))
      fmt           <- paste0("%-", max_nchar + 1, "s ")
      this_rownames <- sprintf(fmt, this_rownames)
      this_rownames <- escape[[mode]](this_rownames)
      if (mode == 'typst') {
        this_rownames <- paste0("`", this_rownames, "`")
      }
      ansi_mat      <- cbind(this_rownames, ansi_mat)
      col_names     <- c(sprintf(fmt, ''), col_names)
    } else {
      rownames(m) <- escape[[mode]](rownames(m))
       if (mode == 'typst') {
        rownames(m) <- paste0("`", rownames(m), "`")
      }
      col_names <- c('', col_names)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assemble single text string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!has_col_names) {
      header <- NULL
    } else {
      header <- paste(col_names, collapse = " ")
      header <- escape[[mode]](header)
      if (mode == 'typst') {
        header <- paste0('`', header, '`')
      }
      header <- paste0(underline_on[[mode]], header, underline_off[[mode]])
    }

    body   <- apply(ansi_mat, 1, paste, collapse = '')
    res    <- paste(c(header, body), collapse = collapser)
    res    <- paste0(res, collapser)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Legends down the bottom.
  # Generate legend from the legend specifications.
  # They need to be rendered here rather than at time of calling hl() as
  # options such as 'full_colour' need to be respected
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(legends)) {
    legend_texts <- vapply(legends, function(spec) {
      create_legend_string(
        scale       = spec$scale,
        values      = spec$values,
        label       = spec$label,
        full_colour = full_colour,
        mode        = mode
      )},
      character(1)
    )

    if (atomic) {
      legend_texts <- c('', legend_texts)
    }

    res <- paste(c(res, legend_texts), collapse = collapser)
  }


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
#' @inheritParams  hl_opts
#'
#' @return contrasting text colour for readable text
#'
#' @importFrom grDevices col2rgb
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_contrasting_text <- function(fill, text_contrast) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # There will be elements where the user has not set a fill colour.
  # Keep track of them
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill_not_set <- is.na(fill) | fill == ''

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
  if (text_contrast == 1) {
    contrast_colour[fill_not_set] <- ''
  }

  contrast_colour
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Interpolate between 2 colour vectors
#'
#' @param colour1,colour2 character vectors of R colours
#' @param frac fraction linear interpolation between the two
#'
#' @importFrom grDevices col2rgb convertColor rgb
#'
#' @return colour
#' @noRd
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
