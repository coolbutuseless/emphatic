
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a single line legend
#'
#' @param scale A colour scale object compatible with \code{ggplot2} e.g.
#'        \code{ggplot2::scale_colour_viridis_c()}
#' @param values Vector of values
#' @param label label name to prefix legend. default NULL
#' @inheritParams as_character_inner
#'
#' @return rendered text for legend
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_legend_string <- function(
  scale,
  values,
  label       = NULL,
  full_colour = FALSE,
  mode        = 'ansi'
) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(all(scale$aesthetics %in% c('colour', 'color', 'fill')))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prep and train the scale.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scale$reset()
  scale$train(values)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ask the scale for all the breaks it wants to display
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(scale, 'ScaleContinuous')) {
    key_vals <- scale$break_info()$minor_source
  } else if (inherits(scale, 'ScaleDiscrete')) {
    key_vals <- scale$get_breaks()
  }
  key_cols <- scale$map(key_vals)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare a character representation of the breaks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_vals <- format(key_vals)
  key_vals <- paste0(" ", key_vals)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble spaces to put ANSI text and fill codes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text <- rep(NA, length(key_vals))
  fill <- key_cols

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Insert label at front if provided
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(label)) {
    # stopifnot(length(label) == 1)
    if (length(label) > 1) {
      label <- "   "
    } else {
      label <- sprintf("%s: ", label)
    }
    key_vals <- c(label, key_vals)
    text <- c(NA, text)
    fill <- c(NA, key_cols)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always use contrasting text for the legend
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text <- calc_contrasting_text(fill, text_contrast = 1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # After each cell we will add the ansi RESET code to revert
  # text and fill attributes to the terminal default
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (mode == 'ansi') {
    end <- rep(reset_ansi, length(key_vals))
  } else if (mode == 'html') {
    end <- rep(reset_html, length(key_vals))
  } else if (mode == 'latex') {
    end <- rep(reset_latex, length(key_vals))
  } else if (mode == 'typst') {
    end <- rep(reset_typst, length(key_vals))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert matrices of R colours to matrices of ANSI codes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (mode == 'ansi') {
    if (isTRUE(full_colour)) {
      text[] <- col2text_ansi24(text)
      fill[] <- col2fill_ansi24(fill)
    } else {
      text[] <- col2text_ansi(text)
      fill[] <- col2fill_ansi(fill)
    }
  } else if (mode == 'html') {
    text[] <- col2text_html(text)
    fill[] <- col2fill_html(fill)
  } else if (mode == 'latex') {
    text[] <- col2text_latex(text)
    fill[] <- col2fill_latex(fill)
  } else if (mode == 'typst') {
    text[] <- col2text_typst(text)
    fill[] <- col2fill_typst(fill)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ansi_vec <- paste0(text, fill, key_vals, end)

  paste(ansi_vec, collapse = '')
}
