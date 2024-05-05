
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reset back to terminal defaults
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reset_ansi         <- "\033[39m\033[49m"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Turn Underline on/off
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
underline_on_ansi  <- "\033[4m"
underline_off_ansi <- "\033[24m"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an R colour to a 216-colour ANSI string. Suitable for most terminals
#' including Rstudio terminal
#'
#' Ref: \url{https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit}
#'
#' If all the RGB colour components are equal then the colour is matched to one
#' of 24 grey levels, otherwise it is converted to one of 216 standard ANSI colours.
#'
#' @param rcolour any R colour e.g. 'red', '#445566'
#'
#' @return ANSI escape string for the given colour as a foreground or background
#'         colour
#'
#' @importFrom grDevices col2rgb
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_ansi <- function(rcolour) {
  rcolour[rcolour == ''] <- NA_character_
  ifelse(
    is.na(rcolour),
    '',
    paste0("\033[48;5;", col2code_ansi(rcolour), "m")
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname col2fill_ansi
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text_ansi <- function(rcolour) {
  rcolour[rcolour == ''] <- NA_character_
  ifelse(
    is.na(rcolour),
    '',
    paste0("\033[38;5;", col2code_ansi(rcolour), "m")
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname col2fill_ansi
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2code_ansi <- function(rcolour) {
  cols <- grDevices::col2rgb(rcolour)

  is_grey        <- cols[1,] == cols[2,] & cols[2,] == cols[3,]
  possibly_white <- cols[1,] == 255L

  grey_code <- 232L + as.integer(round(cols[1,]/255 * 23))

  cols <- round(cols/255 * 5)
  colour_code <- 16L + 36L * cols[1,] + 6 * cols[2,] + cols[3,]

  ifelse(is_grey & !possibly_white, grey_code, colour_code)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an R colour to an ansi string for 24bit colour supported by some terminals
#'
#' Ref: \url{https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit}
#'
#' @param rcolour any R colour e.g. 'red', '#445566'.  If rcolour is NA or empty
#'        string then return an empty string
#'
#' @return ANSI escape string for the given colour as a foreground or background
#'         colour
#'
#' @importFrom grDevices col2rgb
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_ansi24 <- function(rcolour) {
  rcolour[rcolour == ''] <- NA_character_
  ifelse(
    is.na(rcolour),
    '',
    {
      cols <- grDevices::col2rgb(rcolour)
      paste0("\033[48;2;", cols[1,], ";", cols[2,], ";", cols[3,], "m")
    }
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname col2fill_ansi24
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text_ansi24 <- function(rcolour) {
  rcolour[rcolour == ''] <- NA_character_
  ifelse(
    is.na(rcolour),
    '',
    {
      cols <- grDevices::col2rgb(rcolour)
      paste0("\033[38;2;", cols[1,], ";", cols[2,], ";", cols[3,], "m")
    }
  )
}

