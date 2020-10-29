
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2bg24 <- function(rcolour) {
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
#' @rdname col2bg24
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fg24 <- function(rcolour) {
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
