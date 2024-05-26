
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write an emphatic data.frame to an Excel workbook
#'
#' Requires \code{openxlsx} package
#'
#' @param x emphatic data.frame object
#' @param xlsx_filename xlsx filename
#' @param colNames Display column names? logical. Default: TRUE
#' @param opts rendering options
#'
#' @return None
#' @export
#' @examples
#' mtcars |>
#'    hl('blue') |>
#'    write_xlsx(tempfile())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_xlsx <- function(x, xlsx_filename, colNames = TRUE, opts = hl_opts()) {

  stopifnot(is_emphatic(x))
  if (!is.data.frame(x)) {
    stop("Only works on emphatic data.frames")
  }

  rowNames = !is.integer(attr(x, 'row.names'))

  if (requireNamespace('openxlsx', quietly = TRUE)) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 1. Create a workbook
    # 2. Add a worksheet
    # 3. Add data
    # 4. Colour data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sheet_name <- "emphatic"
    wb <- openxlsx::createWorkbook("wb")
    openxlsx::addWorksheet(wb, sheet_name, gridLines = TRUE)
    openxlsx::writeData(wb, sheet = 1, x, colNames = TRUE, rowNames = rowNames)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Extract the colour matrices from the emphatic object and convert
    # to hex strings
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fill <- attr(x, 'fill')
    text <- attr(x, 'text')

    if (opts$text_mode == 'remove') {
      opts$text_mode     <- 'contrast'
      opts$text_contrast <- 0
    }


    fill[] <- col2hex(fill)
    if (opts$text_mode == 'contrast') {
      text[] <- ifelse(
        is.na(text),
        calc_contrasting_text(fill, text_contrast = opts$text_contrast),
        col2hex(text)
      )
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a new style for each cell to colour it correctly
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (row in seq(nrow(fill))) {
      for (col in seq(ncol(fill))) {

        fontColour <- text[row, col]
        if (is.na(fontColour)) {
          fontColour <- NULL
        }
        cell_style <- openxlsx::createStyle(
          fgFill     = fill[row, col],
          fontColour = fontColour
        )
        openxlsx::addStyle(wb, sheet = 1, cell_style, rows = row + 1,
                           cols = col + isTRUE(rowNames), gridExpand = FALSE)
      }
    }

    openxlsx::saveWorkbook(wb, xlsx_filename, overwrite = TRUE)
  } else {
    stop("write_xlsx(): Need to have `openxlsx` installed")
  }

  invisible(xlsx_filename)
}
