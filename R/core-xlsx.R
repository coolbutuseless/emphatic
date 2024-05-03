
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert emphatic to Excel workbook
#'
#' @param x emphatic object
#' @param xlsx_filename xlsx filename
#' @param colNames logical. Default: TRUE
#'
#' @export
#' @examples
#' mtcars |>
#'    hl('blue') |>
#'    write_xlsx(tempfile())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_xlsx <- function(x, xlsx_filename, colNames = TRUE) {

  stopifnot(is_emphatic(x))
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
    fill[] <- col2hex(fill)
    text[] <- col2hex(text)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a new style for each cell to colour it correctly
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (row in seq(nrow(fill))) {
      for (col in seq(ncol(fill))) {
        cell_style <- openxlsx::createStyle(
          fgFill     = fill[row, col],
          fontColour = text[row, col]
        )
        openxlsx::addStyle(wb, sheet = 1, cell_style, rows = row+1, cols = col+1, gridExpand = FALSE)
      }
    }

    openxlsx::saveWorkbook(wb, xlsx_filename, overwrite = TRUE)
  } else {
    stop("write_xlsx(): Need to have `openxlsx` installed")
  }
}



if (FALSE) {
  x <- head(mtcars) |> hl('blue')
  x <- hl_grep(mode, 'switch')
}
