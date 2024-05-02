
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert R colours to hex colours
#'
#' @param colours Character vector of R colours
#'
#' @return Character vector of 6-char hex colours
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2hex <- function(colours) {

  colours <- grDevices::col2rgb(colours)
  colours <- structure(sprintf("%02x", colours), dim = dim(colours))
  colours <- apply(colours, 2, paste0, collapse = '')

  paste0('#', colours)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening html spans for background colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of HTML opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2fill_html <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    "<span>",
    paste0("<span style='background-color:", colours, ";'>")
  )

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of colours to opening html spans for text colour
#'
#' @param colours Chcaracter vector of R colours
#'
#' @return Character vector of HTML opening span tags
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col2text_html <- function(colours) {
  no_colour <- is.na(colours) | colours == ''
  colours[no_colour] <- NA
  colours <- col2hex(colours)

  ifelse(
    no_colour,
    "<span>",
    paste0("<span style='color:", colours, ";'>")
  )

}

reset_html         <- "</span></span>"
underline_on_html  <- "<span style='text-decoration:underline;'>"
underline_off_html <- "</span>"




html_replacement <- c(
  `&` = "&amp;",
  `<` = "&lt;",
  `>` = "&gt;",
  `"` = "&quot;",
  `'` = "&#39;"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Escape HTML by replacing special characters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
escape_html <- function(x) {
  x <- enc2utf8(x)
  for (orig in names(html_replacement)) {
    x <- gsub(orig, html_replacement[[orig]], x, fixed = TRUE, useBytes = TRUE)
  }
  Encoding(x) <- 'UTF-8'
  x
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render an emphatic object to HTML
#'
#' @param x emphatic object
#' @param style html tag styling to apply to the \code{<pre>} wrapper for the
#'        returned HTML
#' @param ... other arguments passed to \code{as.character.emphatic}
#' @param complete logical. Default: FALSE.  If TRUE, then add DOCTYPE and
#'        the tags for 'html', 'body' and 'head' to make a complete standalone
#'        html file.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_html <- function(x, style = NULL, ..., complete = FALSE) {

  if (!is.null(style)) {
    pre <- paste0("<pre style='", style, "'>")
  } else {
    pre <- "<pre>"
  }

  res <- paste0(pre, as.character(x, ..., mode = 'html'), "</pre>")

  if (isTRUE(complete)) {
    res <- paste0("<!DOCTYPE html>\n<html>\n<head></head>\n<body>", res, "\n</body>\n</html>")
  }


  class(res) <- unique(c('knit_asis', class(res)))

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object into an SVG for display in a github README.md
#'
#' Idea borrowed from pointblank
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#'
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg <- function(x, width = 1200, height = 900, ...) {

  # res <- as_html(x, ...)


  svg_text <- paste0(
    "<svg fill=\"none\" viewBox=\"0 0 ", width, " ", height,
    "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">",
    as_svg_group(x, width = width, height = height, ...),
    '</svg>'
  )


  svg_text <- gsub("style>", ">", svg_text, fixed = TRUE)


  class(svg_text) <- unique(c('knit_asis', class(svg_text)))

  svg_text
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object into an SVG for display in a github README.md
#'
#' Idea borrowed from pointblank
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#' @param extra extra tags to insert into group. default NULL
#'
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg_group <- function(x, width = 1200, height = 900, visible = TRUE, extra = NULL, ...) {

  res <- as_html(x, ...)


  svg_text <- paste(
    "",
    ifelse(visible, '<g visibility="visible">', '<g visibility="hidden">'),
    '<foreignObject width=\"100%\" height=\"100%\">',
    '<div xmlns=\"http://www.w3.org/1999/xhtml\">',
    res,
    "</div>",
    "</foreignObject>",
    extra,
    "</g>",
    "",
    sep = "\n"
  )


  svg_text <- gsub("style>", ">", svg_text, fixed = TRUE)

  class(svg_text) <- unique(c('knit_asis', class(svg_text)))

  svg_text
}


make_animate_tag <- function(i, n) {
  if (i == 1) {
    sprintf('<animate id="img%03i" attributeName="visibility" begin="0s; img%03i.end"
            values="hidden;visible;visible;" dur="2s" fill="freeze" />', i, n)
  } else {
    sprintf('<animate id="img%03i" attributeName="visibility" begin="img%03i.end"
            values="hidden;visible;visible;" dur="2s" fill="freeze"/>', i, i - 1)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object into an SVG for display in a github README.md
#'
#' Idea borrowed from pointblank
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#' @param ... multiple emphatic objects
#'
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg_anim <- function(..., width = 1200, height = 900) {

  # <animate attributeName="visibility" begin="svg1.click" dur="3s" from="visible" to="hidden" repeatCount="indefinite" />

  objs <- list(...)
  groups <- vector('list', length(objs))

  for (i in seq_along(objs)) {
    groups[[i]] <- as_svg_group(objs[[i]], width = width, height = height,
                                visible = FALSE,
                                extra = make_animate_tag(i, length(objs)))
  }

  svg_text <- paste(
    "<svg id=\"emphatic-anim\" fill=\"none\" viewBox=\"0 0 ", width, " ", height,
    "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">",
    paste(unlist(groups), collapse = ""),
    # groups[[1]],
    # as_svg_group(objs[i], width = width, height = height, extra = NULL),
    '</svg>',
    collapse = "\n"
  )


  class(svg_text) <- unique(c('knit_asis', class(svg_text)))

  svg_text
}


if (FALSE) {

  as_svg_anim(hl_grep(mode, "switch"), hl_diff("hello", "there")) |> cat()


}
















