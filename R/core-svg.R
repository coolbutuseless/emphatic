
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap a single emphatic object into an SVG for display
#'
#' This is mainly useful within a github \code{README.md} since github will
#' not rendered html-styled text in colour, but \emph{will} render it correctly
#' if it is within a \code{<svg>} tags.
#'
#' This is just a the results of \code{as_html()} wrapped in \code{<svg>} tags
#'
#' @inheritParams as_html
#' @inheritParams as_svg_anim
#' @param width,height viewBox dimensions for SVG
#'
#' @return Character string containing SVG representation
#'
#' @export
#' @examples
#' hl_diff('hello', 'there') |>
#'   as_svg() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg <- function(x, width = 1200, height = 900, ..., font_size = NULL,
                   style = list(), browsable = FALSE) {

  # res <- as_html(x, ...)


  svg_text <- paste0(
    "<svg fill=\"none\" viewBox=\"0 0 ", width, " ", height,
    "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">",
    as_svg_group(x, width = width, height = height, font_size = font_size,
                 style = style, ...),
    '</svg>'
  )


  svg_text <- gsub("style>", ">", svg_text, fixed = TRUE)

  if (isTRUE(browsable)) {
    attr(svg_text, "html") <- TRUE
    attr(svg_text, "browsable_html") <- TRUE
  }
  class(svg_text) <- union(c('knit_asis', 'html', 'character'), class(svg_text))

  svg_text
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap an emphatic object to part of an SVG
#'
#' This function wraps \code{html} in SVG group tags (i.e. \code{<g>}).  This
#' may then be wrapped in \code{<svg>} tags to create a stand-along SVG.
#'
#' This function is used internall by both \code{as_svg()} and \code{as_svg_anim()}
#'
#' @inheritParams as_html
#' @param width,height viewBox dimensions for SVG
#' @param extra extra tags to insert into group. default NULL
#' @param visible should the group be visible? Default: TRUE.  When animating,
#'        every frame other than the first should be set as \code{visible = FALSE}.
#'
#'
#' @return Character string containing representation as an SVG group element
#'         i.e. \code{<g>}.  This result is suitable for combining with other
#'         SVG elements into a custom SVG document.
#'
#' @export
#' @examples
#' hl_diff('hello', 'there') |>
#'   as_svg_group() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg_group <- function(x, width = 1200, height = 900, font_size = NULL,
                         style = list(), visible = TRUE, extra = NULL, ...) {

  res <- as_html(x, font_size = font_size, style = style, ...)


  svg_text <- paste(
    ifelse(visible, '<g visibility="visible">', '<g visibility="hidden">'),
    '<foreignObject width=\"100%\" height=\"100%\">',
    '<div xmlns=\"http://www.w3.org/1999/xhtml\">',
    res,
    "</div>",
    "</foreignObject>",
    extra,
    "</g>",
    sep = "\n"
  )


  svg_text <- gsub("style>", ">", svg_text, fixed = TRUE)
  # class(svg_text) <- unique(c('knit_asis', class(svg_text)))

  svg_text
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This helper function creates a \code{<set>} tag to do frame flipping
# animation within an SVG
# The first frame begins at either time = 0, or with a click
# subsequent frames are then triggered to start at the end of the
# prior frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_animate_tag <- function(i, n, dur = 1, playback, svg_id) {
  if (i == 1) {

    playback_mode <- switch (
      playback,
      infinite = sprintf("0s;%s%03i.end", svg_id, n),
      click    = sprintf("%s.click", svg_id)
    )

    sprintf('<set id="%s%03i" attributeName="visibility" begin="%s"
            to="visible" dur="%fs" />', svg_id, i, playback_mode, dur)
  } else {
    sprintf('<set id="%s%03i" attributeName="visibility" begin="%s%03i.end"
            to="visible" dur="%fs" />', svg_id, i, svg_id, i - 1, dur)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrap multiple emphatic object into an SVG animation
#'
#' Idea borrowed from pointblank
#'
#' @inheritParams as_svg
#' @param width,height viewBox dimensions for SVG
#' @param x list of emphatic objects
#' @param duration frame duration in seconds. May be a single value used for
#'        all frames, or a vector of values (one duration value for each frame).
#'        Can be fractions of a second.
#' @param playback 'click', 'infinite'
#' @param svg_id ID to use for the SVG tag.  Default: NULL means to create a random
#'        ID
#' @param browsable Should the SVG be rendered to the RStudio Viewer pane when
#'        when printed (instead of console output)? Default: FALSE
#'
#' @return Character string containing an animated SVG representation displaying
#'         all elements sequentially
#'
#' @export
#' @examples
#' list(
#'   hl_diff('hello', 'there'),
#'   hl_diff('goodbye', 'good boy')
#' ) |>
#'   as_svg_anim() |>
#'   cat()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg_anim <- function(x, width = 1200, height = 900, duration = 1, playback = c('infinite', 'click'),
                        font_size = NULL, style = list(),
                        svg_id = NULL, browsable = FALSE) {

  # <animate attributeName="visibility" begin="svg1.click" dur="3s" from="visible" to="hidden" repeatCount="indefinite" />

  playback <- match.arg(playback)

  N <- length(x)
  groups <- vector('list', N)

  if (length(duration) != 1 && length(duration) != N) {
    stop("delay must be length = 1, or match number of objects")
  }
  if (length(duration) == 1) {
    duration <- rep(duration, N)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create "unique" ID for the SVG.  This is  in case there are mulitple of these
  # SVGs on a page.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(svg_id)) {
    svg_id <- paste(
      "emphatic",
      paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""),
      sep = ""
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an svg group (<g> tag) for each emphatic object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(x)) {
    groups[[i]] <- as_svg_group(
      x[[i]],
      width     = width,
      height    = height,
      visible   = FALSE,
      font_size = font_size,
      style     = style,
      extra = make_animate_tag(
        i, N,
        dur        = duration[i],
        playback   = playback,
        svg_id     = svg_id
      )
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble Full SVG text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  svg_text <- paste0(
    "<svg id=\"", svg_id, "\" fill=\"none\" viewBox=\"0 0 ", width, " ", height,
    "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">",
    paste(unlist(groups), collapse = ""),
    '</svg>\n',
    collapse = "\n"
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set as browsable to show in the Rstudio viewer instead of the console
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(browsable)) {
    attr(svg_text, "html") <- TRUE
    attr(svg_text, "browsable_html") <- TRUE
  }
  class(svg_text) <- union(c('knit_asis', 'html', 'character'), class(svg_text))

  svg_text
}
