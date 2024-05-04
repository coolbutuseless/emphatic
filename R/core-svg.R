
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
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg <- function(x, ..., width = 1200, height = 900, browsable = FALSE) {

  # res <- as_html(x, ...)


  svg_text <- paste0(
    "<svg fill=\"none\" viewBox=\"0 0 ", width, " ", height,
    "\" width=\"", width, "\" height=\"", height, "\" xmlns=\"http://www.w3.org/2000/svg\">",
    as_svg_group(x, width = width, height = height, ...),
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
#' @return character string containing an SVG snippet.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_svg_anim <- function(x, width = 1200, height = 900, duration = 1, playback = c('infinite', 'click'),
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
      width   = width,
      height  = height,
      visible = i == 1,  # only first frame starts visible
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


if (FALSE) {

  as_svg_anim(
    list(
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:2),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:3),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:4),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:5),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:6),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:7),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:8),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:9),
      mtcars |> head(5) |> hl(ggplot2::scale_color_distiller(), cols = 1, scale_apply = 1:10)
    ),
    duration = c(rep(0.2, 9), 3)
  ) |> writeLines("~/Desktop/demo2.svg")

  as_svg_anim(hl_grep(mode, "switch"), hl_diff("hello", "there")) |> cat()
}


# show_svg <- function(
#     )





if (FALSE) {
  library(dplyr)
  library(tidyr)
  m <- matrix(0.1, 10, 10)

  w <- 16
  h <- 50

  create_sinus <- function(xoff, yoff) {
    expand.grid(x=1:w, y=1:h) |>
      as.data.frame() |>
      mutate(val = cos((x - w/2)/w + xoff) + sin((y - h/3)/h + yoff) ) |>
      mutate(val = round(val, 3)) |>
      spread(x, val) |>
      select(-y) |>
      setNames(sprintf("% 7i", seq(w))) |>
      hl(ggplot2::scale_color_gradient2(), cols = all())
  }


  groups <- purrr::map2(
    cos(seq(0, 2*pi , length.out = 60)),
    sin(seq(-2*pi, 2*pi, length.out = 60)),
    ~create_sinus(.x, .y)
  )


  as_svg_anim(groups, duration = 0.1, playback = 'click') |>
    writeLines("~/Desktop/demo2.svg")

  create_sinus(0.3, -1.2)

  as_svg_anim(groups, duration = 0.1, playback = 'infinite', browsable = TRUE)


  # write_xlsx(create_sinus(0.3, -0.7), "working/sinus.xlsx", opts = hl_opts(text_mode = 'contrast'))



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consolel print output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(emphatic)
  library(dplyr)
  library(tidyr)

  w <- 16
  h <- 50

  create_sinus <- function(xoff, yoff) {
    expand.grid(x=1:w, y=1:h) |>
      as.data.frame() |>
      mutate(val = cos((x - w/2)/w + xoff) + sin((y - h/3)/h + yoff) ) |>
      mutate(val = round(val, 3)) |>
      spread(x, val) |>
      select(-y) |>
      setNames(sprintf("% 7i", seq(w))) |>
      hl(ggplot2::scale_color_gradient2(), cols = all())
  }
  groups <- purrr::map2(
    cos(seq(0, 2*pi , length.out = 60)),
    sin(seq(-2*pi, 2*pi, length.out = 60)),
    function(x, y) {
      # cat("\014")
      create_sinus(x, y) |>
        hl_adjust(full_colour = TRUE, text_mode = 'asis') |>
        as.character() |>
        cat()
      Sys.sleep(0.1)
    }
  )














}
