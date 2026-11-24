# Emphasis and direct labelling.
#
# Two techniques that do more for a figure's legibility than any theme setting, and
# that neither ggplot2 nor the theme layer can supply on its own because both need
# to know which series matters.

# The lightest neutral grey that still clears WCAG 1.4.11's 3:1 non-text contrast
# against white: measured 3.03:1, where the next step up, #959595, gives 2.995:1.
# Muted series have to stay visible, or the comparison they provide is not there.
.mute_grey <- "#949494"

#' Colour one series and mute the rest
#'
#' @description
#' The most effective single edit available to a scientific figure with more than
#' three groups: give the series the figure is about one saturated colour, and render
#' everything else in a neutral grey. A reader's eye goes where the chroma is, so
#' spending chroma on every group spends it on nothing.
#'
#' This is also the honest way to show a comparison series. Ten equally coloured
#' lines assert that all ten matter equally; one coloured line against nine grey ones
#' asserts what the sentence in the results section actually says.
#'
#' @param levels All levels of the grouping variable, in any order.
#' @param highlight The level or levels to emphasise.
#' @param accent Colours for the highlighted levels. A single value is used for all;
#'   `NULL` takes as many as needed from the palette.
#' @param mute Colour for everything else. The default is the lightest grey that
#'   still reaches WCAG's 3:1 non-text contrast against white, measured at 3.03:1.
#' @param palette Palette to draw accents from when `accent` is `NULL`.
#'
#' @return A named character vector of colours, suitable for
#'   [ggplot2::scale_colour_manual()] or the `values` of any manual scale.
#' @export
#' @examples
#' emphasise(c("A", "B", "C", "D"), highlight = "C")
#' emphasise(month.abb, highlight = c("Jan", "Jul"))
emphasise <- function(levels, highlight, accent = NULL, mute = .mute_grey,
                      palette = "okabe_ito") {
  levels <- as.character(levels)
  highlight <- as.character(highlight)
  unknown <- setdiff(highlight, levels)
  if (length(unknown)) {
    cli::cli_abort(c("{.arg highlight} names {length(unknown)} level{?s} not in {.arg levels}: {.val {unknown}}.",
                     i = "Available: {.val {levels}}."))
  }
  check_colour(mute, "mute")
  acc <- accent %||% vellum_pal(palette, n = max(length(highlight), 1L))
  if (length(acc) == 1L) acc <- rep(acc, length(highlight))
  acc <- recycle_to(acc, length(highlight), "accent")
  check_colour(acc, "accent")

  out <- stats::setNames(rep(mute, length(levels)), levels)
  out[highlight] <- acc
  out
}

#' Emphasis scales
#'
#' @description
#' [emphasise()] wrapped as a scale, so the highlight can be declared inline. The
#' legend is dropped by default: once one series is coloured and the rest are grey,
#' a legend has nothing left to disambiguate, and the emphasised series should be
#' labelled directly with [label_series()].
#'
#' @param highlight The level or levels to emphasise. The remaining levels do not
#'   need to be listed.
#' @param accent Colours for the highlighted levels; `NULL` takes them from
#'   `palette`.
#' @param mute Colour for every level that is not highlighted.
#' @param palette Palette to draw accents from.
#' @param guide Guide specification. `"none"` by default, because once one series is
#'   coloured and the rest are grey there is nothing left for a key to disambiguate.
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#' @return A ggplot2 scale.
#'
#' @details
#' Implemented over [ggplot2::scale_colour_manual()] by naming only the highlighted
#' levels and sending everything else to `na.value`. That is why the level list is not
#' needed: a manual scale with a named `values` vector matches by name, and any level
#' it does not find takes `na.value`. Do not add `limits = force`, which would demand
#' a colour for every level and defeat the mechanism.
#'
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   scale_colour_emphasis("virginica") +
#'   theme_vellum()
scale_colour_emphasis <- function(highlight, accent = NULL, mute = .mute_grey,
                                  palette = "okabe_ito", guide = "none", ...) {
  ggplot2::scale_colour_manual(values = emphasis_values(highlight, accent, palette),
                               na.value = mute, guide = guide, ...)
}

#' @rdname scale_colour_emphasis
#' @export
scale_color_emphasis <- scale_colour_emphasis

#' @rdname scale_colour_emphasis
#' @export
scale_fill_emphasis <- function(highlight, accent = NULL, mute = .mute_grey,
                                palette = "okabe_ito", guide = "none", ...) {
  ggplot2::scale_fill_manual(values = emphasis_values(highlight, accent, palette),
                             na.value = mute, guide = guide, ...)
}

emphasis_values <- function(highlight, accent, palette) {
  highlight <- as.character(highlight)
  if (!length(highlight)) cli::cli_abort("{.arg highlight} must name at least one level.")
  acc <- accent %||% vellum_pal(palette, n = length(highlight))
  if (length(acc) == 1L) acc <- rep(acc, length(highlight))
  acc <- recycle_to(acc, length(highlight), "accent")
  check_colour(acc, "accent")
  stats::setNames(acc, highlight)
}

#' Label series directly, at the end of each line
#'
#' @description
#' A legend costs the reader a round trip: eye to the mark, eye to the key, eye back,
#' repeated for every series. Labelling each series where it ends removes the trip
#' entirely, and is why almost every well-made line chart in print has no legend.
#'
#' Labels are placed at the last observation of each group and pushed apart
#' vertically to a minimum separation, so series that converge do not produce
#' overlapping text. No repulsion package is needed.
#'
#' Remember to make room: the labels sit outside the panel, so pair this with
#' `expand_limits()` or the `expand` argument, which this function returns a hint
#' for via its `x` placement.
#'
#' @param data The plotting data.
#' @param x,y,group Column names, as strings.
#' @param side `"right"` to label the last point of each series, `"left"` for the
#'   first.
#' @param nudge Horizontal offset from the end point, as a fraction of the x range.
#' @param min_gap Minimum vertical separation between labels, as a fraction of the y
#'   range. Increase it when labels still touch.
#' @param size Text size in points. Defaults to the venue's small size.
#' @param venue Venue, used for the default text size and family.
#' @param colour Optional single colour. By default labels inherit the series colour,
#'   which is what ties label to line without a key.
#' @param ... Passed to [ggplot2::geom_text()].
#'
#' @return A list of two layers: the labels, and an `expand_limits()` that makes room
#'   for them.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(11)
#' d <- data.frame(
#'   year = rep(2000:2010, 3),
#'   value = c(cumsum(rnorm(11, 1)), cumsum(rnorm(11, 2)), cumsum(rnorm(11, 0.5))),
#'   series = rep(c("Alpha", "Beta", "Gamma"), each = 11)
#' )
#' ggplot(d, aes(year, value, colour = series)) +
#'   geom_line() +
#'   label_series(d, "year", "value", "series") +
#'   scale_colour_vellum(guide = "none") +
#'   theme_vellum()
label_series <- function(data, x, y, group, side = c("right", "left"),
                         nudge = 0.02, min_gap = 0.055, size = NULL,
                         venue = "nature", colour = NULL, ...) {
  side <- match.arg(side)
  d <- data.frame(.x = data[[x]], .y = data[[y]], .g = as.character(data[[group]]),
                  stringsAsFactors = FALSE)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!nrow(d)) cli::cli_abort("No complete cases in {.arg data}.")

  pick <- if (side == "right") which.max else which.min
  ends <- do.call(rbind, lapply(split(d, d$.g), function(g) g[pick(g$.x), , drop = FALSE]))

  x_span <- diff(range(d$.x))
  y_span <- diff(range(d$.y))
  off <- (if (side == "right") 1 else -1) * nudge * x_span
  ends$.lab_y <- spread_labels(ends$.y, min_gap = min_gap * y_span,
                               limits = range(d$.y) + c(-1, 1) * min_gap * y_span)
  ends$.lab_x <- ends$.x + off

  tokens <- vellum_tokens(venue)
  size <- size %||% tokens$type$size$axis_text

  # Never inherit the plot's aesthetics: the summary frame has its own column names,
  # so an inherited `colour = <group>` mapping would look for a column that is not
  # here. Mapping colour to the group explicitly instead picks up the plot's own
  # colour scale, which is what ties each label to its line without a key.
  mapping <- if (is.null(colour)) {
    ggplot2::aes(x = .data$.lab_x, y = .data$.lab_y, label = .data$.g,
                 colour = .data$.g)
  } else {
    ggplot2::aes(x = .data$.lab_x, y = .data$.lab_y, label = .data$.g)
  }
  args <- list(
    data = ends, mapping = mapping,
    hjust = if (side == "right") 0 else 1,
    size = fs(size), family = tokens$type$family,
    inherit.aes = FALSE, show.legend = FALSE, ...
  )
  if (!is.null(colour)) args$colour <- colour
  txt <- do.call(ggplot2::geom_text, args)
  # Room for the text itself, roughly one character width per character at this size.
  pad <- (if (side == "right") 1 else -1) *
    (nudge + 0.011 * max(nchar(ends$.g))) * x_span
  list(txt, ggplot2::expand_limits(x = max(ends$.lab_x + pad, na.rm = TRUE)))
}
