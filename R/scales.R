# Scales.
#
# With theme_vellum() applied, explicit scale calls are usually unnecessary:
# ggplot2 4.0 lets the theme carry the palettes, so a bare geom already gets
# audited colour. These exist for the cases the theme cannot cover -- choosing a
# different palette for one layer, setting a midpoint on a diverging scale,
# binning a continuous scale, or naming the colour for missing data.

vellum_discrete_pal <- function(palette, rev = FALSE) {
  function(n) {
    cols <- vellum_pal(palette, n = n, rev = rev)
    if (length(cols) < n) {
      cli::cli_abort("Palette {.val {palette}} cannot supply {n} distinct colours.")
    }
    cols
  }
}

#' Discrete colour and fill scales
#'
#' @param palette Palette name. See [vellum_palettes()].
#' @param rev Reverse the palette order.
#' @param na.value Colour for missing values. Defaults to the palette's own
#'   designated bad-data colour where it has one, so missing data reads as absent
#'   rather than as a low value.
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @param aesthetics Aesthetics to apply the scale to.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_vellum("bright") +
#'   theme_vellum()
scale_colour_vellum <- function(palette = "okabe_ito", rev = FALSE,
                                na.value = NULL, ..., aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = vellum_discrete_pal(palette, rev),
    na.value = na.value %||% pal_na(palette), ...
  )
}

#' @rdname scale_colour_vellum
#' @export
scale_color_vellum <- scale_colour_vellum

#' @rdname scale_colour_vellum
#' @export
scale_fill_vellum <- function(palette = "okabe_ito", rev = FALSE,
                              na.value = NULL, ..., aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = vellum_discrete_pal(palette, rev),
    na.value = na.value %||% pal_na(palette), ...
  )
}

#' Continuous colour and fill scales
#'
#' @description
#' Continuous scales built on the ramp engine, so the colour distance between two
#' values is proportional to the difference between them. `guide` defaults to a
#' framed colour bar with no ticks, which is the journal convention.
#'
#' @param palette Sequential or diverging palette name.
#' @param rev Reverse the ramp.
#' @param na.value Colour for missing values.
#' @param n Number of colours to build the ramp from. 256 is smooth on any device.
#' @param guide Guide specification.
#' @param ... Passed to [ggplot2::continuous_scale()].
#' @param aesthetics Aesthetics to apply the scale to.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_vellum_c("abyss") +
#'   theme_vellum_frame()
scale_colour_vellum_c <- function(palette = "abyss", rev = FALSE, na.value = NULL,
                                  n = 256, guide = "colourbar", ...,
                                  aesthetics = "colour") {
  cols <- vellum_pal(palette, n = n, rev = rev)
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = scales::pal_gradient_n(cols),
    na.value = na.value %||% pal_na(palette), guide = guide, ...
  )
}

#' @rdname scale_colour_vellum_c
#' @export
scale_color_vellum_c <- scale_colour_vellum_c

#' @rdname scale_colour_vellum_c
#' @export
scale_fill_vellum_c <- function(palette = "abyss", rev = FALSE, na.value = NULL,
                                n = 256, guide = "colourbar", ..., aesthetics = "fill") {
  cols <- vellum_pal(palette, n = n, rev = rev)
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = scales::pal_gradient_n(cols),
    na.value = na.value %||% pal_na(palette), guide = guide, ...
  )
}

#' Diverging colour and fill scales anchored at a midpoint
#'
#' @description
#' A diverging scale is only meaningful when there is a real midpoint -- no
#' change, no effect, no difference -- and the midpoint must be anchored to that
#' value rather than to the middle of the observed data. If the data run from
#' -0.2 to +1.8 and the scale is not anchored, zero lands a quarter of the way
#' along the ramp and the neutral colour marks a value that means nothing.
#'
#' This scale anchors the hinge at `midpoint` and makes the limits symmetric about
#' it by default, so equal deviations in either direction are given equal colour.
#'
#' @param palette Diverging palette name. `"strata"` is the default because it has
#'   the highest measured worst-case separation under colour-vision deficiency.
#' @param midpoint The value the neutral hinge represents.
#' @param symmetric Extend the limits so they are symmetric about `midpoint`.
#' @param rev Reverse the ramp.
#' @param na.value Colour for missing values.
#' @param n Ramp resolution.
#' @param guide Guide specification.
#' @param ... Passed to [ggplot2::continuous_scale()].
#' @param aesthetics Aesthetics to apply the scale to.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' d <- expand.grid(x = 1:10, y = 1:10)
#' d$z <- with(d, (x - 6) * (y - 4) / 8)
#' ggplot(d, aes(x, y, fill = z)) +
#'   geom_raster() +
#'   scale_fill_vellum_div(midpoint = 0) +
#'   theme_vellum_frame()
scale_fill_vellum_div <- function(palette = "strata", midpoint = 0, symmetric = TRUE,
                                  rev = FALSE, na.value = NULL, n = 256,
                                  guide = "colourbar", ..., aesthetics = "fill") {
  diverging_scale(palette, midpoint, symmetric, rev, na.value, n, guide, aesthetics, ...)
}

#' @rdname scale_fill_vellum_div
#' @export
scale_colour_vellum_div <- function(palette = "strata", midpoint = 0, symmetric = TRUE,
                                    rev = FALSE, na.value = NULL, n = 256,
                                    guide = "colourbar", ..., aesthetics = "colour") {
  diverging_scale(palette, midpoint, symmetric, rev, na.value, n, guide, aesthetics, ...)
}

#' @rdname scale_fill_vellum_div
#' @export
scale_color_vellum_div <- scale_colour_vellum_div

diverging_scale <- function(palette, midpoint, symmetric, rev, na.value, n,
                            guide, aesthetics, ...) {
  cols <- vellum_pal(palette, n = n, rev = rev)
  entry <- vellum_palettes()
  kind <- entry$kind[match(vellum_key(palette), entry$palette)]
  if (!identical(kind, "diverging")) {
    cli::cli_warn(c(
      "{.val {palette}} is a {kind} palette used on a diverging scale.",
      i = "A midpoint implies two directions; a sequential ramp has only one.",
      i = "See {.code vellum_palettes('diverging')}."
    ))
  }
  # Rescale so that `midpoint` lands exactly on the hinge, whatever the data do.
  rescaler <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to = to, from = from, mid = midpoint)
  }
  limits_fn <- if (symmetric) {
    function(l) {
      r <- max(abs(l - midpoint), na.rm = TRUE)
      c(midpoint - r, midpoint + r)
    }
  } else NULL
  args <- list(
    aesthetics = aesthetics,
    palette = scales::pal_gradient_n(cols),
    na.value = na.value %||% pal_na(palette),
    rescaler = rescaler, guide = guide, ...
  )
  if (!is.null(limits_fn) && is.null(args$limits)) args$limits <- limits_fn
  do.call(ggplot2::continuous_scale, args)
}

#' Binned colour and fill scales
#'
#' @description
#' A binned scale is often the honest choice for a continuous quantity read off a
#' legend, because readers cannot recover a value from a smooth gradient to better
#' than a few bins anyway. Explicit bins make the achievable precision visible
#' instead of implying precision that is not there.
#'
#' @param palette Palette name.
#' @param bins Number of bins.
#' @param rev Reverse the ramp.
#' @param na.value Colour for missing values.
#' @param guide Guide specification.
#' @param ... Passed to [ggplot2::binned_scale()].
#' @param aesthetics Aesthetics to apply the scale to.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_vellum_binned("ember", bins = 6) +
#'   theme_vellum_frame()
scale_fill_vellum_binned <- function(palette = "abyss", bins = 5, rev = FALSE,
                                     na.value = NULL, guide = "coloursteps", ...,
                                     aesthetics = "fill") {
  ggplot2::binned_scale(
    aesthetics = aesthetics,
    palette = function(x) vellum_pal(palette, n = length(x), rev = rev),
    na.value = na.value %||% pal_na(palette),
    nice.breaks = TRUE, n.breaks = bins, guide = guide, ...
  )
}

#' @rdname scale_fill_vellum_binned
#' @export
scale_colour_vellum_binned <- function(palette = "abyss", bins = 5, rev = FALSE,
                                       na.value = NULL, guide = "coloursteps", ...,
                                       aesthetics = "colour") {
  ggplot2::binned_scale(
    aesthetics = aesthetics,
    palette = function(x) vellum_pal(palette, n = length(x), rev = rev),
    na.value = na.value %||% pal_na(palette),
    nice.breaks = TRUE, n.breaks = bins, guide = guide, ...
  )
}

#' @rdname scale_fill_vellum_binned
#' @export
scale_color_vellum_binned <- scale_colour_vellum_binned

#' Redundant-encoding scales for shape and linetype
#'
#' @description
#' Map the same variable to shape or linetype as well as colour. This is not
#' belt-and-braces: it is what makes a figure readable to a reader with a colour
#' deficiency, in a photocopy, and on a projector with a washed-out gamut.
#'
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl), shape = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_vellum() +
#'   scale_shape_vellum() +
#'   theme_vellum()
scale_shape_vellum <- function(...) {
  ggplot2::discrete_scale(aesthetics = "shape", palette = function(n) vellum_shapes(n), ...)
}

#' @rdname scale_shape_vellum
#' @export
scale_linetype_vellum <- function(...) {
  ggplot2::discrete_scale(aesthetics = "linetype", palette = function(n) vellum_linetypes(n), ...)
}

#' Continuous position scales with print-appropriate defaults
#'
#' @description
#' Three small corrections that together account for much of the difference
#' between a default ggplot2 axis and a typeset one: typographically correct
#' number labels via [label_vellum()], fewer breaks because a 40 mm panel cannot
#' carry seven of them legibly at 6 pt, and no expansion beyond the data on the
#' value axis unless asked for.
#'
#' @param n_breaks Target number of breaks. 4 suits a single-column panel; 5 or 6
#'   a double-column one.
#' @param digits Decimal places, or `NULL` to choose automatically.
#' @param expand Expansion. Defaults to a small multiplicative pad.
#' @param labels Labelling function.
#' @param ... Passed to [ggplot2::scale_x_continuous()] or its y counterpart.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   scale_x_vellum() + scale_y_vellum() + theme_vellum()
scale_x_vellum <- function(n_breaks = 5, digits = NULL,
                           expand = ggplot2::expansion(mult = 0.04),
                           labels = label_vellum(digits), ...) {
  ggplot2::scale_x_continuous(breaks = scales::breaks_extended(n_breaks),
                              labels = labels, expand = expand, ...)
}

#' @rdname scale_x_vellum
#' @export
scale_y_vellum <- function(n_breaks = 5, digits = NULL,
                           expand = ggplot2::expansion(mult = 0.04),
                           labels = label_vellum(digits), ...) {
  ggplot2::scale_y_continuous(breaks = scales::breaks_extended(n_breaks),
                              labels = labels, expand = expand, ...)
}
