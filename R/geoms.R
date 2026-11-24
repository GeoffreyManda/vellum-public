# Geoms and annotations.
#
# A deliberately small set. Each one exists because it is needed for a figure a
# journal will accept and is either absent from ggplot2 or wrong by default there.
# Anything already well served elsewhere -- posterior intervals by ggdist, beeswarm
# packing by ggbeeswarm -- is not duplicated here.

#' Tufte range frame
#'
#' @description
#' Axis rules that span only the range of the data, rather than the full width of
#' the panel. The rule then carries information -- it shows where the data start
#' and stop -- instead of merely bounding the plotting area.
#'
#' Use it with `theme_vellum(axis_style = "range")`, which removes the ordinary
#' axis lines so the frame is not drawn twice.
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#'   ggplot2 layer arguments.
#' @param sides Which sides to draw, as a string containing any of `"t"`, `"r"`,
#'   `"b"`, `"l"`.
#'
#' @return A ggplot2 layer.
#' @references Tufte ER (2001). *The Visual Display of Quantitative Information*,
#'   2nd edition.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_rangeframe() +
#'   theme_vellum(axis_style = "range")
geom_rangeframe <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., sides = "bl",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomRangeframe, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(sides = sides, na.rm = na.rm, ...)
  )
}

#' @rdname geom_rangeframe
#' @format NULL
#' @usage NULL
#' @export
GeomRangeframe <- ggplot2::ggproto(
  "GeomRangeframe", ggplot2::Geom,
  optional_aes = c("x", "y"),
  draw_key = ggplot2::draw_key_blank,
  default_aes = ggplot2::aes(colour = ggplot2::from_theme(ink),
                             linewidth = ggplot2::from_theme(linewidth),
                             linetype = 1, alpha = NA),
  draw_panel = function(self, data, panel_params, coord, sides = "bl", lineend = "butt") {
    d <- coord$transform(data, panel_params)
    gp <- grid::gpar(
      col = scales::alpha(d$colour[1] %||% "black", d$alpha[1] %||% NA),
      lwd = (d$linewidth[1] %||% 0.5) * .pt_ggplot(),
      lty = d$linetype[1] %||% 1, lineend = lineend
    )
    grobs <- list()
    if (grepl("b", sides) && !is.null(d$x)) {
      grobs$b <- grid::segmentsGrob(min(d$x), 0, max(d$x), 0, default.units = "npc", gp = gp)
    }
    if (grepl("t", sides) && !is.null(d$x)) {
      grobs$t <- grid::segmentsGrob(min(d$x), 1, max(d$x), 1, default.units = "npc", gp = gp)
    }
    if (grepl("l", sides) && !is.null(d$y)) {
      grobs$l <- grid::segmentsGrob(0, min(d$y), 0, max(d$y), default.units = "npc", gp = gp)
    }
    if (grepl("r", sides) && !is.null(d$y)) {
      grobs$r <- grid::segmentsGrob(1, min(d$y), 1, max(d$y), default.units = "npc", gp = gp)
    }
    grid::gTree(children = do.call(grid::gList, grobs), name = "rangeframe")
  }
)

# ggplot2's linewidth-to-lwd factor, restated locally rather than reaching into
# an unexported constant.
.pt_ggplot <- function() 72.27 / 25.4

# Summary functions for stat_summary(). The mean is paired with a t-based
# interval and the median with sample quantiles, because those are the intervals
# each centre actually implies; silently pairing a median with a normal-theory
# interval is a common and invisible error.
interval_summary <- function(width, point = c("mean", "median")) {
  point <- match.arg(point)
  function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n == 0L) return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
    if (point == "mean") {
      ctr <- mean(x)
      if (n < 2L) return(data.frame(y = ctr, ymin = ctr, ymax = ctr))
      se <- stats::sd(x) / sqrt(n)
      q <- stats::qt(1 - (1 - width) / 2, df = n - 1)
      data.frame(y = ctr, ymin = ctr - q * se, ymax = ctr + q * se)
    } else {
      qs <- stats::quantile(x, c((1 - width) / 2, 1 - (1 - width) / 2), names = FALSE)
      data.frame(y = stats::median(x), ymin = qs[1], ymax = qs[2])
    }
  }
}

#' Point with nested uncertainty intervals
#'
#' @description
#' The correct default display for a group summary: the estimate as a point, a
#' thick inner interval, and a thin outer interval. Two nested intervals convey
#' the shape of the uncertainty at a glance in a way a single bar cannot, and the
#' whole mark occupies less space than a box plot while hiding less.
#'
#' Never use a bar chart for the mean of a continuous variable. A bar encodes a
#' mean as an area anchored at zero, which asserts both that zero is meaningful
#' and that the whole interval from zero to the mean is occupied. Neither is
#' generally true, and the bar hides the sample size and the spread completely.
#'
#' @param mapping,data,position,show.legend,inherit.aes,... Standard ggplot2 layer
#'   arguments, passed to each of the three constituent layers.
#' @param .width Two interval widths, inner then outer.
#' @param point `"mean"`, paired with a t-based interval, or `"median"`, paired
#'   with sample quantiles.
#' @param orientation `"vertical"` (summarise y within x) or `"horizontal"`.
#' @param fatten Multiplier for the inner interval's weight relative to the outer.
#'
#' @return A list of three ggplot2 layers.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_point(position = position_jitter(width = 0.15, seed = 1),
#'              alpha = 0.35, size = 1) +
#'   geom_pointinterval() +
#'   theme_vellum()
geom_pointinterval <- function(mapping = NULL, data = NULL, position = "identity",
                               ..., .width = c(0.66, 0.95), point = c("mean", "median"),
                               orientation = "vertical", fatten = 2.6,
                               show.legend = NA, inherit.aes = TRUE) {
  point <- match.arg(point)
  if (length(.width) != 2L) cli::cli_abort("{.arg .width} must be two widths, inner then outer.")
  .width <- sort(.width)
  common <- list(mapping = mapping, data = data, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 orientation = if (orientation == "horizontal") "y" else "x")
  dots <- list(...)
  fixed <- c("geom", "fun.data", "fun")
  clash <- intersect(names(dots), fixed)
  if (length(clash)) {
    cli::cli_abort(c(
      "{.arg {clash}} cannot be passed to {.fn geom_pointinterval}.",
      i = "The three constituent layers set their own geometry and summary function."
    ))
  }
  # Anything the caller also supplies has to displace this layer's own value
  # rather than sit beside it, since `do.call()` would otherwise match one formal
  # to two arguments.
  layer <- function(...) {
    own <- list(...)
    do.call(ggplot2::stat_summary,
            c(common, own[setdiff(names(own), names(dots))], dots))
  }
  list(
    layer(geom = "linerange", fun.data = interval_summary(.width[2], point)),
    layer(geom = "linerange", fun.data = interval_summary(.width[1], point),
          linewidth = fatten * lw(0.5)),
    layer(geom = "point", fun = if (point == "mean") mean else stats::median)
  )
}

#' Significance bracket with an exact P value
#'
#' @description
#' Draws a bracket between two positions and labels it with the P value as a
#' number. A cloud of asterisks discards the magnitude of the effect and the exact
#' probability, cannot be re-analysed, and encourages reading a threshold as a
#' result; journals increasingly refuse it.
#'
#' @param x Two positions to bracket. Numeric, or the factor levels' positions.
#' @param y Height of the bracket.
#' @param p A P value, formatted by [label_pval()], or `label` for arbitrary text.
#' @param label Text to draw instead of a formatted P value.
#' @param tip Length of the downward tips, in data units on the y axis.
#' @param size Text size in points.
#' @param colour Ink for the bracket and label.
#' @param linewidth Bracket weight as a ggplot2 linewidth; defaults to a hairline.
#' @param vjust Vertical justification of the label above the bracket.
#'
#' @return A list of two ggplot2 layers.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_boxplot(width = 0.5) +
#'   annotate_signif(c(1, 2), y = 8.2, p = 0.0004) +
#'   annotate_signif(c(2, 3), y = 8.8, p = 0.031) +
#'   theme_vellum()
annotate_signif <- function(x, y, p = NULL, label = NULL, tip = NULL, size = 6,
                            colour = "#000000", linewidth = lw(0.4), vjust = -0.35) {
  if (length(x) != 2L) cli::cli_abort("{.arg x} must be exactly two positions.")
  if (is.null(p) && is.null(label)) cli::cli_abort("Supply either {.arg p} or {.arg label}.")
  txt <- label %||% label_pval(p)
  tip <- tip %||% (abs(y) * 0.02 + .Machine$double.eps)
  path <- data.frame(
    x = c(x[1], x[1], x[2], x[2]),
    y = c(y - tip, y, y, y - tip)
  )
  list(
    ggplot2::annotate("path", x = path$x, y = path$y,
                      colour = colour, linewidth = linewidth, lineend = "square"),
    ggplot2::annotate("text", x = mean(x), y = y, label = txt,
                      size = fs(size), colour = colour, vjust = vjust,
                      family = as.character(resolve_font()))
  )
}

#' Scale bar
#'
#' @description
#' For micrographs, maps, and any panel where the axes have been removed but
#' physical extent still matters. A scale bar is not decoration: without it an
#' image panel makes no quantitative claim at all.
#'
#' @param x,y Position of the bar's left end, in data units.
#' @param length Bar length in data units.
#' @param label Text label. Defaults to the length with the supplied `unit`.
#' @param unit Unit symbol for the default label.
#' @param height Bar thickness in millimetres.
#' @param colour Bar and label ink. Use white on dark images.
#' @param size Label text size in points.
#' @param label_position `"above"` or `"below"`.
#'
#' @return A list of two ggplot2 layers.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   annotate_scalebar(x = 46, y = 1.7, length = 15, unit = "min", colour = "white") +
#'   scale_fill_vellum_c("abyss") +
#'   theme_vellum_void()
annotate_scalebar <- function(x, y, length, label = NULL, unit = "",
                              height = 0.8, colour = "#000000", size = 6,
                              label_position = c("above", "below")) {
  label_position <- match.arg(label_position)
  txt <- label %||% paste0(format(length), if (nzchar(unit)) paste0(NBTHIN, unit) else "")
  list(
    ggplot2::annotate("segment", x = x, xend = x + length, y = y, yend = y,
                      colour = colour, linewidth = lw(mm_pt(height)), lineend = "butt"),
    ggplot2::annotate("text", x = x + length / 2, y = y, label = txt,
                      colour = colour, size = fs(size),
                      vjust = if (label_position == "above") -0.6 else 1.5,
                      family = as.character(resolve_font()))
  )
}

#' Reference line for a null or threshold value
#'
#' A null line is drawn differently from data: dashed, weak, and behind the marks.
#' Drawing it in the same weight and ink as the data invites a reader to mistake it
#' for an observation.
#'
#' @param value Position of the line.
#' @param orientation `"vertical"` or `"horizontal"`.
#' @param colour Ink.
#' @param linetype Dash array.
#' @param linewidth Weight.
#' @param ... Passed to [ggplot2::geom_vline()] or [ggplot2::geom_hline()].
#' @return A ggplot2 layer.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   annotate_null(20, "horizontal") + theme_vellum()
annotate_null <- function(value = 0, orientation = c("vertical", "horizontal"),
                          colour = "#767676", linetype = "22", linewidth = lw(0.4), ...) {
  orientation <- match.arg(orientation)
  if (orientation == "vertical") {
    ggplot2::geom_vline(xintercept = value, colour = colour, linetype = linetype,
                        linewidth = linewidth, ...)
  } else {
    ggplot2::geom_hline(yintercept = value, colour = colour, linetype = linetype,
                        linewidth = linewidth, ...)
  }
}

#' North arrow for a map
#'
#' @description
#' A restrained north indicator, positioned relative to the panel rather than in data
#' coordinates, so it stays put when the projection or the extent changes.
#'
#' A north arrow is not decoration and it is also not always required. Omit it when
#' north is unambiguous -- a familiar continent in an upright conventional projection
#' needs no arrow, and adding one is clutter. Include it whenever the projection
#' rotates the graticule, the extent is unfamiliar, or the map is a detail inset whose
#' orientation the reader cannot infer.
#'
#' @param x,y Position within the panel, from 0 to 1.
#' @param size Height of the arrow in millimetres.
#' @param colour Ink.
#' @param label Text above the arrow. Set `""` for none.
#' @param label_size Label size in points. Defaults to the venue's small size.
#' @param venue Venue, for the default label size and typeface.
#'
#' @return A ggplot2 annotation layer.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
#'   geom_point() +
#'   annotate_north_arrow() +
#'   theme_vellum_map()
annotate_north_arrow <- function(x = 0.94, y = 0.10, size = 4.5,
                                 colour = "#000000", label = "N",
                                 label_size = NULL, venue = "nature") {
  check_number(x, "x", min = 0, max = 1)
  check_number(y, "y", min = 0, max = 1)
  check_number(size, "size", min = 0.5)
  tk <- vellum_tokens(venue)
  label_size <- label_size %||% tk$type$size$small

  # A slim filled triangle reads as a direction at 4 mm; a compass rose does not.
  arrow <- grid::polygonGrob(
    x = c(0.5, 0, 0.5, 1), y = c(1, 0, 0.22, 0),
    gp = grid::gpar(fill = colour, col = colour, lwd = 0.3)
  )
  children <- grid::gList(arrow)
  if (nzchar(label)) {
    children <- grid::gList(
      grid::polygonGrob(x = c(0.5, 0, 0.5, 1), y = c(0.78, 0, 0.17, 0),
                        gp = grid::gpar(fill = colour, col = colour, lwd = 0.3)),
      grid::textGrob(label, x = 0.5, y = 0.93, vjust = 0.5,
                     gp = grid::gpar(col = colour, fontsize = label_size,
                                     fontfamily = tk$type$family, fontface = "bold"))
    )
  }
  # The arrow must be nested one level deep. `annotation_custom()` draws its grob with
  # `editGrob(grob, vp = <panel-sized viewport>)`, which REPLACES the outer grob's own
  # viewport -- so a single gTree sized in millimetres has that sizing thrown away and
  # its npc coordinates then fill the entire panel. Wrapping it means the outer tree
  # absorbs the substitution and the inner one keeps its physical size.
  sized <- grid::gTree(
    children = children,
    vp = grid::viewport(x = x, y = y, just = c("centre", "bottom"),
                        width = grid::unit(size * 0.55, "mm"),
                        height = grid::unit(size, "mm"))
  )
  ggplot2::annotation_custom(grid::gTree(children = grid::gList(sized)),
                             xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
