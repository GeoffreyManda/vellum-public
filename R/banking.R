# Banking to 45 degrees.
#
# Cleveland's result: the accuracy with which a reader judges the rate of change of a
# line depends on the angle at which it is drawn, and judgement is best when the
# typical segment sits near 45 degrees. A trend drawn at 5 degrees looks flat and one
# drawn at 85 degrees looks vertical, and neither is a property of the data -- both
# are properties of the aspect ratio the figure happened to be given.
#
# The awkward part, which is why almost no package offers this, is that banking sets
# the panel's shape while a journal sets the figure's width. Those two constraints
# meet in the figure's HEIGHT, so that is what this computes: the height to hand to
# fit_width(), leaving the exact-width guarantee intact.

#' Aspect ratio that banks the typical slope to 45 degrees
#'
#' @description
#' Returns the panel height-to-width ratio at which the typical segment of a series is
#' drawn at 45 degrees, following Cleveland's banking principle.
#'
#' Use [bank_height()] instead when the figure also has to be an exact width, which
#' for a journal figure it does.
#'
#' @param data A data frame.
#' @param x,y Column names, as strings.
#' @param group Optional grouping column; slopes are computed within group.
#' @param method `"median"` (default, robust to a few steep segments) or `"mean"`.
#' @param weighted Weight each segment's slope by its horizontal extent, which is
#'   correct for irregularly spaced series.
#'
#' @return A single number: panel height divided by panel width.
#'
#' @references Cleveland WS (1993). *Visualizing Data*. Also Cleveland WS, McGill ME,
#'   McGill R (1988). The shape parameter of a two-variable graph. *JASA*
#'   83(402):289-300.
#'
#' @export
#' @examples
#' set.seed(11)
#' d <- data.frame(x = 1:40, y = cumsum(rnorm(40, 0.4)))
#' bank_aspect(d, "x", "y")
bank_aspect <- function(data, x, y, group = NULL, method = c("median", "mean"),
                        weighted = TRUE) {
  method <- match.arg(method)
  d <- data.frame(.x = as.numeric(data[[x]]), .y = as.numeric(data[[y]]),
                  .g = if (is.null(group)) "all" else as.character(data[[group]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 2L) cli::cli_abort("Need at least two complete observations.")

  x_span <- diff(range(d$.x))
  y_span <- diff(range(d$.y))
  if (x_span == 0 || y_span == 0) {
    cli::cli_abort("Cannot bank a series with no range in {.arg {if (x_span == 0) x else y}}.")
  }

  seg <- do.call(rbind, lapply(split(d, d$.g), function(g) {
    g <- g[order(g$.x), , drop = FALSE]
    if (nrow(g) < 2L) return(NULL)
    dx <- diff(g$.x); dy <- diff(g$.y)
    keep <- dx != 0
    data.frame(slope = abs(dy[keep] / dx[keep]), w = abs(dx[keep]))
  }))
  if (is.null(seg) || !nrow(seg)) cli::cli_abort("No segments with non-zero horizontal extent.")

  s <- if (method == "mean") {
    if (weighted) stats::weighted.mean(seg$slope, seg$w) else mean(seg$slope)
  } else {
    if (weighted) weighted_median(seg$slope, seg$w) else stats::median(seg$slope)
  }
  if (!is.finite(s) || s == 0) return(1)

  # A data slope s renders at 45 degrees when
  #   s * (panel_h / y_span) / (panel_w / x_span) == 1,
  # so panel_h / panel_w == y_span / (s * x_span).
  y_span / (s * x_span)
}

weighted_median <- function(x, w) {
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

#' Figure height that banks the typical slope to 45 degrees
#'
#' @description
#' The composable form of [bank_aspect()]. Given a plot and its target width, this
#' measures the decoration, applies the banked aspect ratio to the panel, and returns
#' the **total figure height** to pass to [fit_width()] or [save_figure()].
#'
#' Doing it this way rather than by setting `aspect.ratio` matters: `aspect.ratio`
#' fixes the panel's shape and lets its size float, which is incompatible with the
#' absolute panel geometry [fit_width()] uses to guarantee an exact width. Solving for
#' the height instead means banking and exact geometry compose rather than compete.
#'
#' @param plot A `ggplot` object.
#' @param data,x,y,group Passed to [bank_aspect()]. If `data` is `NULL` the plot's own
#'   data are used.
#' @param venue,column Target venue and column, as in [fit_width()].
#' @param width Explicit width in millimetres, overriding `venue`/`column`.
#' @param limits Bounds on the returned aspect ratio, as `c(min, max)`. Banking an
#'   almost-flat or almost-vertical series would otherwise ask for an absurd figure;
#'   the default keeps the panel between 1:3 and 3:1.
#' @param ... Passed to [bank_aspect()].
#'
#' @return The total height in millimetres, with the banked panel aspect ratio in the
#'   `aspect` attribute and a note in `clamped` if `limits` bound the result.
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(11)
#' d <- data.frame(x = 1:40, y = cumsum(rnorm(40, 0.4)))
#' p <- ggplot(d, aes(x, y)) + geom_line() + theme_venue("nature")
#' h <- bank_height(p, d, "x", "y", venue = "nature", column = "single")
#' h
#' fitted <- fit_width(p, "nature", "single", height = h, quiet = TRUE)
bank_height <- function(plot, data = NULL, x, y, group = NULL,
                        venue = "nature", column = "single", width = NULL,
                        limits = c(1 / 3, 3), ...) {
  data <- data %||% tryCatch(plot$data, error = function(e) NULL)
  if (is.null(data) || !nrow(as.data.frame(data))) {
    cli::cli_abort("No data available; pass {.arg data} explicitly.")
  }
  # `limits` is read positionally as a lower and an upper bound, so a scalar or a
  # descending pair would silently clamp every aspect ratio to one value.
  if (!is.numeric(limits) || length(limits) != 2L || anyNA(limits) ||
      limits[1] >= limits[2]) {
    cli::cli_abort("{.arg limits} must be two increasing numbers, {.code c(min, max)}.")
  }
  aspect <- bank_aspect(as.data.frame(data), x, y, group = group, ...)
  clamped <- aspect < limits[1] || aspect > limits[2]
  if (clamped) {
    cli::cli_warn(c(
      "Banked aspect ratio {round(aspect, 3)} is outside {.arg limits} and was clamped.",
      i = "A series this {if (aspect < limits[1]) 'flat' else 'steep'} cannot be banked to 45 degrees inside a sane figure.",
      i = "Consider transforming the y axis, or splitting the series across facets."
    ))
    aspect <- clamp(aspect, limits[1], limits[2])
  }

  target_w <- width %||% venue_width(venue, column)
  dim0 <- figure_dim(plot)
  deco_w <- dim0$decoration[["width"]]
  deco_h <- dim0$decoration[["height"]]
  panel_w <- target_w - deco_w
  if (panel_w <= 0) {
    cli::cli_abort("Decoration alone needs {round(deco_w, 1)} mm, which exceeds the {round(target_w, 1)} mm target.")
  }
  out <- panel_w * aspect + deco_h

  # Banking optimises the reader's judgement of slope; the page constrains the shape.
  # Those two aims genuinely conflict, and when they do the page wins, because a
  # figure that exceeds the column depth is not a figure the journal will print.
  spec <- venue_spec(venue)
  if (out > spec$height_max) {
    cli::cli_warn(c(
      "Banking asks for {round(out, 1)} mm, above {.val {spec$name}}'s {spec$height_max} mm maximum depth.",
      i = "Height capped at the maximum; the median segment will render below 45 degrees.",
      i = "To bank within the page, use a wider column or split the series across facets."
    ))
    out <- spec$height_max
    aspect <- (out - deco_h) / panel_w
    clamped <- TRUE
  }
  structure(round(out, 3), aspect = aspect, clamped = clamped,
            venue = spec$name, width = target_w)
}
