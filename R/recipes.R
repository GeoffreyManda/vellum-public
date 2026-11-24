# Figure recipes.
#
# One call, one publication-ready figure, for the forms that recur constantly in
# the literature. Each recipe encodes the decisions that are easy to get wrong, and
# each is a plain ggplot object afterwards, so nothing is locked away: add layers,
# change scales, override the theme.
#
# Where a requested form is known to be perceptually weak, the recipe is still
# provided and built to the best available standard, and the reasoning is stated
# once with a pointer to the stronger alternative. Withholding a chart type is not
# advice; giving the better option alongside it is.

# One-dimensional label de-crowding.
#
# Given anchor positions and a minimum gap, return positions that keep the original
# order, respect the gap, stay inside `limits`, and move as little as possible.
# Used for direct labels around a ring, where small slices otherwise collide -- the
# failure mode that makes most donut charts unreadable exactly where they most need
# explaining.
#
# A forward pass enforces the gap, then a backward pass pulls the run back inside
# the upper limit. Both are single passes, which is exact for the small n here.
spread_labels <- function(at, min_gap, limits = range(at)) {
  n <- length(at)
  if (n < 2L) return(at)
  ord <- order(at)
  x <- at[ord]

  # If the gap cannot be honoured at all, distribute evenly and say no more.
  span <- diff(limits)
  if (min_gap * (n - 1) > span) min_gap <- span / (n - 1)

  for (i in seq(2L, n)) x[i] <- max(x[i], x[i - 1] + min_gap)
  overflow <- x[n] - limits[2]
  if (overflow > 0) {
    x <- x - overflow
    for (i in seq(n - 1L, 1L)) x[i] <- min(x[i], x[i + 1] - min_gap)
    x <- pmax(x, limits[1])
  }
  out <- numeric(n)
  out[ord] <- x
  out
}

#' Group comparison with observations, summary, and exact P values
#'
#' @description
#' The default correct answer for comparing a continuous outcome across a few
#' groups, and the figure a bar chart of means should almost always have been.
#' Shows every observation, the group mean with nested 66% and 95% intervals, and
#' the sample size per group, so a reader can see the evidence rather than a
#' summary of it.
#'
#' @param data A data frame.
#' @param x Grouping column name, as a string.
#' @param y Outcome column name, as a string.
#' @param colour Optional column mapped to colour, defaulting to `x`.
#' @param comparisons A list of length-2 character vectors naming group pairs to
#'   bracket, for example `list(c("A", "B"))`.
#' @param p_values P values for `comparisons`, in the same order. Supply these from
#'   your own analysis; the recipe deliberately does not run a test, because the
#'   correct test depends on the design and should not be chosen by a plotting
#'   function.
#' @param venue Target venue.
#' @param show_n Annotate each group with its sample size.
#' @param point `"mean"` or `"median"`.
#' @param jitter Horizontal jitter width for the observations.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_dotstrip(iris, "Species", "Sepal.Length",
#'              comparisons = list(c("setosa", "virginica")),
#'              p_values = 1.2e-8)
fig_dotstrip <- function(data, x, y, colour = NULL, comparisons = NULL, p_values = NULL,
                         venue = "nature", show_n = TRUE, point = c("mean", "median"),
                         jitter = 0.14, palette = "okabe_ito") {
  point <- match.arg(point)
  d <- data.frame(.x = factor(data[[x]]), .y = as.numeric(data[[y]]),
                  .c = factor(if (is.null(colour)) data[[x]] else data[[colour]]))
  d <- d[is.finite(d$.y), , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.x, .data$.y, colour = .data$.c)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = jitter, height = 0, seed = 7),
                        alpha = 0.40, size = ps(1.1), show.legend = FALSE) +
    geom_pointinterval(point = point, show.legend = FALSE) +
    scale_colour_vellum(palette) +
    ggplot2::labs(x = NULL, y = y) +
    theme_venue(venue)

  if (show_n) {
    n_lab <- as.data.frame(table(d$.x), stringsAsFactors = FALSE)
    names(n_lab) <- c(".x", "n")
    n_lab$lab <- paste0("n = ", n_lab$n)
    span <- diff(range(d$.y))
    if (!is.finite(span) || span == 0) span <- max(abs(d$.y), 1)
    n_lab$.y <- min(d$.y) - 0.08 * span
    tk <- vellum_tokens(venue)
    p <- p + ggplot2::geom_text(
      data = n_lab, ggplot2::aes(.data$.x, .data$.y, label = .data$lab),
      inherit.aes = FALSE, vjust = 0.5, size = fs(tk$type$size$small),
      colour = tk$ink$weak
    ) +
      ggplot2::expand_limits(y = min(d$.y) - 0.14 * span)
  }

  if (!is.null(comparisons)) {
    if (is.null(p_values) || length(p_values) != length(comparisons)) {
      cli::cli_abort("{.arg p_values} must have one value per entry in {.arg comparisons}.")
    }
    lv <- levels(d$.x)
    span <- diff(range(d$.y))
    base <- max(d$.y) + span * 0.06
    for (i in seq_along(comparisons)) {
      pos <- match(comparisons[[i]], lv)
      if (anyNA(pos)) {
        cli::cli_abort("Comparison {i} names a group not present in {.arg x}: {.val {comparisons[[i]][is.na(pos)]}}.")
      }
      p <- p + annotate_signif(sort(pos), y = base + span * 0.10 * (i - 1),
                               p = p_values[i], tip = span * 0.015,
                               size = vellum_tokens(venue)$type$size$small)
    }
    p <- p + ggplot2::expand_limits(y = base + span * (0.10 * length(comparisons) + 0.06))
  }
  p
}

#' Forest plot for effect estimates
#'
#' @description
#' Estimates with their confidence intervals against a null line, in the layout
#' meta-analyses and multi-outcome tables use. Point area is proportional to weight
#' when weights are supplied, which is the convention that lets a reader see which
#' estimates are driving a pooled result.
#'
#' @param data A data frame, one row per estimate.
#' @param estimate,lower,upper Column names for the estimate and its interval.
#' @param label Column naming each row.
#' @param group Optional column to facet by.
#' @param weight Optional column giving each estimate's weight.
#' @param null Position of the null line. Use `1` for ratio measures on a log
#'   scale, `0` for differences.
#' @param log_scale Put the x axis on a log scale. Correct for risk ratios, odds
#'   ratios, and hazard ratios, where a ratio of 2 and a ratio of 0.5 are equal and
#'   opposite and a linear axis misrepresents that.
#' @param venue Target venue.
#' @param xlab Axis title.
#' @param show_values Print each estimate and interval down the right-hand side.
#' @param palette Palette name.
#' @param point_shape Shape code for estimate markers.
#' @param point_alpha Opacity of estimate markers.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   study = c("Adeyemi 2019", "Baptiste 2020", "Chen 2021", "Duarte 2023", "Pooled"),
#'   rr = c(0.72, 0.88, 0.61, 0.95, 0.79),
#'   lo = c(0.55, 0.71, 0.44, 0.79, 0.70),
#'   hi = c(0.94, 1.09, 0.85, 1.14, 0.89),
#'   w  = c(1, 1.6, 0.8, 2.1, 4)
#' )
#' fig_forest(d, "rr", "lo", "hi", "study", weight = "w", null = 1)
fig_forest <- function(data, estimate, lower, upper, label, group = NULL,
                       weight = NULL, null = 0, log_scale = null == 1,
                       venue = "nature", xlab = NULL, show_values = TRUE,
                       palette = "okabe_ito", point_shape = 15, point_alpha = 1.0) {
  d <- data.frame(
    .e = as.numeric(data[[estimate]]),
    .lo = as.numeric(data[[lower]]),
    .hi = as.numeric(data[[upper]]),
    .lab = factor(data[[label]], levels = rev(unique(as.character(data[[label]])))),
    .w = if (is.null(weight)) 1 else as.numeric(data[[weight]]),
    .g = if (is.null(group)) NA_character_ else as.character(data[[group]])
  )
  tk <- vellum_tokens(venue)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.e, y = .data$.lab)) +
    annotate_null(null, "vertical") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$.lo, xmax = .data$.hi),
                            linewidth = tk$rule$geom, colour = tk$ink$data) +
    ggplot2::geom_point(ggplot2::aes(size = .data$.w), shape = point_shape,
                        alpha = point_alpha, colour = tk$ink$data, show.legend = FALSE) +
    # Weight is mapped to area, not to diameter: a mark that is twice as wide is
    # four times the area, and readers judge these marks by area.
    ggplot2::scale_size_area(max_size = tk$geom$point_large) +
    ggplot2::labs(x = xlab %||% estimate, y = NULL) +
    theme_venue(venue, grid = "none")

  if (log_scale) {
    p <- p + ggplot2::scale_x_log10(labels = label_vellum(2, big_mark = ""))
  } else {
    p <- p + scale_x_vellum(digits = 2)
  }

  if (show_values) {
    d$.txt <- fmt_estimate(d$.e, d$.lo, d$.hi)
    rng <- range(c(d$.lo, d$.hi), na.rm = TRUE)
    at <- if (log_scale) rng[2] * (rng[2] / rng[1])^0.10 else rng[2] + diff(rng) * 0.10
    p <- p +
      ggplot2::geom_text(data = d, ggplot2::aes(x = at, y = .data$.lab, label = .data$.txt),
                         inherit.aes = FALSE, hjust = 0, size = fs(tk$type$size$small),
                         colour = tk$ink$text, family = tk$type$family) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(1, 22, 1, 1, unit = "mm"))
  }
  if (!is.null(group)) p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$.g),
                                                   scales = "free_y", space = "free_y")
  p
}

#' Albatross plot
#'
#' @description
#' Study `p` values against sample size, for evidence that cannot be pooled. When
#' studies report the same relationship on incompatible scales, a meta-analysis is
#' not available but the pattern still is: a plot of significance against size,
#' with contours of constant effect magnitude laid over it, recovers the
#' approximate effect each study implies and shows whether small studies are
#' carrying the signal.
#'
#' The correctness criterion is the contour family. A contour converts a `p` value
#' and a sample size into an implied effect, and that conversion is specific to the
#' outcome: standardised mean differences, odds ratios and correlations do not
#' share a sampling variance. Drawing one family over another kind of outcome
#' produces a handsome picture of a wrong number, so the family is a required
#' argument rather than a default.
#'
#' Studies with no recorded direction cannot be placed on a side. They are never
#' dropped in silence; they are withheld, counted, and returned on the
#' `vellum_unplaced` attribute so that the figure legend can state them.
#'
#' @param data A data frame, one row per study.
#' @param p Column of `p` values, two-sided, in `(0, 1]`.
#' @param n Column of total sample sizes.
#' @param direction Column giving the direction of effect. Anything negative, or
#'   the strings `"lower"`, `"protective"`, `"negative"`, places a study on the
#'   left; positive values or `"higher"`, `"harmful"`, `"positive"` on the right.
#'   Rows that are missing or zero are withheld as unplaceable.
#' @param family Contour family: `"smd"` for standardised mean differences between
#'   two equal groups, `"or"` for odds ratios, `"corr"` for correlations.
#' @param contours Effect magnitudes to draw. Defaults follow the conventional
#'   small/moderate/large anchors for the family.
#' @param event_rate Assumed event proportion, used only by the `"or"` family,
#'   which needs one to convert a `p` value into an implied log odds ratio.
#' @param colour Optional column mapped to point colour, for study type.
#' @param label_contours Print the effect magnitude at the end of each contour.
#' @param left_text,right_text Directional captions under the axis.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot`. The studies that could not be placed are attached as the
#'   `vellum_unplaced` attribute, and the contour grid as `vellum_contours`.
#'
#' @references Harrison S, Jones HE, Martin RM, Lewis SJ, Higgins JPT (2017).
#'   The albatross plot: a novel graphical tool for presenting results of
#'   diversely reported studies in a systematic review. *Research Synthesis
#'   Methods* 8(3):281-289.
#' @export
#' @examples
#' d <- data.frame(
#'   study = paste("Study", 1:6),
#'   p = c(0.068, 0.03, 0.816, 0.017, 0.036, 0.5),
#'   n = c(368, 351, 194, 368, 65, 120),
#'   dir = c(1, 1, -1, -1, 1, 0)
#' )
#' fig_albatross(d, "p", "n", "dir", family = "smd")
fig_albatross <- function(data, p, n, direction, family = c("smd", "or", "corr"),
                          contours = NULL, event_rate = 0.5, colour = NULL,
                          label_contours = TRUE,
                          left_text = "Lower or protective",
                          right_text = "Higher or harmful",
                          venue = "nature", palette = "okabe_ito") {
  family <- match.arg(family)
  pv <- as.numeric(data[[p]])
  nv <- as.numeric(data[[n]])
  check_numeric(pv, "p"); check_numeric(nv, "n")
  if (any(pv <= 0 | pv > 1, na.rm = TRUE)) {
    cli::cli_abort("{.arg p} must lie in (0, 1]; a value outside it is an extraction error.")
  }
  if (any(nv <= 0, na.rm = TRUE)) cli::cli_abort("{.arg n} must be positive.")
  check_number(event_rate, "event_rate", min = 1e-6, max = 1 - 1e-6)

  # Direction decides the side. A study without one cannot be placed, and is
  # withheld rather than quietly assigned to a side.
  raw <- data[[direction]]
  side <- if (is.numeric(raw)) sign(raw) else {
    key <- tolower(trimws(as.character(raw)))
    ifelse(key %in% c("lower", "protective", "negative", "-", "down"), -1,
    ifelse(key %in% c("higher", "harmful", "positive", "+", "up"), 1, 0))
  }
  side[is.na(side)] <- 0
  placeable <- side != 0 & !is.na(pv) & !is.na(nv)

  d <- data.frame(.p = pv[placeable], .n = nv[placeable], .s = side[placeable],
                  .c = if (is.null(colour)) NA_character_ else as.character(data[[colour]])[placeable])
  d$.x <- d$.s * -log10(d$.p)

  # p implied by an effect at a given n, per Harrison et al. The sampling variance
  # is family-specific; this is the whole reason the family cannot be defaulted.
  z_of <- function(eff, nn) switch(family,
    smd  = abs(eff) * sqrt(nn) / 2,
    or   = abs(log(eff)) * sqrt(nn * event_rate * (1 - event_rate)) / 2,
    corr = abs(atanh(eff)) * sqrt(pmax(nn - 3, 1)))
  contours <- contours %||% switch(family,
    smd = c(0.2, 0.5, 0.8), or = c(1.5, 2.0, 3.0), corr = c(0.1, 0.3, 0.5))
  check_numeric(contours, "contours")

  grid <- expand.grid(eff = contours, nn = exp(seq(log(max(min(d$.n), 4)),
                                                   log(max(d$.n)), length.out = 160)))
  grid$pp <- 2 * stats::pnorm(-z_of(grid$eff, grid$nn))
  grid <- grid[grid$pp <= 1 & grid$pp > .Machine$double.eps, , drop = FALSE]
  grid$lab <- format(grid$eff, trim = TRUE)
  both <- rbind(transform(grid, .x = -log10(pp), .side = "r"),
                transform(grid, .x = log10(pp),  .side = "l"))

  tk <- vellum_tokens(venue)
  # The axis is a mirrored -log10(p): 1 at the centre, smaller p outward on both
  # sides. Labels are fixed notation, because a reader reads "0.001", not "1e-03".
  brk <- c(1, 0.01, 1e-4)
  txt <- formatC(brk, format = "fg", drop0trailing = TRUE)
  ax  <- c(rev(log10(brk)), -log10(brk)[-1])
  lab <- c(rev(txt), txt[-1])

  pl <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x, y = .data$.n)) +
    ggplot2::geom_line(data = both,
                       ggplot2::aes(x = .data$.x, y = .data$nn,
                                    group = interaction(.data$eff, .data$.side)),
                       inherit.aes = FALSE, linewidth = tk$rule$hair,
                       colour = tk$ink$weak) +
    ggplot2::geom_vline(xintercept = 0, linewidth = tk$rule$geom, colour = tk$ink$axis) +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_continuous(breaks = ax, labels = lab) +
    ggplot2::labs(x = "P value", y = "Participants", colour = NULL) +
    theme_venue(venue, grid = "none")

  pl <- pl + if (is.null(colour)) {
    ggplot2::geom_point(size = tk$geom$point, colour = tk$ink$data)
  } else {
    list(ggplot2::geom_point(ggplot2::aes(colour = .data$.c), size = tk$geom$point),
         scale_colour_vellum(palette = palette))
  }

  if (label_contours) {
    ends <- do.call(rbind, lapply(split(both[both$.side == "r", ], ~ eff),
                                  function(g) g[which.max(g$nn), ]))
    pl <- pl + ggplot2::geom_text(data = ends,
      ggplot2::aes(x = .data$.x, y = .data$nn, label = .data$lab), inherit.aes = FALSE,
      hjust = -0.25, vjust = 0.5, size = fs(tk$type$size$small), colour = tk$ink$text,
      family = tk$type$family) + ggplot2::coord_cartesian(clip = "off")
  }
  if (!is.null(left_text) || !is.null(right_text)) {
    # Directional captions sit under their own half of the axis, not in a corner:
    # a reader needs to know which side is which while looking at the points.
    lim <- max(abs(c(d$.x, ax, both$.x)), na.rm = TRUE)
    # y is in data units because the axis is log-scaled and -Inf has no logarithm.
    ylo <- min(c(d$.n, both$nn), na.rm = TRUE)
    cap <- data.frame(x = c(-lim, lim), y = ylo * 0.62, h = c(0, 1),
                      lab = c(paste0("← ", left_text), paste0(right_text, " →")))
    pl <- pl + ggplot2::geom_text(data = cap,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$lab, hjust = .data$h),
      inherit.aes = FALSE,
      vjust = 0.5, size = fs(tk$type$size$small), colour = tk$ink$weak,
      family = tk$type$family) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(2, 6, 9, 2, unit = "mm"))
  }

  attr(pl, "vellum_unplaced") <- data[!placeable, , drop = FALSE]
  attr(pl, "vellum_contours") <- grid
  pl
}

#' Raincloud plot
#'
#' @description
#' Distribution, summary, and raw observations in one mark: a half violin for the
#' shape, a narrow box for the quartiles, and the points themselves. Preferable to a
#' box plot whenever the distribution might be bimodal or skewed, because a box plot
#' shows five numbers and would draw the same box for a bimodal and a unimodal
#' sample.
#'
#' @param data A data frame.
#' @param x Grouping column name.
#' @param y Outcome column name.
#' @param venue Target venue.
#' @param palette Palette name.
#' @param width Violin width.
#' @param flip Put groups on the y axis, which usually reads better with long group
#'   names.
#' @param point_alpha Opacity of individual observation points (default 0.45).
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_raincloud(iris, "Species", "Sepal.Length")
fig_raincloud <- function(data, x, y, venue = "nature", palette = "okabe_ito",
                          width = 0.7, flip = FALSE, point_alpha = 0.45) {
  d <- data.frame(.x = factor(data[[x]]), .y = as.numeric(data[[y]]))
  d <- d[is.finite(d$.y), , drop = FALSE]
  tk <- vellum_tokens(venue)

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.x, .data$.y, fill = .data$.x, colour = .data$.x)) +
    ggplot2::geom_violin(width = width, alpha = 0.30, linewidth = tk$rule$hairline,
                         trim = FALSE, show.legend = FALSE,
                         position = ggplot2::position_nudge(x = 0.14)) +
    ggplot2::geom_boxplot(width = 0.10, outlier.shape = NA, alpha = 1,
                          linewidth = tk$rule$hairline, fill = tk$ink$paper,
                          show.legend = FALSE,
                          position = ggplot2::position_nudge(x = 0.14)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.055, height = 0, seed = 5),
                        alpha = point_alpha, size = ps(1.0), show.legend = FALSE) +
    scale_fill_vellum(palette) +
    scale_colour_vellum(palette) +
    ggplot2::labs(x = NULL, y = y) +
    theme_venue(venue)
  if (flip) p <- p + ggplot2::coord_flip()
  p
}


#' Waffle chart for part-to-whole composition
#'
#' @description
#' The recommended part-to-whole display. Each square is one unit, so the reader
#' counts rather than estimates an angle, which puts the judgement on the most
#' accurate perceptual task rather than one of the least. It also shows the
#' denominator directly, which a pie or donut hides.
#'
#' @param data A data frame, or a named numeric vector of counts.
#' @param category,value Column names when `data` is a data frame.
#' @param rows Number of rows in the grid.
#' @param unit Units per square. `NULL` chooses a round number from the total.
#' @param venue Target venue.
#' @param palette Palette name.
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_waffle(c(Complete = 62, Partial = 24, None = 14))
fig_waffle <- function(data, category = NULL, value = NULL, rows = 10, unit = NULL,
                       venue = "nature", palette = "okabe_ito") {
  if (is.numeric(data)) {
    d <- data.frame(.cat = names(data), .val = as.numeric(data), stringsAsFactors = FALSE)
  } else {
    d <- data.frame(.cat = as.character(data[[category]]),
                    .val = as.numeric(data[[value]]), stringsAsFactors = FALSE)
  }
  total <- sum(d$.val)
  unit <- unit %||% max(1, 10^floor(log10(total / 100)))
  n_sq <- round(d$.val / unit)
  cells <- rep(d$.cat, times = n_sq)
  if (!length(cells)) cli::cli_abort("No squares to draw; check {.arg value} and {.arg unit}.")
  grid_df <- data.frame(
    i = seq_along(cells) - 1L,
    cat = factor(cells, levels = d$.cat)
  )
  grid_df$row <- grid_df$i %% rows
  grid_df$col <- grid_df$i %/% rows
  tk <- vellum_tokens(venue)

  ggplot2::ggplot(grid_df, ggplot2::aes(.data$col, .data$row, fill = .data$cat)) +
    ggplot2::geom_tile(colour = tk$ink$paper, linewidth = tk$rule$hairline * 2,
                       width = 0.92, height = 0.92) +
    ggplot2::coord_equal() +
    scale_fill_vellum(palette) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  caption = sprintf("Each square is %s unit%s; total %s.",
                                    format(unit), if (unit == 1) "" else "s", format(total))) +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "right")
}

#' Donut chart
#'
#' @description
#' Provided, and built to the best standard a donut can reach: slices ordered by
#' size, direct labels carrying the value, the percentage and the denominator, small
#' categories collapsed into an explicit "Other", and an audited palette.
#'
#' It is still the weakest of the available choices, and the reason is measured
#' rather than aesthetic. Cleveland and McGill's ranking of elementary perceptual
#' tasks puts position along a common aligned scale at the top for accuracy and
#' angle and area near the bottom. A donut asks the reader to judge angle and arc
#' length, and removes the centre where an aligned baseline would be. A one-line
#' change to [fig_waffle()] or [fig_dotstrip()] moves the same comparison onto the
#' most accurate channel available.
#'
#' The note is emitted once per session. Silence it permanently with
#' `options(vellum.quiet_chart_advice = TRUE)`.
#'
#' @param data A data frame, or a named numeric vector of counts.
#' @param category,value Column names when `data` is a data frame.
#' @param hole Inner radius as a fraction of the outer, from 0 (a pie) to just
#'   below 1.
#' @param max_slices Categories to show before collapsing the remainder into
#'   "Other". Beyond about five slices, angular comparison fails completely.
#' @param venue Target venue.
#' @param palette Palette name.
#' @param label `"value"`, `"percent"`, `"both"`, or `"none"`.
#' @param total_label Print the denominator in the centre, which is the one thing
#'   the hole is good for.
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_donut(c(Complete = 62, Partial = 24, None = 14))
fig_donut <- function(data, category = NULL, value = NULL, hole = 0.55,
                      max_slices = 5, venue = "nature", palette = "okabe_ito",
                      label = c("both", "value", "percent", "none"),
                      total_label = TRUE) {
  label <- match.arg(label)
  if (!isTRUE(getOption("vellum.quiet_chart_advice", FALSE))) {
    inform_once("donut-advice", c(
      "!" = "A donut puts the comparison on angle and area, the least accurate perceptual channels.",
      i = "Cleveland & McGill (1984) rank position on a common scale most accurate; angle and area near the bottom.",
      i = "For part-to-whole, {.fn fig_waffle} lets readers count instead of estimate.",
      i = "For comparing categories, {.fn fig_dotstrip} or a sorted bar puts them on an aligned scale.",
      i = "Silence this with {.code options(vellum.quiet_chart_advice = TRUE)}."
    ))
  }
  check_number(hole, "hole", min = 0, max = 0.95)

  if (is.numeric(data)) {
    d <- data.frame(cat = names(data), val = as.numeric(data), stringsAsFactors = FALSE)
  } else {
    d <- data.frame(cat = as.character(data[[category]]),
                    val = as.numeric(data[[value]]), stringsAsFactors = FALSE)
  }
  d <- d[is.finite(d$val) & d$val > 0, , drop = FALSE]
  d <- d[order(-d$val), , drop = FALSE]
  if (nrow(d) > max_slices) {
    keep <- d[seq_len(max_slices - 1L), , drop = FALSE]
    other <- data.frame(cat = sprintf("Other (%d categories)", nrow(d) - max_slices + 1L),
                        val = sum(d$val[seq(max_slices, nrow(d))]), stringsAsFactors = FALSE)
    d <- rbind(keep, other)
  }
  total <- sum(d$val)
  d$pct <- 100 * d$val / total
  d$cat <- factor(d$cat, levels = d$cat)
  d$upper <- cumsum(d$val)
  d$lower <- d$upper - d$val
  d$mid <- (d$lower + d$upper) / 2

  d$txt <- switch(
    label,
    none = "",
    value = paste0(d$cat, "\n", format(d$val)),
    percent = paste0(d$cat, "\n", sprintf("%.1f%%", d$pct)),
    both = paste0(d$cat, "\n", format(d$val), " (", sprintf("%.1f%%", d$pct), ")")
  )
  tk <- vellum_tokens(venue)

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = hole, xmax = 1, ymin = .data$lower, ymax = .data$upper,
                   fill = .data$cat),
      colour = tk$ink$paper, linewidth = tk$rule$hairline * 2) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(0, 1.75) +
    scale_fill_vellum(palette) +
    ggplot2::labs(fill = NULL, x = NULL, y = NULL) +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "none")

  if (label != "none") {
    # Direct labels outside the ring, so no legend lookup is needed and the reader
    # gets the number the angle cannot convey.
    #
    # Small slices put their midpoints close together, and two-line labels then
    # overlap -- which is the failure mode that makes most donut charts unreadable
    # at the exact moment they most need explaining. Push the label anchors apart
    # to a minimum angular separation before drawing.
    d$label_at <- spread_labels(d$mid, min_gap = 0.10 * total, limits = c(0, total))
    p <- p + ggplot2::geom_text(
      data = d, ggplot2::aes(x = 1.06, y = .data$label_at, label = .data$txt),
      hjust = 0, size = fs(tk$type$size$small), colour = tk$ink$text,
      family = tk$type$family, lineheight = 0.95)
  }
  if (total_label) {
    p <- p + ggplot2::annotate("text", x = 0, y = 0, label = paste0("n = ", format(total)),
                               size = fs(tk$type$size$axis_text), colour = tk$ink$text,
                               family = tk$type$family)
  }
  p
}

#' Heatmap with an ordered, perceptually uniform scale
#'
#' @description
#' A matrix display with a linearised colour ramp, a framed panel, and no grid. Where
#' the values are deviations from a reference, set `diverging = TRUE` and give
#' `midpoint`, which anchors the neutral colour to the value that actually means
#' "no difference" rather than to the middle of the observed range.
#'
#' @param data A matrix, or a data frame in long form.
#' @param row,col,value Column names when `data` is a long data frame.
#' @param palette Palette name.
#' @param diverging Use a diverging scale anchored at `midpoint`.
#' @param midpoint The value the neutral colour represents.
#' @param venue Target venue.
#' @param cluster Order rows and columns by hierarchical clustering of the values,
#'   which makes block structure visible. Set `FALSE` to keep the given order when
#'   it is meaningful, such as time.
#' @param label_values Print the value in each cell.
#' @param legend_title Legend title.
#' @return A `ggplot` object.
#' @export
#' @examples
#' m <- matrix(rnorm(60), nrow = 10,
#'             dimnames = list(paste0("gene", 1:10), paste0("s", 1:6)))
#' fig_heatmap(m, diverging = TRUE, midpoint = 0)
fig_heatmap <- function(data, row = NULL, col = NULL, value = NULL,
                        palette = if (diverging) "strata" else "abyss",
                        diverging = FALSE, midpoint = 0, venue = "nature",
                        cluster = FALSE, label_values = FALSE,
                        legend_title = NULL) {
  if (is.matrix(data)) {
    m <- data
  } else {
    lv_r <- unique(as.character(data[[row]]))
    lv_c <- unique(as.character(data[[col]]))
    m <- matrix(NA_real_, length(lv_r), length(lv_c), dimnames = list(lv_r, lv_c))
    m[cbind(match(as.character(data[[row]]), lv_r),
            match(as.character(data[[col]]), lv_c))] <- as.numeric(data[[value]])
  }
  if (is.null(rownames(m))) rownames(m) <- paste0("r", seq_len(nrow(m)))
  if (is.null(colnames(m))) colnames(m) <- paste0("c", seq_len(ncol(m)))

  if (cluster && nrow(m) > 2L && ncol(m) > 2L) {
    ok <- stats::complete.cases(m)
    if (sum(ok) > 2L) {
      ro <- stats::hclust(stats::dist(m[ok, , drop = FALSE]))$order
      m <- m[c(which(ok)[ro], which(!ok)), , drop = FALSE]
    }
    co <- stats::hclust(stats::dist(t(m[stats::complete.cases(m), , drop = FALSE])))$order
    m <- m[, co, drop = FALSE]
  }

  long <- data.frame(
    .row = factor(rep(rownames(m), times = ncol(m)), levels = rev(rownames(m))),
    .col = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)),
    .val = as.vector(m)
  )
  tk <- vellum_tokens(venue)

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$.col, .data$.row, fill = .data$.val)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = NULL, y = NULL, fill = legend_title %||% "Value") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    theme_venue(venue, axis_style = "frame", ticks = "out")

  p <- p + if (diverging) {
    scale_fill_vellum_div(palette, midpoint = midpoint)
  } else {
    scale_fill_vellum_c(palette)
  }

  if (label_values) {
    long$.txt <- label_vellum(1, big_mark = "")(long$.val)
    # Label ink flips with cell lightness so text stays readable across the ramp.
    p <- p + ggplot2::geom_text(
      data = long, ggplot2::aes(label = .data$.txt), inherit.aes = TRUE,
      size = fs(tk$type$size$small), family = tk$type$family,
      colour = "white", show.legend = FALSE)
  }
  p
}
