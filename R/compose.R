# Composition and physical geometry.
#
# The problem this file solves is that ggplot2 sizes panels relatively: a panel
# takes whatever space is left after axes, titles, and legends. Two figures in the
# same paper, exported at the same width, will therefore have different panel
# sizes if one has longer tick labels than the other, and the reader sees two
# different scales presented as if they were the same. Journals notice this;
# readers feel it without being able to name it.
#
# ggplot2 4.0 exposes `panel.widths` and `panel.heights`, which set the panel area
# absolutely. Everything here builds on that.

#' Measure a plot's rendered size
#'
#' @description
#' Builds the plot and measures the resulting grob, returning the physical size it
#' will occupy and how that size divides between the panel area and everything
#' else. Use it to find out where a figure's width is actually going before
#' blaming the panel.
#'
#' @param plot A `ggplot` object.
#' @param units `"mm"` or `"in"`.
#'
#' @return A list with `total`, `panel`, and `decoration` widths and heights, plus
#'   `panel_fixed`, a logical pair saying whether each dimension of the panel is
#'   absolute. Where a panel dimension is free -- ggplot2's default, in which the
#'   panel expands to fill whatever device it is drawn on -- the panel and total
#'   sizes for that dimension are `NA`, because the plot genuinely has no intrinsic
#'   size in that direction until a device is chosen. The decoration is always
#'   reported, since that part is intrinsic, and it is the number [fit_width()]
#'   needs.
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
#' figure_dim(p)$decoration        # intrinsic; the panel is still free
#' figure_dim(fit_width(p, "nature", "single", quiet = TRUE))$total
figure_dim <- function(plot, units = c("mm", "in")) {
  units <- match.arg(units)
  gt <- as_gtable(plot)
  is_panel <- grepl("^panel", gt$layout$name)
  panel_cols <- unique(gt$layout$l[is_panel])
  panel_rows <- unique(gt$layout$t[is_panel])

  # A null unit converts to zero length, so summing blindly would silently report
  # a free panel as a zero-width one and a partial total as a complete one.
  free <- function(u, idx) length(idx) == 0L || any(grid::unitType(u[idx]) == "null")
  w_free <- free(gt$widths, panel_cols)
  h_free <- free(gt$heights, panel_rows)

  # `u[-integer(0)]` drops everything rather than nothing, so a gtable with no
  # panel at all has to be summed whole: all of it is decoration.
  except <- function(u, idx) if (length(idx)) u[-idx] else u
  deco_w <- as_mm(sum(except(gt$widths, panel_cols)), "width")
  deco_h <- as_mm(sum(except(gt$heights, panel_rows)), "height")
  panel_w <- if (w_free) NA_real_ else as_mm(sum(gt$widths[panel_cols]), "width")
  panel_h <- if (h_free) NA_real_ else as_mm(sum(gt$heights[panel_rows]), "height")

  k <- if (units == "mm") 1 else 1 / 25.4
  list(
    units = units,
    total = c(width = (deco_w + panel_w) * k, height = (deco_h + panel_h) * k),
    panel = c(width = panel_w * k, height = panel_h * k),
    decoration = c(width = deco_w * k, height = deco_h * k),
    panel_fixed = c(width = !w_free, height = !h_free)
  )
}

as_gtable <- function(plot, call = rlang::caller_env()) {
  if (inherits(plot, "gtable")) return(plot)
  if (inherits(plot, "patchwork")) {
    check_installed("patchwork", "to measure a composed figure", call = call)
    return(patchwork::patchworkGrob(plot))
  }
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a {.cls ggplot}, {.cls patchwork}, or {.cls gtable}.",
                   call = call)
  }
  ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
}

#' Fix the panel area to an absolute size
#'
#' @description
#' Sets the panel region to an exact physical size, so panels are identical across
#' separate figures regardless of how long their axis labels are. This is the
#' setting that makes a set of figures in one paper look like a set.
#'
#' Note that this fixes the *panel*, so the total figure size becomes panel plus
#' decoration and varies between plots. If you need the total size fixed instead,
#' use [fit_width()].
#'
#' @param plot A `ggplot` object.
#' @param width,height Panel-area size in millimetres. `NULL` leaves that
#'   dimension free.
#' @return The plot, with absolute panel geometry.
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
#' figure_dim(set_panel_area(p, width = 50, height = 35))$panel
set_panel_area <- function(plot, width = NULL, height = NULL) {
  if (!is.null(width)) check_number(width, "width", min = 1)
  if (!is.null(height)) check_number(height, "height", min = 1)
  plot + ggplot2::theme(
    panel.widths = if (is.null(width)) NULL else u_mm(width),
    panel.heights = if (is.null(height)) NULL else u_mm(height)
  )
}

#' Fit a figure to an exact total size
#'
#' @description
#' Solves the question every journal submission asks and no plotting library
#' answers directly: *make the whole figure exactly 89 mm wide.*
#'
#' Setting a device to 89 mm gives a 89 mm canvas, not a 89 mm figure -- the plot
#' then stretches to fill it, and the panel ends up at whatever size is left after
#' the axis labels have taken their share. This function instead measures the
#' decoration by building the plot, subtracts it from the target, and sets the
#' panel area to the difference. It iterates, because changing the panel size can
#' change how many break labels fit and therefore the decoration, and it stops when
#' the measured total is within `tol` of the target.
#'
#' @param plot A `ggplot` object.
#' @param venue Venue whose column widths to use. See [venues()].
#' @param column Column name (`"single"`, `"onehalf"`, `"double"`) or a width in
#'   millimetres.
#' @param height Total height in millimetres. `NULL` derives it from `ratio`.
#' @param ratio Height-to-width ratio when `height` is not given. The default of
#'   0.72 suits a single panel; multi-panel plates usually want more.
#' @param tol Tolerance in millimetres.
#' @param max_iter Maximum refinement passes.
#' @param quiet Suppress the achieved-size message.
#'
#' @return The plot with absolute panel geometry, carrying the achieved size in the
#'   `vellum_fit` attribute.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
#' fitted <- fit_width(p, "nature", "single")
#' figure_dim(fitted)$total
#' @export
fit_width <- function(plot, venue = "nature", column = "single", height = NULL,
                      ratio = 0.72, tol = 0.25, max_iter = 8L, quiet = FALSE) {
  if (inherits(plot, "patchwork")) {
    cli::cli_abort(c(
      "{.fn fit_width} works on a single {.cls ggplot}, not a composed {.cls patchwork}.",
      i = "For a composed figure, pass the target size to {.fn save_figure} instead, which sets the device exactly.",
      i = "To make panels identical across a plate, apply {.fn set_panel_area} to each panel before composing."
    ))
  }
  target_w <- venue_width(venue, column)
  target_h <- height %||% (target_w * ratio)
  spec <- venue_spec(venue)
  if (target_h > spec$height_max) {
    cli::cli_warn("Requested height {round(target_h, 1)} mm exceeds {.val {spec$name}}'s maximum of {spec$height_max} mm.")
  }

  # First pass with the panel free, to learn the decoration.
  dim0 <- figure_dim(plot)
  deco_w <- dim0$decoration[["width"]]
  deco_h <- dim0$decoration[["height"]]
  panel_w <- target_w - deco_w
  panel_h <- target_h - deco_h

  if (panel_w <= 0 || panel_h <= 0) {
    cli::cli_abort(c(
      "The decoration alone needs {round(deco_w, 1)} x {round(deco_h, 1)} mm, which exceeds the {round(target_w, 1)} x {round(target_h, 1)} mm target.",
      i = "Shorten axis titles, wrap tick labels with {.fn label_wrap}, move the legend, or use a wider column."
    ))
  }

  out <- plot
  achieved <- c(width = NA_real_, height = NA_real_)
  for (i in seq_len(max_iter)) {
    out <- set_panel_area(plot, width = panel_w, height = panel_h)
    d <- figure_dim(out)
    achieved <- d$total
    dw <- target_w - achieved[["width"]]
    dh <- target_h - achieved[["height"]]
    if (abs(dw) < tol && abs(dh) < tol) break
    panel_w <- panel_w + dw
    panel_h <- panel_h + dh
    if (panel_w <= 0 || panel_h <= 0) {
      cli::cli_abort("Could not fit the figure: the panel would need a non-positive size.")
    }
  }
  if (abs(target_w - achieved[["width"]]) >= tol || abs(target_h - achieved[["height"]]) >= tol) {
    cli::cli_warn(c(
      "Did not converge to within {tol} mm after {max_iter} passes.",
      i = "Achieved {round(achieved[['width']], 2)} x {round(achieved[['height']], 2)} mm against a target of {round(target_w, 2)} x {round(target_h, 2)} mm."
    ))
  } else if (!quiet) {
    cli::cli_inform("Fitted to {round(achieved[['width']], 2)} x {round(achieved[['height']], 2)} mm (panel {round(panel_w, 2)} x {round(panel_h, 2)} mm).")
  }
  attr(out, "vellum_fit") <- list(target = c(width = target_w, height = target_h),
                                 achieved = achieved,
                                 panel = c(width = panel_w, height = panel_h),
                                 venue = spec$name, column = column)
  out
}

#' Compose a multi-panel figure with venue-correct panel labels
#'
#' @description
#' Assembles panels and labels them in the target venue's own convention: Nature
#' uses 8 pt bold upright lowercase `a`, `b`, `c`, while Science and Cell Press use
#' bold uppercase. Getting this wrong is one of the few figure errors a production
#' editor will always send back.
#'
#' Labels are placed in the margin rather than inside the panel, so they cannot
#' collide with the data.
#'
#' @param ... Plots to compose, or a single list of plots.
#' @param venue Venue whose labelling convention to follow.
#' @param ncol,nrow Layout. One is enough; the other is inferred.
#' @param widths,heights Relative panel sizes, passed to `patchwork`.
#' @param tags Explicit tag vector, overriding the venue convention. Use `NULL` for
#'   no tags.
#' @param collect_legends Merge identical legends into one, which is almost always
#'   what a multi-panel figure wants.
#' @param design Optional patchwork design string or area specification.
#' @param axes Which repeated axes to collect.
#' @param axis_titles Which repeated axis titles to collect.
#' @param contract Optional [figure_contract()] for the complete composition.
#'
#' @return A `patchwork` object.
#' @export
#' @examples
#' library(ggplot2)
#' a <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
#' b <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot() + theme_vellum()
#' compose_figure(a, b, venue = "nature", ncol = 2)
compose_figure <- function(..., venue = "nature", ncol = NULL, nrow = NULL,
                           widths = NULL, heights = NULL, tags = NULL,
                           collect_legends = TRUE, design = NULL,
                           axes = c("keep", "collect", "collect_x", "collect_y"),
                           axis_titles = axes, contract = NULL) {
  check_installed("patchwork", "to compose multi-panel figures")
  axes <- match.arg(axes)
  axis_titles <- match.arg(
    axis_titles, c("keep", "collect", "collect_x", "collect_y")
  )
  plots <- list(...)
  if (length(plots) == 1L && is.list(plots[[1]]) && !inherits(plots[[1]], "ggplot")) {
    plots <- plots[[1]]
  }
  spec <- venue_spec(venue)
  warn_unverified(spec)
  n <- length(plots)

  tag_levels <- if (is.null(tags)) {
    if (spec$tag_case == "lower") "a" else "A"
  } else NULL

  patch <- patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow,
                                 widths = widths, heights = heights,
                                 guides = if (collect_legends) "collect" else "keep",
                                 design = design, axes = axes,
                                 axis_titles = axis_titles)

  tk <- vellum_tokens(venue)
  ann <- patchwork::plot_annotation(
    tag_levels = tag_levels,
    theme = ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = tk$type$family, size = spec$tag_size,
        face = spec$tag_face, colour = tk$ink$text, hjust = 0, vjust = 1
      ),
      plot.tag.position = "topleft",
      plot.tag.location = "margin"
    )
  )
  out <- patch + ann
  if (!is.null(tags)) {
    if (length(tags) != n) {
      cli::cli_abort("{.arg tags} has {length(tags)} entries for {n} panels.")
    }
    out <- patch + patchwork::plot_annotation(tag_levels = list(tags), theme = ann$theme)
  }
  attr(out, "vellum_composition") <- list(
    venue = venue, panels = n, widths = widths, heights = heights,
    design = design, axes = axes, axis_titles = axis_titles,
    collect_legends = collect_legends
  )
  if (!is.null(contract)) out <- attach_contract(out, contract)
  out
}

#' Show naive and corrected versions of a figure side by side
#'
#' @description
#' The demonstration form. Superiority in a figure is a claim about perception, and
#' a claim about perception is settled by looking, at final size, not by assertion.
#'
#' The built-in comparison is the one that matters most often in the life sciences:
#' a bar of the group mean with standard-error whiskers, against the same data
#' shown as observations with the mean and its 95% confidence interval. The bar
#' chart hides the sample size, hides the spread, implies that zero is meaningful,
#' and encodes a mean as an area; the second shows all of it in the same space.
#'
#' @param data A data frame. Defaults to a small simulated example.
#' @param x,y Column names, as strings.
#' @param venue Venue to build both panels for, so the comparison is at final size.
#' @return A `patchwork` object with two panels.
#' @export
#' @examples
#' superiority_panel()
superiority_panel <- function(data = NULL, x = "group", y = "value", venue = "nature") {
  check_installed("patchwork", "to show the comparison side by side")
  if (is.null(data)) {
    set.seed(11)
    data <- data.frame(
      group = rep(c("Control", "Treated"), times = c(24, 22)),
      value = c(stats::rnorm(24, 10, 2.6), c(stats::rnorm(18, 12.4, 2.1), 19.5, 20.2, 4.1, 4.6))
    )
  }
  d <- data.frame(group = data[[x]], value = data[[y]])

  sem <- function(v) stats::sd(v) / sqrt(length(v))
  agg <- stats::aggregate(value ~ group, d, function(v) c(m = mean(v), s = sem(v)))
  agg <- data.frame(group = agg$group, m = agg$value[, "m"], s = agg$value[, "s"])

  naive <- ggplot2::ggplot(agg, ggplot2::aes(.data$group, .data$m)) +
    ggplot2::geom_col(width = 0.6, fill = "grey70", colour = "black",
                      linewidth = lw(0.4)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$m, ymax = .data$m + .data$s),
                           width = 0.18, linewidth = lw(0.4)) +
    ggplot2::labs(x = NULL, y = "Value", title = "Bar of mean with SEM",
                  subtitle = "n hidden, spread hidden, zero implied") +
    theme_vellum(venue = venue)

  n_lab <- as.data.frame(table(d$group), stringsAsFactors = FALSE)
  names(n_lab) <- c("group", "n")
  n_lab$lab <- paste0("n = ", n_lab$n)

  better <- ggplot2::ggplot(d, ggplot2::aes(.data$group, .data$value)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.13, height = 0, seed = 3),
                        alpha = 0.4, size = ps(1.1)) +
    geom_pointinterval(.width = c(0.66, 0.95), point = "mean") +
    ggplot2::geom_text(data = n_lab, ggplot2::aes(.data$group, y = -Inf, label = .data$lab),
                       vjust = -0.8, size = fs(5), colour = "#767676") +
    ggplot2::labs(x = NULL, y = "Value", title = "Observations with mean and 95% CI",
                  subtitle = "n visible, spread visible, no false baseline") +
    theme_vellum(venue = venue)

  patchwork::wrap_plots(naive, better, ncol = 2)
}
