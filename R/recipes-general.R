# General-purpose recipes: the forms a taxonomy of visualisation has to name
# even when a package would rather people used something else.

#' Bar chart
#'
#' @description
#' Provided, and built to the best standard a bar can reach: sorted by value
#' unless the categories have their own order, a baseline pinned at zero, direct
#' value labels, and no gridline competing with the bar it measures.
#'
#' It is here because a taxonomy that omits the most-drawn chart in the world is
#' not a taxonomy. It is also, for most of the jobs it is given, more ink than
#' the comparison needs: [fig_lollipop()] puts the same value on the same aligned
#' scale with a fraction of the mark, and [fig_dotstrip()] shows the observations
#' behind it rather than only their summary.
#'
#' The zero baseline is not stylistic. A bar encodes value as *length*, so
#' truncating the axis makes the length a lie about the ratio. Where the
#' interesting variation sits far from zero, that is a signal to use a dot or an
#' interval, whose position carries no such promise — which is why `baseline` is
#' the one thing this recipe will not let you move.
#'
#' @param data A data frame, or a named numeric vector.
#' @param category,value Column names when `data` is a data frame.
#' @param sort `"value"`, `"none"` for the given order, or `"alpha"`.
#' @param horizontal Draw bars horizontally, which fits long category names.
#' @param label Print the value at the end of each bar.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_bar(c(Nairobi = 412, Kampala = 366, Lusaka = 291, Harare = 240,
#'           Lilongwe = 188))
fig_bar <- function(data, category = NULL, value = NULL,
                    sort = c("value", "none", "alpha"), horizontal = TRUE,
                    label = TRUE, venue = "nature", palette = "okabe_ito") {
  sort <- match.arg(sort)
  check_bool(horizontal, "horizontal")
  check_bool(label, "label")
  if (is.numeric(data)) {
    d <- data.frame(cat = names(data), val = as.numeric(data), stringsAsFactors = FALSE)
  } else {
    if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame or a named numeric vector.")
    missing_cols <- setdiff(c(category, value), names(data))
    if (length(missing_cols)) {
      cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
    }
    d <- data.frame(cat = as.character(data[[category]]),
                    val = as.numeric(data[[value]]), stringsAsFactors = FALSE)
  }
  check_numeric(d$val, "value")
  if (any(d$val < 0, na.rm = TRUE) && any(d$val > 0, na.rm = TRUE)) {
    cli::cli_warn(c(
      "The values cross zero, so the bars point in both directions.",
      i = "A diverging bar is legitimate, but check that zero means what the reader will assume."
    ))
  }

  ord <- switch(sort,
                value = order(d$val),
                alpha = order(d$cat, decreasing = TRUE),
                none = rev(seq_len(nrow(d))))
  d$cat <- factor(d$cat, levels = d$cat[ord])

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 1)
  aes_main <- if (horizontal) ggplot2::aes(.data$val, .data$cat) else
    ggplot2::aes(.data$cat, .data$val)

  p <- ggplot2::ggplot(d, aes_main) +
    ggplot2::geom_col(fill = cols[1], width = 0.72) +
    # Always from zero. A bar's length is its value, so a truncated baseline
    # misstates every ratio on the chart.
    ggplot2::expand_limits(!!!(if (horizontal) list(x = 0) else list(y = 0)))

  if (label) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = format(.data$val, big.mark = " ", trim = TRUE)),
      hjust = if (horizontal) -0.18 else 0.5,
      vjust = if (horizontal) 0.5 else -0.6,
      size = fs(tk$type$size$small), colour = tk$ink$text, family = tk$type$family)
  }
  expand <- ggplot2::expansion(mult = c(0, if (label) 0.12 else 0.04))
  p +
    (if (horizontal) ggplot2::scale_x_continuous(expand = expand)
     else ggplot2::scale_y_continuous(expand = expand)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_vellum(venue = venue, grid = "none")
}

#' Scatterplot matrix
#'
#' @description
#' Every pair of variables against every other, which is the standard first look
#' at a multivariate dataset and the fastest way to find the pair that matters.
#'
#' It scales badly on purpose: `k` variables give `k(k-1)/2` panels, so eight
#' variables is twenty-eight panels and twelve is sixty-six. Past about eight the
#' panels are too small to read and [fig_correlation()] summarises the same
#' structure in one. That limit is enforced rather than suggested.
#'
#' The diagonal carries each variable's own distribution, because a matrix that
#' shows only the pairs hides the marginals that explain most of the odd-looking
#' ones.
#'
#' @param data A data frame.
#' @param columns Numeric columns to include.
#' @param group Optional column mapped to colour.
#' @param max_columns Largest matrix that will be drawn.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_splom(iris, columns = names(iris)[1:4], group = "Species")
fig_splom <- function(data, columns, group = NULL, max_columns = 8,
                      venue = "nature", palette = "okabe_ito") {
  check_number(max_columns, "max_columns", min = 2)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (length(columns) < 2L) cli::cli_abort("A scatterplot matrix needs at least two columns.")
  if (length(columns) > max_columns) {
    cli::cli_abort(c(
      "{length(columns)} columns give {length(columns) * (length(columns) - 1) / 2} panels.",
      i = "Past about {max_columns} the panels are too small to read anything from.",
      i = "Use {.fn fig_correlation} for the same structure in one panel, or raise {.arg max_columns}."
    ))
  }
  m <- numeric_columns(data, columns)
  g <- if (is.null(group)) NULL else as.character(data[[group]])

  # Long form with one row per (row variable, column variable, observation), so
  # the whole matrix is one facetted plot rather than a composition.
  grid <- expand.grid(row = columns, col = columns, stringsAsFactors = FALSE)
  long <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    rv <- grid$row[i]; cv <- grid$col[i]
    data.frame(row = factor(rv, levels = columns), col = factor(cv, levels = columns),
               x = m[, cv], y = m[, rv],
               group = if (is.null(g)) "" else g,
               diagonal = rv == cv, stringsAsFactors = FALSE)
  }))

  tk <- vellum_tokens(venue)
  off <- long[!long$diagonal, , drop = FALSE]
  diag_rows <- long[long$diagonal, , drop = FALSE]

  p <- ggplot2::ggplot(mapping = ggplot2::aes(.data$x, .data$y))
  p <- p + if (is.null(group)) {
    ggplot2::geom_point(data = off, colour = tk$ink$data,
                        size = tk$geom$point_small, alpha = 0.6, stroke = 0)
  } else {
    list(ggplot2::geom_point(data = off, ggplot2::aes(colour = .data$group),
                             size = tk$geom$point_small, alpha = 0.7, stroke = 0),
         scale_colour_vellum(palette))
  }
  # The diagonal shows the marginal, drawn as a density scaled into the panel.
  dens <- do.call(rbind, lapply(columns, function(cv) {
    v <- m[, cv]
    z <- stats::density(v[is.finite(v)])
    data.frame(row = factor(cv, levels = columns), col = factor(cv, levels = columns),
               x = z$x, y = min(v, na.rm = TRUE) +
                 (z$y / max(z$y)) * diff(range(v, na.rm = TRUE)) * 0.92,
               stringsAsFactors = FALSE)
  }))
  p +
    ggplot2::geom_line(data = dens, colour = tk$ink$text,
                       linewidth = tk$rule$geom) +
    ggplot2::facet_grid(row ~ col, scales = "free", switch = "y") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    theme_vellum(venue = venue, grid = "none", axis_style = "frame") +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   strip.placement = "outside",
                   panel.spacing = ggplot2::unit(1.2, "mm"))
}

#' UpSet plot
#'
#' @description
#' Set intersections as a sorted bar chart with a membership matrix beneath, for
#' the question a Venn diagram answers badly past three sets and cannot answer at
#' all past four.
#'
#' Every bar is an *exclusive* intersection: the records in exactly these sets
#' and no others. That is the property that makes the bars sum to the total and
#' the plot readable, and the one people most often get wrong by supplying
#' inclusive counts, so it is checked where a total is given.
#'
#' @param data A named numeric vector of intersection sizes, whose names give
#'   the sets in each intersection separated by `sep`.
#' @param sets Optional set sizes, drawn as the left-hand marginal.
#' @param sep Separator within an intersection name.
#' @param total Optional grand total, used to verify the intersections are
#'   exclusive.
#' @param top Number of intersections to show, largest first.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_upset(c(
#'   "PubMed" = 412, "Embase" = 233, "CENTRAL" = 96,
#'   "PubMed&Embase" = 318, "PubMed&CENTRAL" = 54,
#'   "Embase&CENTRAL" = 41, "PubMed&Embase&CENTRAL" = 129
#' ))
fig_upset <- function(data, sets = NULL, sep = "&", total = NULL, top = 12,
                      venue = "nature", palette = "okabe_ito") {
  check_string(sep, "sep")
  check_number(top, "top", min = 1)
  if (!is.numeric(data) || is.null(names(data))) {
    cli::cli_abort("{.arg data} must be a named numeric vector of intersection sizes.")
  }
  if (any(data < 0)) cli::cli_abort("Intersection sizes cannot be negative.")

  members <- strsplit(names(data), sep, fixed = TRUE)
  all_sets <- unique(unlist(members))
  if (!is.null(total)) {
    check_number(total, "total", min = 0)
    if (abs(sum(data) - total) > 0.5) {
      cli::cli_abort(c(
        "The intersections sum to {format(sum(data))}, but {.arg total} is {format(total)}.",
        i = "Every bar must be an EXCLUSIVE intersection: the records in exactly those sets and no others.",
        i = "Inclusive counts double-count every record in more than one set, which is the usual cause."
      ))
    }
  }

  d <- data.frame(name = names(data), n = as.numeric(data), stringsAsFactors = FALSE)
  d <- d[order(-d$n), , drop = FALSE]
  if (nrow(d) > top) {
    cli::cli_inform("Showing the largest {top} of {nrow(d)} intersections.")
    d <- d[seq_len(top), , drop = FALSE]
    members <- strsplit(d$name, sep, fixed = TRUE)
  } else {
    members <- strsplit(d$name, sep, fixed = TRUE)
  }
  d$i <- seq_len(nrow(d))

  # Sets ordered by total size so the matrix reads top-heavy, as UpSet's own
  # convention does.
  set_order <- if (is.null(sets)) {
    names(sort(vapply(all_sets, function(s)
      sum(d$n[vapply(members, function(m) s %in% m, logical(1))]), numeric(1)),
      decreasing = TRUE))
  } else {
    names(sort(sets, decreasing = TRUE))
  }
  dots <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    data.frame(i = i, set = factor(set_order, levels = rev(set_order)),
               on = set_order %in% members[[i]], stringsAsFactors = FALSE)
  }))
  # The connecting line spans the members of one intersection, which is what
  # distinguishes an UpSet matrix from a grid of dots.
  spans <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    idx <- which(rev(set_order) %in% members[[i]])
    if (length(idx) < 2L) return(NULL)
    data.frame(i = i, lo = min(idx), hi = max(idx))
  }))

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 1)
  n_set <- length(set_order)
  # Matrix beneath the bars in one panel: the bars occupy positive y, the matrix
  # a negative band, so the two stay in register without a composition step.
  band <- max(d$n) * 0.055
  dots$y <- -band * (as.integer(dots$set))
  if (!is.null(spans)) {
    spans$ylo <- -band * spans$lo
    spans$yhi <- -band * spans$hi
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(data = d, ggplot2::aes(.data$i, .data$n),
                      fill = cols[1], width = 0.68) +
    ggplot2::geom_text(data = d, ggplot2::aes(.data$i, .data$n,
                                              label = format(.data$n, big.mark = " ", trim = TRUE)),
                       vjust = -0.5, size = fs(tk$type$size$small),
                       colour = tk$ink$text, family = tk$type$family) +
    ggplot2::geom_point(data = dots, ggplot2::aes(.data$i, .data$y,
                                                  colour = .data$on),
                        size = tk$geom$point) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = tk$ink$text, `FALSE` = tk$ink$grid),
                                 guide = "none")
  if (!is.null(spans)) {
    p <- p + ggplot2::geom_segment(data = spans,
                                   ggplot2::aes(x = .data$i, xend = .data$i,
                                                y = .data$ylo, yend = .data$yhi),
                                   colour = tk$ink$text, linewidth = tk$rule$geom)
  }
  p +
    ggplot2::scale_y_continuous(
      breaks = c(pretty(c(0, max(d$n)), 4), -band * seq_len(n_set)),
      labels = c(format(pretty(c(0, max(d$n)), 4), big.mark = " ", trim = TRUE),
                 rev(set_order)),
      expand = ggplot2::expansion(mult = c(0.04, 0.1))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = "Each bar is an exclusive intersection.") +
    theme_vellum(venue = venue, grid = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

#' Waterfall chart
#'
#' @description
#' A decomposition: a starting total, the signed contributions that move it, and
#' the total they arrive at. Use it where the question is *what accounts for the
#' difference* — budget impact, an attributable-fraction decomposition, a
#' change in a rate broken into its components.
#'
#' The contributions must reconcile. A waterfall whose bars do not carry the
#' start to the end is drawing a decomposition that does not decompose, which the
#' shape of the chart actively conceals because the final bar is drawn wherever
#' it is told. That is checked here.
#'
#' @param data A data frame in order, one row per step.
#' @param label,value Column names.
#' @param start Value of the opening total.
#' @param end Optional closing total, verified against the contributions.
#' @param is_total Optional logical column marking rows that are totals rather
#'   than contributions, drawn from the baseline.
#' @param check Verify that the contributions reconcile.
#' @param venue Target venue.
#' @param palette Palette name; its first two colours are increases and
#'   decreases.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   step = c("Baseline", "Coverage", "Price", "Adherence", "Wastage", "Net"),
#'   value = c(0, 42, -18, 26, -9, 41),
#'   total = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
#' )
#' fig_waterfall(d, "step", "value", is_total = "total")
fig_waterfall <- function(data, label = "label", value = "value", start = 0,
                          end = NULL, is_total = NULL, check = TRUE,
                          venue = "nature", palette = "okabe_ito") {
  check_bool(check, "check")
  check_number(start, "start")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(label, value, is_total), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(label = as.character(data[[label]]),
                  value = as.numeric(data[[value]]), stringsAsFactors = FALSE)
  check_numeric(d$value, value)
  d$total <- if (is.null(is_total)) FALSE else as.logical(data[[is_total]])

  # Contributions accumulate; totals are drawn from the baseline to their own
  # value, which is what makes the opening and closing bars read as levels rather
  # than as another step.
  cum <- start
  d$ymin <- NA_real_; d$ymax <- NA_real_
  for (i in seq_len(nrow(d))) {
    if (d$total[i]) {
      d$ymin[i] <- min(0, d$value[i]); d$ymax[i] <- max(0, d$value[i])
      cum <- d$value[i]
    } else {
      d$ymin[i] <- min(cum, cum + d$value[i])
      d$ymax[i] <- max(cum, cum + d$value[i])
      cum <- cum + d$value[i]
    }
  }

  if (check) {
    contrib <- sum(d$value[!d$total])
    opening <- if (any(d$total)) d$value[which(d$total)[1]] else start
    closing <- end %||% (if (sum(d$total) > 1L) d$value[utils::tail(which(d$total), 1)] else NULL)
    if (!is.null(closing) && abs(opening + contrib - closing) > 1e-8) {
      cli::cli_abort(c(
        "The contributions do not reconcile.",
        x = sprintf("%s plus contributions of %s gives %s, but the closing total is %s.",
                    format(opening), format(contrib), format(opening + contrib),
                    format(closing)),
        i = "A waterfall claims the steps account for the difference; if they do not, the chart hides it.",
        i = "Set {.code check = FALSE} to draw it anyway."
      ))
    }
  }

  d$label <- factor(d$label, levels = d$label)
  d$kind <- ifelse(d$total, "total", ifelse(d$value >= 0, "increase", "decrease"))
  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 2)
  fills <- c(increase = cols[1], decrease = cols[2], total = tk$ink$weak)

  # Connectors between consecutive bars, which is what makes the eye read a
  # running total rather than a set of independent bars.
  link <- data.frame(
    x = seq_len(nrow(d) - 1L) + 0.36, xend = seq_len(nrow(d) - 1L) + 1 - 0.36,
    y = vapply(seq_len(nrow(d) - 1L), function(i)
      if (d$total[i]) d$ymax[i] else if (d$value[i] >= 0) d$ymax[i] else d$ymin[i],
      numeric(1))
  )

  ggplot2::ggplot(d) +
    ggplot2::geom_hline(yintercept = 0, colour = tk$ink$axis, linewidth = tk$rule$axis) +
    ggplot2::geom_segment(data = link,
                          ggplot2::aes(x = .data$x, xend = .data$xend,
                                       y = .data$y, yend = .data$y),
                          colour = tk$ink$weak, linewidth = tk$rule$grid,
                          linetype = "22") +
    ggplot2::geom_rect(ggplot2::aes(xmin = as.integer(.data$label) - 0.36,
                                    xmax = as.integer(.data$label) + 0.36,
                                    ymin = .data$ymin, ymax = .data$ymax,
                                    fill = .data$kind)) +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_len(nrow(d)), labels = levels(d$label),
                                expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_vellum(venue = venue, grid = "y")
}

#' Streamgraph
#'
#' @description
#' A stacked area centred on a floating baseline rather than pinned to zero. It
#' trades the total, which a reader can no longer measure, for the shape of each
#' band, which becomes far easier to follow when there are many of them.
#'
#' Make that trade deliberately. If the total is part of the finding,
#' [fig_stacked_area()] keeps it; a streamgraph is for the case where the
#' *composition* is the finding and the total is context. The same refusals
#' apply: negative values cannot be stacked, and a series missing at some time
#' points would be interpolated across.
#'
#' @param data A data frame in long form.
#' @param x,y,group Column names.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(15)
#' d <- expand.grid(t = 1:30, series = LETTERS[1:5])
#' d$v <- abs(stats::rnorm(150, 6, 2)) + rep(seq(1, 5), each = 30)
#' fig_stream(d, "t", "v", "series")
fig_stream <- function(data, x, y, group, venue = "nature", palette = "okabe_ito") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(x, y, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(x = data[[x]], y = as.numeric(data[[y]]),
                  group = as.character(data[[group]]), stringsAsFactors = FALSE)
  check_numeric(d$y, y)
  if (any(d$y < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Values must be non-negative; {sum(d$y < 0, na.rm = TRUE)} {?is/are} below zero.",
      i = "A stream reads each band's thickness as its value, so a negative band inverts everything outside it."
    ))
  }
  complete <- table(d$group, d$x)
  incomplete <- rownames(complete)[apply(complete, 1, function(r) any(r == 0))]
  if (length(incomplete)) {
    cli::cli_abort(c(
      "{length(incomplete)} series {?is/are} missing at some time points: {.val {incomplete}}.",
      i = "The band would be interpolated across the gap, drawing a shape through periods with no measurement."
    ))
  }

  # Inside-out ordering: the largest series in the middle, which is what keeps a
  # stream's wiggle down and is the ordering ThemeRiver introduced.
  totals <- sort(vapply(split(d$y, d$group), sum, numeric(1)), decreasing = TRUE)
  inner <- names(totals)[c(seq(1, length(totals), by = 2))]
  outer <- rev(names(totals)[c(seq(2, length(totals), by = 2))])
  d$group <- factor(d$group, levels = c(outer, inner))

  xs <- sort(unique(d$x))
  wide <- lapply(levels(d$group), function(g) {
    v <- d$y[d$group == g][order(d$x[d$group == g])]
    v
  })
  names(wide) <- levels(d$group)
  totals_at <- Reduce(`+`, wide)
  base <- -totals_at / 2
  bands <- list()
  for (g in levels(d$group)) {
    bands[[g]] <- data.frame(x = xs, ymin = base, ymax = base + wide[[g]],
                             group = g, stringsAsFactors = FALSE)
    base <- base + wide[[g]]
  }
  out <- do.call(rbind, bands)
  out$group <- factor(out$group, levels = levels(d$group))

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(out, ggplot2::aes(.data$x, ymin = .data$ymin, ymax = .data$ymax,
                                    fill = .data$group)) +
    ggplot2::geom_ribbon(colour = tk$ink$paper, linewidth = tk$rule$hairline) +
    scale_fill_vellum(palette) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  caption = "Baseline floats; the total is not measurable from this chart.") +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "right")
}

#' Bump chart
#'
#' @description
#' Rank through time, one line per entity. It answers "who overtook whom", which
#' a chart of the underlying values answers badly when the values are close and
#' the ordering is the point.
#'
#' What it discards is the margin. Two entities one rank apart may be separated
#' by a hair or by a chasm, and a bump chart draws both identically. Where the
#' size of the gap matters, plot the value; where the ordering is the finding,
#' this is the clearer figure.
#'
#' @param data A data frame in long form.
#' @param x,group Column names for time and entity.
#' @param rank Column of ranks. If `NULL`, ranks are computed from `value`.
#' @param value Column of values, used to derive ranks and to break ties.
#' @param descending Rank 1 is the largest value.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- expand.grid(year = 2019:2024, team = c("A", "B", "C", "D", "E"))
#' set.seed(16)
#' d$points <- stats::runif(30, 20, 90)
#' fig_bump(d, x = "year", group = "team", value = "points")
fig_bump <- function(data, x, group, rank = NULL, value = NULL, descending = TRUE,
                     venue = "nature", palette = "okabe_ito") {
  check_bool(descending, "descending")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(x, group, rank, value), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  if (is.null(rank) && is.null(value)) {
    cli::cli_abort("Supply either {.arg rank} or {.arg value}.")
  }
  d <- data.frame(x = data[[x]], group = as.character(data[[group]]),
                  stringsAsFactors = FALSE)
  if (is.null(rank)) {
    v <- as.numeric(data[[value]])
    check_numeric(v, value)
    d$rank <- unlist(lapply(split(v, d$x), function(z)
      rank(if (descending) -z else z, ties.method = "first")), use.names = FALSE)[
        order(order(d$x))]
    d <- d[order(d$x), , drop = FALSE]
    d$rank <- unlist(lapply(split(seq_along(v), d$x), function(idx)
      rank(if (descending) -v[idx] else v[idx], ties.method = "first")),
      use.names = FALSE)
  } else {
    d$rank <- as.numeric(data[[rank]])
    check_numeric(d$rank, rank)
  }

  # Every entity must appear at every time point, or a line jumps a period and
  # implies a rank it never held.
  seen <- table(d$group, d$x)
  gaps <- rownames(seen)[apply(seen, 1, function(r) any(r != 1))]
  if (length(gaps)) {
    cli::cli_abort(c(
      "{length(gaps)} entit{?y/ies} {?is/are} missing or duplicated at some time points: {.val {gaps}}.",
      i = "A bump line that skips a period implies a rank the entity never held."
    ))
  }

  tk <- vellum_tokens(venue)
  ends <- d[d$x == max(d$x), , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$rank, colour = .data$group)) +
    ggplot2::geom_line(linewidth = tk$rule$emphasis, lineend = "round") +
    ggplot2::geom_point(size = tk$geom$point) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = .data$group),
                       hjust = -0.3, size = fs(tk$type$size$small),
                       family = tk$type$family, show.legend = FALSE) +
    scale_colour_vellum(palette) +
    ggplot2::scale_y_reverse(breaks = seq_len(max(d$rank))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.14))) +
    ggplot2::labs(x = NULL, y = "Rank", colour = NULL) +
    theme_vellum(venue = venue, grid = "none") +
    ggplot2::theme(legend.position = "none")
}

#' Population pyramid
#'
#' @description
#' Age structure by sex, as back-to-back horizontal bars. Its shape is a
#' demographic summary in itself: a broad base is high fertility, a bulge is a
#' cohort moving through, a narrow base with a wide middle is an ageing
#' population.
#'
#' Counts and percentages answer different questions and the axis label must say
#' which. On counts, two populations of different size cannot be compared by
#' shape; on percentages they can, but the absolute burden disappears. Neither is
#' wrong and mixing them up is.
#'
#' @param data A data frame in long form.
#' @param age,count,sex Column names.
#' @param proportional Express bars as a percentage of the total population.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' ages <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")
#' d <- data.frame(
#'   age = rep(ages, 2),
#'   sex = rep(c("Female", "Male"), each = length(ages)),
#'   n = c(890, 1520, 1320, 1080, 860, 640, 430, 380,
#'         920, 1580, 1360, 1100, 850, 610, 390, 290)
#' )
#' fig_pyramid(d, "age", "n", "sex")
fig_pyramid <- function(data, age = "age", count = "count", sex = "sex",
                        proportional = FALSE, venue = "nature",
                        palette = "okabe_ito") {
  check_bool(proportional, "proportional")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(age, count, sex), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  n <- as.numeric(data[[count]])
  check_numeric(n, count)
  if (any(n < 0, na.rm = TRUE)) cli::cli_abort("Counts cannot be negative.")
  s <- as.character(data[[sex]])
  groups <- unique(s)
  if (length(groups) != 2L) {
    cli::cli_abort(c(
      "{.arg sex} must have exactly two levels; {length(groups)} {?was/were} given.",
      i = "A pyramid is back-to-back bars, so it has two sides and no more."
    ))
  }

  d <- data.frame(age = factor(as.character(data[[age]]),
                               levels = unique(as.character(data[[age]]))),
                  sex = factor(s, levels = groups), n = n, stringsAsFactors = FALSE)
  if (proportional) d$n <- 100 * d$n / sum(d$n)
  d$signed <- ifelse(d$sex == groups[1], -d$n, d$n)

  tk <- vellum_tokens(venue)
  lim <- max(abs(d$signed)) * 1.04
  ggplot2::ggplot(d, ggplot2::aes(.data$signed, .data$age, fill = .data$sex)) +
    ggplot2::geom_col(width = 0.84) +
    ggplot2::geom_vline(xintercept = 0, colour = tk$ink$paper,
                        linewidth = tk$rule$border) +
    scale_fill_vellum(palette) +
    ggplot2::scale_x_continuous(
      limits = c(-lim, lim),
      labels = function(z) if (proportional) sprintf("%.0f%%", abs(z))
                           else format(abs(z), big.mark = " ", trim = TRUE)) +
    ggplot2::labs(x = if (proportional) "Percentage of population" else "Population",
                  y = NULL, fill = NULL) +
    theme_vellum(venue = venue, grid = "none")
}

#' Swimmer plot
#'
#' @description
#' One horizontal lane per participant, showing time on treatment and the events
#' along it. It is the figure that keeps individual trajectories visible where a
#' Kaplan-Meier curve has already aggregated them away, and it is standard in
#' early-phase oncology reporting for exactly that reason.
#'
#' Lanes sorted by duration, because the unsorted version is noise and the sorted
#' version shows the distribution of response as a shape.
#'
#' Ongoing follow-up must be distinguishable from an ended one. A lane that
#' simply stops looks like an event; the arrow says the participant is still
#' being followed, which is a different fact.
#'
#' @param data A data frame, one row per participant.
#' @param id,start,end Column names for the participant and their interval.
#' @param ongoing Optional logical column marking continuing follow-up.
#' @param group Optional column mapped to lane colour.
#' @param events Optional data frame of point events with `id`, `time` and
#'   `event` columns.
#' @param sort Sort lanes by `"duration"` or keep the `"given"` order.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(17)
#' d <- data.frame(
#'   id = sprintf("P%02d", 1:14),
#'   start = 0,
#'   end = stats::runif(14, 2, 26),
#'   arm = rep(c("Dose 1", "Dose 2"), 7)
#' )
#' d$ongoing <- d$end > 18
#' ev <- data.frame(id = d$id[1:6], time = d$end[1:6] * 0.5,
#'                  event = "Response")
#' fig_swimmer(d, events = ev)
fig_swimmer <- function(data, id = "id", start = "start", end = "end",
                        ongoing = NULL, group = NULL, events = NULL,
                        sort = c("duration", "given"), venue = "nature",
                        palette = "okabe_ito") {
  sort <- match.arg(sort)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(id, start, end, ongoing, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(id = as.character(data[[id]]),
                  start = as.numeric(data[[start]]),
                  end = as.numeric(data[[end]]), stringsAsFactors = FALSE)
  check_numeric(d$start, start)
  check_numeric(d$end, end)
  if (any(d$end < d$start, na.rm = TRUE)) {
    cli::cli_abort("{sum(d$end < d$start, na.rm = TRUE)} lane{?s} end{?s/} before {?it/they} start{?s/}.")
  }
  if (anyDuplicated(d$id)) {
    cli::cli_abort("{.arg id} must be unique; a swimmer plot is one lane per participant.")
  }
  d$ongoing <- if (is.null(ongoing)) FALSE else as.logical(data[[ongoing]])
  d$group <- if (is.null(group)) "" else as.character(data[[group]])

  d$duration <- d$end - d$start
  ord <- if (sort == "duration") order(d$duration) else rev(seq_len(nrow(d)))
  d$id <- factor(d$id, levels = d$id[ord])

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(d)
  p <- p + if (is.null(group)) {
    ggplot2::geom_segment(ggplot2::aes(x = .data$start, xend = .data$end,
                                       y = .data$id, yend = .data$id),
                          colour = tk$ink$data, linewidth = 2.4, lineend = "round")
  } else {
    list(ggplot2::geom_segment(ggplot2::aes(x = .data$start, xend = .data$end,
                                            y = .data$id, yend = .data$id,
                                            colour = .data$group),
                               linewidth = 2.4, lineend = "round"),
         scale_colour_vellum(palette))
  }
  if (any(d$ongoing)) {
    p <- p + ggplot2::geom_segment(
      data = d[d$ongoing, , drop = FALSE],
      ggplot2::aes(x = .data$end, xend = .data$end + max(d$duration) * 0.045,
                   y = .data$id, yend = .data$id),
      colour = tk$ink$text, linewidth = tk$rule$geom,
      arrow = grid::arrow(length = grid::unit(1.5, "mm"), type = "closed"))
  }
  if (!is.null(events)) {
    if (!all(c("id", "time", "event") %in% names(events))) {
      cli::cli_abort("{.arg events} needs {.field id}, {.field time} and {.field event}.")
    }
    ev <- events
    ev$id <- factor(as.character(ev$id), levels = levels(d$id))
    if (anyNA(ev$id)) {
      cli::cli_abort("{.arg events} names participants not present in {.arg data}.")
    }
    p <- p + ggplot2::geom_point(data = ev,
                                 ggplot2::aes(.data$time, .data$id,
                                              shape = .data$event),
                                 size = tk$geom$point, colour = tk$ink$text) +
      ggplot2::scale_shape_manual(values = c(19, 17, 15, 18, 4))
  }
  p +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(x = "Time from enrolment", y = NULL, colour = NULL, shape = NULL,
                  caption = if (any(d$ongoing)) "Arrow: follow-up continuing." else NULL) +
    theme_vellum(venue = venue, grid = "none")
}
