# Composition recipes: flows and part-to-whole where the parts have width.

# Kahn's algorithm, returning each node's longest-path depth from a source.
# Longest path rather than shortest, because a Sankey stage is "everything that
# cannot happen until the previous stage has", and a node reachable by both a
# short and a long route belongs at the far end of the longer one.
flow_stages <- function(from, to, call = rlang::caller_env()) {
  nodes <- unique(c(from, to))
  indeg <- vapply(nodes, function(n) sum(to == n), integer(1))
  depth <- stats::setNames(integer(length(nodes)), nodes)
  queue <- nodes[indeg == 0L]
  if (!length(queue)) {
    cli::cli_abort(c(
      "Every node has an inflow, so the flow graph is cyclic.",
      i = "A Sankey diagram lays stages out left to right; a cycle has no leftmost stage.",
      i = "Use {.fn fig_pathway} or {.fn diagram} for a graph that loops back."
    ), call = call)
  }
  seen <- character(0)
  while (length(queue)) {
    u <- queue[1]; queue <- queue[-1]; seen <- c(seen, u)
    for (v in to[from == u]) {
      depth[[v]] <- max(depth[[v]], depth[[u]] + 1L)
      indeg[[v]] <- indeg[[v]] - 1L
      if (indeg[[v]] == 0L) queue <- c(queue, v)
    }
  }
  if (length(seen) < length(nodes)) {
    cli::cli_abort(c(
      "The flow graph contains a cycle through {.val {setdiff(nodes, seen)}}.",
      i = "A Sankey diagram lays stages out left to right; a cycle has no leftmost stage."
    ), call = call)
  }
  depth
}

# The raised cosine that both edges of a ribbon follow. A straight line reads as
# a connector rather than a quantity, and a cubic needs a spline package to
# guarantee the two edges never cross. Both edges share the easing, so the
# ribbon's vertical thickness is the flow's value at every x, not just the ends.
flow_ribbon <- function(x0, x1, y_from, y_to, thickness, n = 64) {
  t <- seq(0, 1, length.out = n)
  ease <- (1 - cos(pi * t)) / 2
  xs <- x0 + (x1 - x0) * t
  edge <- y_from + (y_to - y_from) * ease
  data.frame(x = c(xs, rev(xs)), y = c(edge, rev(edge + thickness)))
}

#' Sankey diagram
#'
#' @description
#' Flows between states, drawn so that the width of every ribbon is the quantity
#' it carries and the stages read left to right. Use it when the question is
#' where a quantity goes, not how much of it there is: a Sankey is the only
#' common chart in which the reader can follow one unit through several
#' transitions without arithmetic.
#'
#' What separates a Sankey from a decorative flow picture is conservation. If a
#' node's inflow and outflow disagree, the diagram is drawing a quantity that
#' appears or vanishes between stages, and every ribbon downstream of it is the
#' wrong width. That is checked here rather than assumed, because it is invisible
#' once drawn -- the picture looks equally convincing either way.
#'
#' Losses are legitimate, but they must be stated. Route them to an explicit
#' node (`"lost to follow-up"`, `"waste heat"`) so the reader can see the
#' quantity leave rather than infer it from a ribbon that narrows.
#'
#' @param data A data frame of flows, one row per edge.
#' @param from,to,value Column names giving each flow's source node, target node,
#'   and magnitude.
#' @param colour_by Colour ribbons by their `"source"` or their `"target"`.
#'   Source answers "where did this go", target answers "where did this come
#'   from"; they are different questions and the figure can only answer one.
#' @param check Verify that inflow equals outflow at every intermediate node.
#' @param tolerance Relative tolerance for that check.
#' @param label Draw node names and totals.
#' @param alpha Ribbon opacity. Ribbons cross, so some transparency is needed to
#'   see the one underneath; too little and the colours stop being the palette's.
#' @param node_width Node bar width, as a fraction of the panel.
#' @param gap Vertical space between nodes in a stage, as a fraction of the
#'   largest stage total.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' flows <- data.frame(
#'   from  = c("Screened", "Screened", "Enrolled", "Enrolled", "Enrolled"),
#'   to    = c("Enrolled", "Ineligible", "Completed", "Withdrew", "Lost"),
#'   value = c(180, 120, 140, 22, 18)
#' )
#' fig_sankey(flows)
fig_sankey <- function(data, from = "from", to = "to", value = "value",
                       colour_by = c("source", "target"), check = TRUE,
                       tolerance = 1e-6, label = TRUE, alpha = 0.85,
                       node_width = 0.045, gap = 0.03,
                       venue = "nature", palette = "okabe_ito") {
  colour_by <- match.arg(colour_by)
  check_bool(check, "check")
  check_bool(label, "label")
  check_number(alpha, "alpha", min = 0, max = 1)
  check_number(node_width, "node_width", min = 0, max = 0.3)
  check_number(gap, "gap", min = 0, max = 0.5)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame of flows.")
  missing_cols <- setdiff(c(from, to, value), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  f <- as.character(data[[from]])
  t <- as.character(data[[to]])
  v <- as.numeric(data[[value]])
  check_numeric(v, value)
  if (any(v <= 0)) {
    cli::cli_abort(c(
      "Flows must be positive; {sum(v <= 0)} {?is/are} zero or negative.",
      i = "A ribbon's width is its value, so a negative flow has no width to draw.",
      i = "Reverse the direction of the edge instead of negating it."
    ))
  }
  if (any(f == t)) {
    cli::cli_abort("A flow cannot leave and enter the same node: {.val {unique(f[f == t])}}.")
  }

  depth <- flow_stages(f, t)
  nodes <- names(depth)
  inflow <- vapply(nodes, function(n) sum(v[t == n]), numeric(1))
  outflow <- vapply(nodes, function(n) sum(v[f == n]), numeric(1))

  intermediate <- inflow > 0 & outflow > 0
  if (check && any(intermediate)) {
    imbalance <- abs(inflow - outflow) / pmax(inflow, outflow)
    bad <- nodes[intermediate & imbalance > tolerance]
    if (length(bad)) {
      cli::cli_abort(c(
        "Flow is not conserved at {length(bad)} node{?s}.",
        stats::setNames(sprintf("%s: %s in, %s out, %s unaccounted for.",
                                bad, format(inflow[bad]), format(outflow[bad]),
                                format(inflow[bad] - outflow[bad])),
                        rep("x", length(bad))),
        i = "Every unit entering a node must leave it or end there.",
        i = "Route losses to an explicit node so the reader can see them leave.",
        i = "Set {.code check = FALSE} to draw it anyway, but the widths will be wrong."
      ))
    }
  }

  size <- pmax(inflow, outflow)
  stage_of <- depth[nodes]
  stage_total <- vapply(split(size, stage_of), sum, numeric(1))
  span <- max(stage_total)
  pad <- gap * span

  # Node order within a stage follows first appearance in the data, so the caller
  # controls it by ordering their rows rather than by an argument nobody reads.
  appearance <- match(nodes, c(f, t))
  layout <- do.call(rbind, lapply(sort(unique(stage_of)), function(s) {
    in_stage <- nodes[stage_of == s]
    in_stage <- in_stage[order(appearance[match(in_stage, nodes)])]
    h <- size[in_stage]
    tops <- cumsum(c(0, utils::head(h, -1) + pad))
    # Centre each stage vertically, so stages with different totals do not hang
    # from a shared top edge and imply a loss that is not there.
    offset <- (span - (sum(h) + pad * (length(h) - 1L))) / 2
    data.frame(node = in_stage, stage = s, top = tops + offset,
               bottom = tops + offset + h, stringsAsFactors = FALSE)
  }))
  layout$mid <- (layout$top + layout$bottom) / 2
  n_stage <- length(unique(layout$stage))
  layout$x <- if (n_stage == 1L) 0.5 else layout$stage / (n_stage - 1L)
  layout$x <- layout$x * (1 - node_width) + node_width / 2
  rownames(layout) <- layout$node

  # Order the ribbons leaving and entering each node by where the other end sits,
  # which is the cheapest way to keep crossings down without a layout solver.
  edges <- data.frame(from = f, to = t, value = v, stringsAsFactors = FALSE)
  edges$from_mid <- layout[edges$from, "mid"]
  edges$to_mid <- layout[edges$to, "mid"]
  out_order <- order(layout[edges$from, "stage"], edges$from_mid, edges$to_mid)
  edges <- edges[out_order, , drop = FALSE]

  out_used <- stats::setNames(layout[nodes, "top"], nodes)
  in_used <- out_used
  # Pre-allocated: assigning into a column that does not exist yet grows it only
  # to the index written, and the incoming pass writes out of order.
  edges$y_from <- NA_real_
  edges$y_to <- NA_real_
  for (i in seq_len(nrow(edges))) {
    edges$y_from[i] <- out_used[[edges$from[i]]]
    out_used[[edges$from[i]]] <- edges$y_from[i] + edges$value[i]
  }
  in_order <- order(edges$to_mid, edges$from_mid)
  for (i in in_order) {
    edges$y_to[i] <- in_used[[edges$to[i]]]
    in_used[[edges$to[i]]] <- edges$y_to[i] + edges$value[i]
  }

  ribbons <- do.call(rbind, lapply(seq_len(nrow(edges))[order(-edges$value)], function(i) {
    poly <- flow_ribbon(
      x0 = layout[edges$from[i], "x"] + node_width / 2,
      x1 = layout[edges$to[i], "x"] - node_width / 2,
      y_from = edges$y_from[i], y_to = edges$y_to[i], thickness = edges$value[i]
    )
    poly$flow <- i
    poly$series <- if (colour_by == "source") edges$from[i] else edges$to[i]
    poly
  }))
  ribbons$series <- factor(ribbons$series,
                           levels = unique(layout$node[order(layout$stage, layout$top)]))
  ribbons$flow <- factor(ribbons$flow, levels = unique(ribbons$flow))

  tk <- vellum_tokens(venue)
  layout$label <- sprintf("%s\n%s", layout$node,
                          format(size[layout$node], big.mark = " ", trim = TRUE))
  last_stage <- max(layout$stage)

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ribbons,
      ggplot2::aes(.data$x, .data$y, group = .data$flow, fill = .data$series),
      alpha = alpha, colour = tk$ink$paper, linewidth = tk$rule$hairline) +
    ggplot2::geom_rect(
      data = layout,
      ggplot2::aes(xmin = .data$x - node_width / 2, xmax = .data$x + node_width / 2,
                   ymin = .data$top, ymax = .data$bottom),
      fill = tk$ink$text) +
    scale_fill_vellum(palette) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "none")

  if (label) {
    # Direct labels rather than a legend: the node is already in the figure, so a
    # legend would ask the reader to match a colour to a name that is six
    # millimetres away from where the name already is.
    layout$hjust <- ifelse(layout$stage == last_stage, 1, 0)
    layout$nudge <- ifelse(layout$stage == last_stage, -1, 1) *
      (node_width / 2 + 0.012)
    p <- p + ggplot2::geom_text(
      data = layout,
      ggplot2::aes(x = .data$x + .data$nudge, y = .data$mid, label = .data$label,
                   hjust = .data$hjust),
      size = fs(tk$type$size$small), colour = tk$ink$text,
      family = tk$type$family, lineheight = 0.95)
  }
  p
}

#' Marimekko chart
#'
#' @description
#' A stacked bar whose bars also differ in width, so that the area of every tile
#' is a count rather than a proportion of a proportion. Use it when both margins
#' of a two-way table matter: which categories are large, and how each one
#' divides.
#'
#' The gain over a plain stacked bar is that the reader can no longer be misled
#' by a wide segment inside a tiny category. The cost is that only the bottom row
#' of segments shares a baseline, so every comparison above it is between
#' floating rectangles. Keep the number of stacked levels small, and put the
#' level the argument depends on at the bottom.
#'
#' @param data A data frame in long form, one row per cell of the two-way table.
#' @param x,fill,value Column names giving the column variable, the stacking
#'   variable, and the count.
#' @param label Print each tile's share of its column, where the tile is large
#'   enough to hold it.
#' @param min_label Smallest share, as a proportion, that still gets a label.
#' @param gap Space between columns, as a fraction of the total width.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- expand.grid(region = c("North", "Central", "South"),
#'                  outcome = c("Recovered", "Relapsed", "Died"))
#' d$n <- c(410, 260, 120, 90, 70, 45, 30, 24, 18)
#' fig_marimekko(d, x = "region", fill = "outcome", value = "n")
fig_marimekko <- function(data, x, fill, value, label = TRUE, min_label = 0.06,
                          gap = 0.006, venue = "nature", palette = "okabe_ito") {
  check_bool(label, "label")
  check_number(min_label, "min_label", min = 0, max = 1)
  check_number(gap, "gap", min = 0, max = 0.2)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(x, fill, value), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  v <- as.numeric(data[[value]])
  check_numeric(v, value)
  if (any(v < 0)) {
    cli::cli_abort(c(
      "Values must be non-negative; {sum(v < 0)} {?is/are} below zero.",
      i = "Every tile's area is a count, and there is no negative area to draw."
    ))
  }
  xf <- factor(data[[x]], levels = unique(as.character(data[[x]])))
  ff <- factor(data[[fill]], levels = unique(as.character(data[[fill]])))
  d <- data.frame(x = xf, fill = ff, value = v, stringsAsFactors = FALSE)
  d <- d[order(d$x, d$fill), , drop = FALSE]

  col_total <- vapply(split(d$value, d$x), sum, numeric(1))
  if (any(col_total <= 0)) {
    cli::cli_abort("Column{?s} {.val {names(col_total)[col_total <= 0]}} {?has/have} no observations.")
  }
  widths <- col_total / sum(col_total)
  n_col <- length(widths)
  usable <- 1 - gap * (n_col - 1L)
  left <- cumsum(c(0, utils::head(widths, -1) * usable + gap))
  d$xmin <- left[as.integer(d$x)]
  d$xmax <- d$xmin + widths[as.integer(d$x)] * usable

  d$share <- d$value / col_total[as.integer(d$x)]
  d$ymax <- unlist(lapply(split(d$share, d$x), cumsum), use.names = FALSE)
  d$ymin <- d$ymax - d$share

  tk <- vellum_tokens(venue)
  mid_x <- (left + d$xmax[match(levels(d$x), d$x)]) / 2

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax, fill = .data$fill),
      colour = tk$ink$paper, linewidth = tk$rule$hairline * 2) +
    scale_fill_vellum(palette) +
    ggplot2::scale_x_continuous(
      breaks = mid_x,
      labels = sprintf("%s\n(%s)", levels(d$x),
                       format(col_total, big.mark = " ", trim = TRUE)),
      expand = ggplot2::expansion(mult = 0.004)) +
    ggplot2::scale_y_continuous(labels = function(z) paste0(z * 100, "%"),
                                expand = ggplot2::expansion(mult = 0.004)) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    theme_vellum(venue = venue, grid = "none", axis_style = "none", ticks = "none")

  if (label) {
    big <- d[d$share >= min_label, , drop = FALSE]
    if (nrow(big)) {
      p <- p + ggplot2::geom_text(
        data = big,
        ggplot2::aes(x = (.data$xmin + .data$xmax) / 2,
                     y = (.data$ymin + .data$ymax) / 2,
                     label = sprintf("%.0f%%", 100 * .data$share)),
        size = fs(tk$type$size$small), colour = tk$ink$paper,
        family = tk$type$family)
    }
  }
  p
}

#' Stacked area chart
#'
#' @description
#' Composition changing through time. The total is the top edge and every band is
#' a part of it, so the figure answers two questions at once: how the whole moved,
#' and how its parts divided.
#'
#' Two things are checked rather than assumed. Negative values cannot be stacked
#' -- a band drawn below the one under it inverts the reading of every band above
#' -- so they are refused. And a group missing at some time points is refused
#' too: `geom_area` will happily join across the gap, which draws a straight line
#' through years for which there is no measurement and makes an absence look like
#' a trend.
#'
#' Set `proportional = TRUE` for shares. That answers a different question and
#' loses the total entirely, so state which one the figure is making.
#'
#' @param data A data frame in long form.
#' @param x,y,group Column names giving time, magnitude, and the stacked series.
#' @param proportional Normalise each time point to 1.
#' @param order_by `"size"` puts the largest band at the bottom, where it has a
#'   flat baseline and can actually be read; `"given"` keeps the factor order,
#'   for series that have a natural sequence.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- expand.grid(year = 2015:2024, source = c("Grant", "Contract", "Donation"))
#' d$amount <- c(seq(40, 76, length.out = 10), seq(30, 12, length.out = 10),
#'               seq(8, 30, length.out = 10))
#' fig_stacked_area(d, x = "year", y = "amount", group = "source")
fig_stacked_area <- function(data, x, y, group, proportional = FALSE,
                             order_by = c("size", "given"),
                             venue = "nature", palette = "okabe_ito") {
  order_by <- match.arg(order_by)
  check_bool(proportional, "proportional")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(x, y, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(x = data[[x]], y = as.numeric(data[[y]]),
                  group = as.character(data[[group]]), stringsAsFactors = FALSE)
  check_numeric(d$y, y)
  if (any(d$y < 0)) {
    cli::cli_abort(c(
      "Values must be non-negative; {sum(d$y < 0)} {?is/are} below zero.",
      i = "A stacked area reads each band's thickness as its value, so a negative band would sit below the one beneath it and invert everything above.",
      i = "For series that cross zero, draw them unstacked with {.fn fig_trajectory}."
    ))
  }

  # A missing group-time cell is the failure this recipe exists to catch.
  complete <- table(d$group, d$x)
  incomplete <- rownames(complete)[apply(complete, 1, function(r) any(r == 0))]
  if (length(incomplete)) {
    cli::cli_abort(c(
      "{length(incomplete)} series {?is/are} missing at some time points: {.val {incomplete}}.",
      i = "A stacked area interpolates across the gap, which draws a trend through periods with no measurement.",
      i = "Supply an explicit zero where the quantity was zero, or trim the range to the period every series covers."
    ))
  }

  levels_by <- if (order_by == "size") {
    names(sort(vapply(split(d$y, d$group), sum, numeric(1)), decreasing = TRUE))
  } else {
    unique(as.character(data[[group]]))
  }
  d$group <- factor(d$group, levels = levels_by)

  if (proportional) {
    totals <- vapply(split(d$y, d$x), sum, numeric(1))
    d$y <- d$y / totals[as.character(d$x)]
  }

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y, fill = .data$group)) +
    ggplot2::geom_area(colour = tk$ink$paper, linewidth = tk$rule$hairline * 2) +
    scale_fill_vellum(palette) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      labels = if (proportional) function(z) paste0(z * 100, "%") else ggplot2::waiver()) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = NULL, y = if (proportional) "Share" else "Value", fill = NULL) +
    theme_vellum(venue = venue, grid = "none")
}
