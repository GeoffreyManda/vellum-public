# Hierarchical and relational recipes ---------------------------------------

hierarchy_nodes <- function(data, path, value) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (!is.character(path) || !length(path) || any(!path %in% names(data))) {
    cli::cli_abort("{.arg path} must name one or more hierarchy columns.")
  }
  check_string(value, "value")
  if (!value %in% names(data) || !is.numeric(data[[value]])) {
    cli::cli_abort("{.arg value} must name a numeric column.")
  }
  if (any(!stats::complete.cases(data[c(path, value)]))) {
    cli::cli_abort("Hierarchy paths and values cannot contain missing values.")
  }
  if (any(data[[value]] < 0) || sum(data[[value]]) <= 0) {
    cli::cli_abort("Hierarchy values must be non-negative with a positive total.")
  }
  data[path] <- lapply(data[path], as.character)
  pieces <- vector("list", length(path))
  separator <- "\u001f"
  for (depth in seq_along(path)) {
    keys <- apply(data[path[seq_len(depth)]], 1, paste, collapse = separator)
    groups <- split(seq_len(nrow(data)), keys)
    pieces[[depth]] <- do.call(rbind, lapply(names(groups), function(key) {
      ii <- groups[[key]]
      labels <- strsplit(key, separator, fixed = TRUE)[[1]]
      parent_key <- if (depth == 1L) "__root__" else
        paste(labels[-length(labels)], collapse = separator)
      data.frame(
        id = paste0(depth, ":", key),
        parent = if (depth == 1L) "__root__" else
          paste0(depth - 1L, ":", parent_key),
        depth = depth,
        label = labels[[length(labels)]],
        top = labels[[1]],
        value = sum(data[[value]][ii]),
        stringsAsFactors = FALSE
      )
    }))
  }
  nodes <- rbind(
    data.frame(
      id = "__root__", parent = NA_character_, depth = 0L,
      label = "Total", top = "Total", value = sum(data[[value]]),
      stringsAsFactors = FALSE
    ),
    do.call(rbind, pieces)
  )
  parent_ids <- unique(nodes$parent[!is.na(nodes$parent)])
  nodes$leaf <- !nodes$id %in% parent_ids
  rownames(nodes) <- NULL
  nodes
}

hierarchy_colours <- function(top, palette = "muted", colours = NULL) {
  levels <- unique(top[top != "Total"])
  check_string(palette, "palette")
  if (is.null(colours)) {
    out <- vellum_pal(palette, length(levels))
  } else {
    check_colour(colours, "colours")
    if (!is.null(names(colours)) && any(nzchar(names(colours)))) {
      missing <- setdiff(levels, names(colours))
      if (length(missing)) {
        cli::cli_abort(c(
          "{.arg colours} does not define every hierarchy group.",
          i = "Missing: {.val {missing}}."
        ))
      }
      out <- unname(colours[levels])
    } else {
      if (length(colours) != length(levels)) {
        cli::cli_abort(
          "Unnamed {.arg colours} must contain exactly {length(levels)} values."
        )
      }
      out <- unname(colours)
    }
  }
  stats::setNames(out, levels)
}

layout_hierarchy_rect <- function(nodes, mode = c("treemap", "partition")) {
  mode <- match.arg(mode)
  out <- list()
  visit <- function(parent, xmin, xmax, ymin, ymax, vertical = TRUE) {
    children <- nodes[!is.na(nodes$parent) & nodes$parent == parent, ,
                      drop = FALSE]
    if (!nrow(children)) return(invisible(NULL))
    children <- children[order(-children$value, children$label), , drop = FALSE]
    weights <- children$value / sum(children$value)
    bounds <- c(0, cumsum(weights))
    for (i in seq_len(nrow(children))) {
      child <- children[i, , drop = FALSE]
      if (mode == "treemap") {
        if (vertical) {
          x0 <- xmin + (xmax - xmin) * bounds[i]
          x1 <- xmin + (xmax - xmin) * bounds[i + 1L]
          y0 <- ymin
          y1 <- ymax
        } else {
          x0 <- xmin
          x1 <- xmax
          y0 <- ymin + (ymax - ymin) * bounds[i]
          y1 <- ymin + (ymax - ymin) * bounds[i + 1L]
        }
      } else {
        x0 <- xmin + (xmax - xmin) * bounds[i]
        x1 <- xmin + (xmax - xmin) * bounds[i + 1L]
        x_depth <- max(nodes$depth)
        y0 <- x_depth - child$depth
        y1 <- y0 + 1
      }
      row <- cbind(child, xmin = x0, xmax = x1, ymin = y0, ymax = y1)
      out[[length(out) + 1L]] <<- row
      visit(
        child$id, x0, x1,
        if (mode == "treemap") y0 else ymin,
        if (mode == "treemap") y1 else ymax,
        !vertical
      )
    }
    invisible(NULL)
  }
  visit("__root__", 0, 1, 0, 1)
  do.call(rbind, out)
}

#' Hierarchical treemap
#'
#' @description
#' Displays nested part-to-whole structure using area, with top-level groups
#' reinforced by labelled boundaries and a colourblind-safe qualitative palette.
#' Treemaps are appropriate for composition and inventory, not precise comparison.
#'
#' @param data A data frame with one row per leaf or repeated leaf records.
#' @param path Character vector of hierarchy-column names, ordered root to leaf.
#' @param value Non-negative magnitude column.
#' @param label_leaves Label sufficiently large leaf rectangles.
#' @param palette Qualitative palette name. Semantic collections such as
#'   `"flow_jama"` and `"atni"` are available through
#'   [vellum_palette_collections()].
#' @param colours Optional explicit colours. A named vector must define every
#'   top-level group; an unnamed vector must have exactly one colour per group.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_treemap <- function(data, path, value, label_leaves = TRUE,
                        venue = "nature", profile = "editorial",
                        palette = "muted", colours = NULL) {
  check_bool(label_leaves, "label_leaves")
  nodes <- hierarchy_nodes(data, path, value)
  d <- layout_hierarchy_rect(nodes, "treemap")
  colours <- hierarchy_colours(d$top, palette, colours)
  leaves <- d[d$leaf, , drop = FALSE]
  leaves$.area <- with(leaves, (xmax - xmin) * (ymax - ymin))
  leaves$.alpha <- scales::rescale(leaves$value, to = c(0.58, 0.94))
  leaves$.show <- label_leaves & leaves$.area >= 0.018
  groups <- d[d$depth == 1L, , drop = FALSE]
  second <- d[d$depth == 2L, , drop = FALSE]
  ggplot2::ggplot(leaves) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax,
        fill = .data$top, alpha = .data$.alpha
      ),
      colour = "#FFFFFF", linewidth = lw(0.35)
    ) +
    ggplot2::geom_rect(
      data = second,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE, fill = NA, colour = "#FFFFFF",
      linewidth = lw(0.7)
    ) +
    ggplot2::geom_rect(
      data = groups,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE, fill = NA, colour = "#111111",
      linewidth = lw(0.8)
    ) +
    ggplot2::geom_text(
      data = leaves[leaves$.show, , drop = FALSE],
      ggplot2::aes(
        x = (.data$xmin + .data$xmax) / 2,
        y = (.data$ymin + .data$ymax) / 2,
        label = .data$label
      ),
      colour = "#111111", size = fs(vellum_tokens(venue)$type$size$small),
      lineheight = 0.92
    ) +
    ggplot2::geom_text(
      data = groups,
      ggplot2::aes(
        x = (.data$xmin + .data$xmax) / 2,
        y = .data$ymax - 0.012,
        label = .data$label
      ),
      inherit.aes = FALSE, hjust = 0.5, vjust = 1,
      fontface = "bold", colour = "#111111",
      size = fs(vellum_tokens(venue)$type$size$small),
      check_overlap = TRUE
    ) +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::labs(
      caption = paste0("Area represents ", value, "; outlined regions are ",
                       path[[1]], ".")
    ) +
    theme_vellum_profile(profile, venue = venue) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
}

#' Icicle hierarchy
#'
#' @description
#' Shows hierarchical depth as aligned rectangular bands. Unlike a sunburst, an
#' icicle retains a common horizontal baseline and is usually better for comparing
#' branch magnitude.
#'
#' @inheritParams fig_treemap
#' @return A ggplot object.
#' @export
fig_icicle <- function(data, path, value, label_leaves = TRUE,
                       venue = "nature", profile = "editorial",
                       palette = "muted", colours = NULL) {
  check_bool(label_leaves, "label_leaves")
  nodes <- hierarchy_nodes(data, path, value)
  d <- layout_hierarchy_rect(nodes, "partition")
  colours <- hierarchy_colours(d$top, palette, colours)
  d$.width <- d$xmax - d$xmin
  d$.alpha <- 0.42 + 0.50 * d$depth / max(d$depth)
  d$.show <- d$depth == 1L | (label_leaves & d$leaf & d$.width >= 0.055)
  d$.face <- ifelse(d$depth == 1L, "bold", "plain")
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax,
        fill = .data$top, alpha = .data$.alpha
      ),
      colour = "#FFFFFF", linewidth = lw(0.35)
    ) +
    ggplot2::geom_text(
      data = d[d$.show, , drop = FALSE],
      ggplot2::aes(
        x = (.data$xmin + .data$xmax) / 2,
        y = (.data$ymin + .data$ymax) / 2,
        label = .data$label, fontface = .data$.face
      ),
      size = fs(vellum_tokens(venue)$type$size$small),
      check_overlap = TRUE
    ) +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(
      x = NULL, y = "Hierarchy depth",
      caption = paste0("Width represents ", value, ".")
    ) +
    theme_vellum_profile(profile, venue = venue) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Sunburst hierarchy
#'
#' @description
#' Circular counterpart to [fig_icicle()]. Use it when the radial overview earns
#' its space; prefer the icicle when branch comparison is the primary task.
#'
#' @inheritParams fig_treemap
#' @return A ggplot object.
#' @export
fig_sunburst <- function(data, path, value,
                         venue = "nature", profile = "editorial",
                         palette = "muted", colours = NULL) {
  nodes <- hierarchy_nodes(data, path, value)
  d <- layout_hierarchy_rect(nodes, "partition")
  colours <- hierarchy_colours(d$top, palette, colours)
  d$.alpha <- 0.44 + 0.50 * d$depth / max(d$depth)
  ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$depth - 1, ymax = .data$depth,
        fill = .data$top, alpha = .data$.alpha
      ),
      colour = "#FFFFFF", linewidth = lw(0.35)
    ) +
    ggplot2::scale_fill_manual(values = colours, name = path[[1]]) +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_polar(theta = "x", start = -pi / 2, clip = "off") +
    ggplot2::xlim(0, 1) +
    ggplot2::labs(
      caption = paste0("Angle represents ", value,
                       "; rings move from root to leaf.")
    ) +
    theme_vellum_profile(profile, venue = venue) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

dendrogram_geometry <- function(tree) {
  next_x <- 0L
  segments <- list()
  leaves <- list()
  walk <- function(node) {
    height <- attr(node, "height") %||% 0
    if (stats::is.leaf(node)) {
      next_x <<- next_x + 1L
      label <- attr(node, "label") %||% as.character(next_x)
      leaves[[length(leaves) + 1L]] <<- data.frame(
        x = next_x, y = 0, label = label, stringsAsFactors = FALSE
      )
      return(list(x = next_x, y = height))
    }
    child_pos <- lapply(node, walk)
    xs <- vapply(child_pos, `[[`, numeric(1), "x")
    ys <- vapply(child_pos, `[[`, numeric(1), "y")
    x <- mean(range(xs))
    for (i in seq_along(xs)) {
      segments[[length(segments) + 1L]] <<- data.frame(
        x = xs[i], y = ys[i], xend = xs[i], yend = height
      )
    }
    segments[[length(segments) + 1L]] <<- data.frame(
      x = min(xs), y = height, xend = max(xs), yend = height
    )
    list(x = x, y = height)
  }
  walk(tree)
  list(
    segments = do.call(rbind, segments),
    leaves = do.call(rbind, leaves)
  )
}

#' Publication dendrogram
#'
#' @param tree An `hclust` or `dendrogram` object.
#' @param k Optional number of terminal clusters to mark redundantly with colour
#'   and shape.
#' @param leaf_labels Show leaf labels.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_dendrogram <- function(tree, k = NULL, leaf_labels = TRUE,
                           venue = "nature", profile = "technical") {
  if (!inherits(tree, c("hclust", "dendrogram"))) {
    cli::cli_abort("{.arg tree} must be an {.cls hclust} or {.cls dendrogram} object.")
  }
  check_bool(leaf_labels, "leaf_labels")
  hc <- if (inherits(tree, "hclust")) tree else stats::as.hclust(tree)
  dend <- if (inherits(tree, "dendrogram")) tree else stats::as.dendrogram(tree)
  g <- dendrogram_geometry(dend)
  ordered_labels <- hc$labels[hc$order]
  g$leaves$label <- ordered_labels
  if (is.null(k)) {
    g$leaves$.cluster <- factor("Leaves")
  } else {
    check_number(k, "k", min = 2, max = length(hc$order))
    membership <- stats::cutree(hc, k = as.integer(k))
    g$leaves$.cluster <- factor(membership[ordered_labels])
  }
  label_y <- -max(hc$height) * 0.035
  ggplot2::ggplot(g$segments) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend
      ),
      colour = "#4D4D4D", linewidth = lw(0.45), lineend = "round"
    ) +
    ggplot2::geom_point(
      data = g$leaves,
      ggplot2::aes(.data$x, .data$y,
                   colour = .data$.cluster, shape = .data$.cluster),
      size = ps(1.5), show.legend = !is.null(k)
    ) +
    ggplot2::geom_text(
      data = if (leaf_labels) g$leaves else g$leaves[0, ],
      ggplot2::aes(.data$x, label_y, label = .data$label,
                   colour = .data$.cluster),
      angle = 55, hjust = 1, vjust = 0.5,
      size = fs(vellum_tokens(venue)$type$size$small),
      show.legend = FALSE
    ) +
    scale_colour_vellum(name = if (is.null(k)) NULL else "Cluster") +
    scale_shape_vellum(name = if (is.null(k)) NULL else "Cluster") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.12, 0.04))
    ) +
    ggplot2::labs(x = NULL, y = "Merge height") +
    ggplot2::coord_cartesian(clip = "off") +
    theme_vellum_profile(profile, venue = venue) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

cubic_bezier <- function(p0, p1, p2, p3, n = 28) {
  tt <- seq(0, 1, length.out = n)
  cbind(
    x = (1 - tt)^3 * p0[1] + 3 * (1 - tt)^2 * tt * p1[1] +
      3 * (1 - tt) * tt^2 * p2[1] + tt^3 * p3[1],
    y = (1 - tt)^3 * p0[2] + 3 * (1 - tt)^2 * tt * p1[2] +
      3 * (1 - tt) * tt^2 * p2[2] + tt^3 * p3[2]
  )
}

polar_xy <- function(angle, radius = 1) {
  cbind(x = cos(angle) * radius, y = sin(angle) * radius)
}

spread_label_boxes <- function(desired, half_height, gap, limits, labels) {
  n <- length(desired)
  if (!n) return(numeric())
  ord <- order(desired)
  wanted <- desired[ord]
  half <- half_height[ord]
  span <- diff(limits)
  required <- sum(2 * half) + gap * max(n - 1L, 0L)
  if (required > span + sqrt(.Machine$double.eps)) {
    cli::cli_abort(c(
      "Chord labels cannot fit without overlap at the requested size.",
      i = "Crowded labels: {.val {labels[ord]}}.",
      i = "Aggregate groups, shorten labels, reduce {.arg label_gap}, or use a wider venue."
    ))
  }

  placed <- wanted
  placed[1] <- max(placed[1], limits[1] + half[1])
  if (n > 1L) {
    for (i in 2:n) {
      placed[i] <- max(
        placed[i],
        placed[i - 1L] + half[i - 1L] + half[i] + gap
      )
    }
  }
  if (placed[n] > limits[2] - half[n]) {
    placed[n] <- limits[2] - half[n]
    if (n > 1L) {
      for (i in seq.int(n - 1L, 1L)) {
        placed[i] <- min(
          placed[i],
          placed[i + 1L] - half[i + 1L] - half[i] - gap
        )
      }
    }
  }
  if (placed[1] < limits[1] + half[1]) {
    placed[1] <- limits[1] + half[1]
    if (n > 1L) {
      for (i in 2:n) {
        placed[i] <- max(
          placed[i],
          placed[i - 1L] + half[i - 1L] + half[i] + gap
        )
      }
    }
  }
  out <- numeric(n)
  out[ord] <- placed
  out
}

chord_label_layout <- function(mid, labels, venue = "nature",
                               label_width = 15, label_gap = 1) {
  check_number(label_width, "label_width", min = 6, max = 40)
  check_number(label_gap, "label_gap", min = 0, max = 8)
  tk <- vellum_tokens(venue)
  size_pt <- tk$type$size$small
  family <- tk$type$family
  wrapped <- label_wrap_mm(
    label_width, size = size_pt, family = family
  )(labels)
  lines <- strsplit(wrapped, "\n", fixed = TRUE)
  widths_pt <- lapply(lines, function(x) {
    measured <- try(
      systemfonts::string_width(x, family = family, size = size_pt),
      silent = TRUE
    )
    if (inherits(measured, "try-error")) nchar(x) * size_pt * 0.52 else measured
  })
  max_width_pt <- max(unlist(widths_pt), 0)
  if (max_width_pt > mm_pt(label_width) + 0.5) {
    offending <- labels[vapply(widths_pt, max, numeric(1)) >
                          mm_pt(label_width) + 0.5]
    cli::cli_abort(c(
      "A chord label contains a word wider than {.arg label_width}.",
      i = "Shorten or hyphenate: {.val {offending}}.",
      i = "Labels are never clipped or allowed to cross the figure bounds."
    ))
  }

  # Reserve a conservative 20% of the venue width for plot decoration. Solving
  # this equation makes the physical text width part of the coordinate bound:
  # x_limit = label_x + text_width_in_data_units + margin.
  # The chord is almost always the claim panel of a double-column figure; sizing
  # the label ring for the single column silently caps capacity at ~13-16 labels
  # per side even though the rendered panel is much wider (observed 2026-08-04:
  # a 28-node causal-structure chord aborted as "crowded" at 89 mm and rendered
  # at 183 mm). Fall back to the single width only for venues with no double.
  panel_mm <- tryCatch(
    venue_width(venue, "double"),
    error = function(e) venue_width(venue)
  ) * 0.80
  label_x <- 1.25
  edge_pad <- 0.06
  max_width_mm <- max_width_pt / .PT
  denominator <- 1 - 2 * (max_width_mm + 0.8) / panel_mm
  if (denominator <= 0.20) {
    cli::cli_abort(c(
      "Chord labels consume too much of the available physical width.",
      i = "Shorten labels or use a wider venue."
    ))
  }
  x_limit <- max(1.72, (label_x + edge_pad) / denominator)
  units_per_mm <- 2 * x_limit / panel_mm
  line_height <- pt_mm(size_pt) * tk$type$lineheight * units_per_mm
  half_height <- lengths(lines) * line_height / 2
  gap_data <- label_gap * units_per_mm
  y_limits <- c(-1.13, 1.13)
  side <- ifelse(cos(mid) >= 0, 1, -1)
  desired <- sin(mid) * 1.06
  y <- numeric(length(mid))
  for (s in c(-1, 1)) {
    take <- which(side == s)
    y[take] <- spread_label_boxes(
      desired[take], half_height[take], gap_data, y_limits, labels[take]
    )
  }
  anchor <- polar_xy(mid, 1.065)
  out <- data.frame(
    anchor_x = anchor[, 1],
    anchor_y = anchor[, 2],
    x = side * label_x,
    y = y,
    hjust = ifelse(side > 0, 0, 1),
    side = side,
    label = wrapped,
    raw_label = labels,
    half_height = half_height,
    lower = y - half_height,
    upper = y + half_height,
    stringsAsFactors = FALSE
  )
  attr(out, "xlim") <- c(-x_limit, x_limit)
  attr(out, "ylim") <- y_limits
  attr(out, "family") <- family
  attr(out, "size_pt") <- size_pt
  out
}

chord_leaders <- function(labels) {
  do.call(rbind, lapply(seq_len(nrow(labels)), function(i) {
    s <- labels$side[i]
    data.frame(
      x = c(labels$anchor_x[i], s * 1.12, s * 1.19, s * 1.225),
      y = c(labels$anchor_y[i], labels$anchor_y[i], labels$y[i], labels$y[i]),
      label_id = i
    )
  }))
}

#' Weighted chord diagram
#'
#' @description
#' Draws an accessible, static chord diagram from a non-negative adjacency
#' matrix. Ribbons encode pairwise magnitude; outer arcs and direct labels
#' preserve group identity. Labels are physically measured, wrapped, separated
#' on each side and connected with leaders. If they cannot fit without collision
#' or clipping, the function stops rather than returning a defective plot.
#' Chords are best for topology and reciprocity, not precise pairwise ranking.
#'
#' @param matrix A square non-negative matrix with row and column names in the
#'   same order. Diagonal values are ignored.
#' @param gap Angular gap between groups in radians.
#' @param min_value Do not draw links below this value.
#' @param palette Qualitative palette name. This includes semantic skill
#'   collections such as `"flow_jama"`, `"structured_domains"` and `"atni"`; see
#'   [vellum_palette_collections()].
#' @param colours Optional explicit colours. A named vector must define every
#'   matrix group; an unnamed vector must contain exactly one colour per group.
#' @param label_width Maximum physical width of each wrapped label line, in mm.
#' @param label_gap Minimum physical gap between neighbouring label boxes, in mm.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_chord <- function(matrix, gap = 0.045, min_value = 0,
                      venue = "nature", profile = "technical",
                      palette = "muted", colours = NULL,
                      label_width = 15, label_gap = 1) {
  m <- as.matrix(matrix)
  if (!is.numeric(m) || nrow(m) != ncol(m) || nrow(m) < 2L ||
      any(!is.finite(m)) || any(m < 0)) {
    cli::cli_abort("{.arg matrix} must be square, finite, numeric and non-negative.")
  }
  if (is.null(rownames(m)) || is.null(colnames(m)) ||
      !identical(rownames(m), colnames(m))) {
    cli::cli_abort("Row and column names must be present and identical.")
  }
  check_number(gap, "gap", min = 0, max = 0.4)
  check_number(min_value, "min_value", min = 0)
  diag(m) <- 0
  totals <- rowSums(m) + colSums(m)
  keep <- totals > 0
  m <- m[keep, keep, drop = FALSE]
  totals <- totals[keep]
  if (nrow(m) < 2L || sum(m) <= 0) {
    cli::cli_abort("At least two connected groups are required.")
  }
  n_group <- nrow(m)
  usable <- 2 * pi - n_group * gap
  if (usable <= 0) {
    cli::cli_abort(
      "{.arg gap} leaves no angular space for {n_group} groups."
    )
  }
  widths <- usable * totals / sum(totals)
  starts <- numeric(n_group)
  ends <- numeric(n_group)
  cursor <- -pi / 2
  for (i in seq_len(n_group)) {
    starts[i] <- cursor
    ends[i] <- cursor + widths[i]
    cursor <- ends[i] + gap
  }
  names(starts) <- names(ends) <- rownames(m)
  colours <- hierarchy_colours(rownames(m), palette, colours)
  out_cursor <- starts
  in_cursor <- starts + widths * rowSums(m) / totals
  ribbons <- list()
  ribbon_id <- 0L
  for (i in seq_len(n_group)) {
    for (j in seq_len(n_group)) {
      value <- m[i, j]
      if (i == j || value <= min_value) next
      source_span <- widths[i] * value / totals[i]
      target_span <- widths[j] * value / totals[j]
      sa <- c(out_cursor[i], out_cursor[i] + source_span)
      ta <- c(in_cursor[j], in_cursor[j] + target_span)
      out_cursor[i] <- sa[2]
      in_cursor[j] <- ta[2]
      source_arc <- polar_xy(seq(sa[1], sa[2], length.out = 8), 0.98)
      target_arc <- polar_xy(seq(ta[1], ta[2], length.out = 8), 0.98)
      bridge_1 <- cubic_bezier(
        source_arc[nrow(source_arc), ],
        source_arc[nrow(source_arc), ] * 0.12,
        target_arc[1, ] * 0.12,
        target_arc[1, ]
      )
      bridge_2 <- cubic_bezier(
        target_arc[nrow(target_arc), ],
        target_arc[nrow(target_arc), ] * 0.12,
        source_arc[1, ] * 0.12,
        source_arc[1, ]
      )
      ribbon_id <- ribbon_id + 1L
      polygon <- rbind(source_arc, bridge_1, target_arc, bridge_2)
      ribbons[[ribbon_id]] <- data.frame(
        polygon, ribbon = ribbon_id, source = rownames(m)[i],
        value = value, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(ribbons)) {
    cli::cli_abort(c(
      "No chord remains above {.arg min_value}.",
      i = "Lower the threshold or provide a matrix with larger connections."
    ))
  }
  ribbon_data <- do.call(rbind, ribbons)
  arcs <- do.call(rbind, lapply(seq_len(n_group), function(i) {
    xy <- polar_xy(seq(starts[i], ends[i], length.out = 60), 1.02)
    data.frame(xy, group = rownames(m)[i], order = seq_len(nrow(xy)))
  }))
  mid <- (starts + ends) / 2
  labels <- chord_label_layout(
    mid, names(mid), venue = venue,
    label_width = label_width, label_gap = label_gap
  )
  leaders <- chord_leaders(labels)
  x_limits <- attr(labels, "xlim")
  y_limits <- attr(labels, "ylim")
  family <- attr(labels, "family")
  size_pt <- attr(labels, "size_pt")
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ribbon_data,
      ggplot2::aes(.data$x, .data$y, group = .data$ribbon,
                   fill = .data$source),
      colour = NA, alpha = 0.58
    ) +
    ggplot2::geom_path(
      data = arcs,
      ggplot2::aes(.data$x, .data$y, group = .data$group,
                   colour = .data$group),
      linewidth = lw(4.2), lineend = "butt"
    ) +
    ggplot2::geom_path(
      data = leaders,
      ggplot2::aes(.data$x, .data$y, group = .data$label_id),
      colour = "#767676", linewidth = lw(0.35), lineend = "round"
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(.data$x, .data$y, label = .data$label,
                   hjust = .data$hjust),
      family = family, size = fs(size_pt), lineheight = 1.08,
      colour = "#111111"
    ) +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::scale_colour_manual(values = colours, guide = "none") +
    ggplot2::coord_equal(
      xlim = x_limits, ylim = y_limits,
      expand = FALSE, clip = "on"
    ) +
    ggplot2::labs(
      caption = "Ribbon width encodes connection magnitude; outer arcs encode total involvement."
    ) +
    theme_vellum_profile(profile, venue = venue) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
  attr(p, "vellum_chord_labels") <- labels
  attr(p, "vellum_chord_bounds") <- list(x = x_limits, y = y_limits)
  p
}
