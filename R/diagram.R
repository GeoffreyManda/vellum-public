# Diagrams.
#
# A scientific schematic is a graph with opinions. The nodes are entities with a
# type, the edges are relations with a meaning, and the meaning is carried by the
# arrowhead: an activation and an inhibition are the same line with different ends
# and opposite conclusions. Drawing them as identical arrows, which is what a
# general diagramming tool does, throws away the entire content.
#
# So this is not a graph-drawing library with biology-flavoured defaults. The edge
# vocabulary is the vocabulary the figure protocols use -- activation, inhibition,
# binding, translocation, catalysis, conversion, association, flow -- and each has
# a fixed glyph, because a reader who has learned the convention should not have to
# relearn it per figure.
#
# The result is a ggplot. That is deliberate and it is most of the value here: a
# schematic built this way carries a figure contract, obeys venue tokens, composes
# into a patchwork plate beside a chart, exports through `save_figure()` at exact
# physical size, and is checked by `venue_check()` like anything else.

# ---- node and edge constructors ----------------------------------------------

.dg_shapes <- c("rect", "round", "stadium", "ellipse", "circle", "hexagon",
                "diamond", "parallelogram", "cylinder", "asset", "label")

#' Describe a node in a diagram
#'
#' @param id Unique identifier, used by [dg_edge()] and [dg_compartment()].
#' @param label Text drawn in or beside the node. Wrapped to the node's width.
#' @param shape One of `"rect"`, `"round"`, `"stadium"`, `"ellipse"`, `"circle"`,
#'   `"hexagon"`, `"diamond"`, `"parallelogram"`, `"cylinder"`, `"asset"`, or
#'   `"label"` for text with no frame.
#' @param role Semantic role, used for default colour and reported in the
#'   diagram's own summary. Free text; the causal roles `"exposure"`,
#'   `"outcome"`, `"confounder"`, `"mediator"`, `"collider"` and `"instrument"`
#'   are understood by [fig_dag()].
#' @param x,y Position in diagram units. Supply for `layout = "manual"`; leave
#'   `NA` to be positioned by the layout.
#' @param width,height Size in diagram units. Defaults come from the diagram.
#' @param fill,colour,text_colour Ink. `NULL` takes the diagram's palette.
#' @param asset A `vellum_svg` illustration to draw instead of a shape, for
#'   `shape = "asset"`.
#' @param label_position `"inside"`, or `"below"`/`"above"` for an asset node.
#' @param label_dx,label_dy Offset of the label from the node's centre, in
#'   millimetres. Use this to seat an illustration inside a node and move the
#'   text clear of it. The wrapping width shrinks by twice the offset, so the
#'   label stays inside the shape rather than running out of the far side.
#' @param count Optional number shown with the label, for flow diagrams whose
#'   counts must reconcile. See [fig_consort()].
#' @param ... Further columns carried through to the node table.
#'
#' @return A one-row `data.frame` of class `vellum_dg_node`.
#' @seealso [dg_edge()], [dg_compartment()], [diagram()].
#' @export
#' @examples
#' dg_node("il6", "IL-6", shape = "ellipse", role = "ligand")
dg_node <- function(id, label = id, shape = "round", role = NA_character_,
                    x = NA_real_, y = NA_real_, width = NA_real_, height = NA_real_,
                    fill = NA_character_, colour = NA_character_,
                    text_colour = NA_character_, asset = NULL,
                    label_position = "inside", label_dx = 0, label_dy = 0,
                    count = NA_real_, ...) {
  check_string(id, "id")
  check_string(label, "label")
  if (!shape %in% .dg_shapes) {
    cli::cli_abort("{.arg shape} must be one of {.val {.dg_shapes}}, not {.val {shape}}.")
  }
  if (!is.null(asset) && !inherits(asset, c("vellum_svg", "vellum_raster"))) {
    cli::cli_abort(c(
      "{.arg asset} must be an illustration.",
      i = "Vector artwork from {.fn svg_read} or {.fn vellum_asset}, or raster artwork from {.fn raster_read}."))
  }
  if (!is.null(asset)) shape <- "asset"
  if (identical(shape, "asset") && is.null(asset)) {
    cli::cli_abort("{.arg shape} is {.val asset} but no {.arg asset} was given.")
  }
  out <- data.frame(
    id = id, label = label, shape = shape, role = role,
    x = as.numeric(x), y = as.numeric(y),
    width = as.numeric(width), height = as.numeric(height),
    fill = fill, colour = colour, text_colour = text_colour,
    label_position = label_position,
    label_dx = as.numeric(label_dx), label_dy = as.numeric(label_dy),
    count = as.numeric(count),
    stringsAsFactors = FALSE, ...
  )
  attr(out, "asset") <- stats::setNames(list(asset), id)
  structure(out, class = c("vellum_dg_node", "data.frame"))
}

# The edge vocabulary. Each entry fixes the glyph, so the same relation looks the
# same in every figure this package draws.
.dg_edge_types <- list(
  flow          = list(head = "arrow",     linetype = "solid",  label = "proceeds to"),
  activation    = list(head = "arrow",     linetype = "solid",  label = "activates"),
  inhibition    = list(head = "bar",       linetype = "solid",  label = "inhibits"),
  binding       = list(head = "ball",      linetype = "solid",  label = "binds"),
  translocation = list(head = "open",      linetype = "22",     label = "translocates to"),
  catalysis     = list(head = "circle",    linetype = "solid",  label = "catalyses"),
  conversion    = list(head = "arrow",     linetype = "solid",  label = "is converted to"),
  association   = list(head = "none",      linetype = "22",     label = "is associated with"),
  causal        = list(head = "arrow",     linetype = "solid",  label = "causes"),
  biasing       = list(head = "arrow",     linetype = "42",     label = "opens a biasing path")
)

#' Describe an edge in a diagram
#'
#' @description
#' The `type` fixes the arrowhead, and the arrowhead is the claim. An activation
#' and an inhibition differ only in their ending, and drawing both as a plain
#' arrow discards the mechanism the figure exists to show.
#'
#' @param from,to Node identifiers.
#' @param type One of `"flow"`, `"activation"`, `"inhibition"`, `"binding"`,
#'   `"translocation"`, `"catalysis"`, `"conversion"`, `"association"`,
#'   `"causal"`, or `"biasing"`.
#' @param label Optional text beside the edge, such as a rate or a count.
#' @param route `"straight"`, or `"elbow"` for the orthogonal routing that flow
#'   diagrams use for side branches.
#' @param colour Ink. `NULL` takes the diagram's default.
#' @param linewidth Weight as a ggplot2 linewidth. `NA` takes the venue token.
#' @param curvature Bend, for separating reciprocal edges between the same pair.
#' @param ... Further columns carried through.
#'
#' @return A one-row `data.frame` of class `vellum_dg_edge`.
#' @export
#' @examples
#' dg_edge("il6", "stat3", type = "activation")
#' dg_edge("drug", "kinase", type = "inhibition", label = "IC50 12 nM")
dg_edge <- function(from, to, type = "flow", label = NA_character_,
                    route = c("straight", "elbow"), colour = NA_character_,
                    linewidth = NA_real_, curvature = 0, ...) {
  check_string(from, "from")
  check_string(to, "to")
  route <- match.arg(route)
  if (!type %in% names(.dg_edge_types)) {
    cli::cli_abort(c("{.arg type} must be one of {.val {names(.dg_edge_types)}}.",
                     i = "The type decides the arrowhead, and the arrowhead is the claim."))
  }
  structure(data.frame(
    from = from, to = to, type = type, label = label, route = route,
    colour = colour, linewidth = as.numeric(linewidth),
    curvature = as.numeric(curvature), stringsAsFactors = FALSE, ...
  ), class = c("vellum_dg_edge", "data.frame"))
}

#' Describe a compartment enclosing several nodes
#'
#' @description
#' A compartment is the biological or organisational container a set of nodes
#' sits in: the nucleus, the cytosol, a ward, a study arm. It is drawn behind the
#' nodes and takes no ink of its own beyond a faint frame, because a compartment
#' that competes with its contents has inverted the hierarchy.
#'
#' @param id Identifier.
#' @param label Text drawn at the compartment's edge.
#' @param contains Node identifiers inside it.
#' @param fill,colour Ink. `NA` takes a faint default derived from the tokens.
#' @param pad Space between the contents and the frame, in diagram units.
#' @param label_position `"top"`, `"bottom"`, `"topleft"`.
#'
#' @return A one-row `data.frame` of class `vellum_dg_compartment`.
#' @export
#' @examples
#' dg_compartment("nucleus", "Nucleus", contains = c("stat3n", "dna"))
dg_compartment <- function(id, label = id, contains, fill = NA_character_,
                           colour = NA_character_, pad = 0.35,
                           label_position = c("top", "bottom", "topleft")) {
  check_string(id, "id")
  check_string(label, "label")
  if (!is.character(contains) || !length(contains)) {
    cli::cli_abort("{.arg contains} must name at least one node.")
  }
  label_position <- match.arg(label_position)
  check_number(pad, "pad", min = 0)
  out <- data.frame(id = id, label = label, fill = fill, colour = colour,
                    pad = pad, label_position = label_position,
                    stringsAsFactors = FALSE)
  out$contains <- I(list(contains))
  structure(out, class = c("vellum_dg_compartment", "data.frame"))
}

# ---- collection --------------------------------------------------------------

dg_bind <- function(x, what) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) cli::cli_abort("{.arg {what}} must be a list or a data frame.")
  parts <- Filter(Negate(is.null), x)
  if (!length(parts)) return(NULL)
  assets <- unlist(lapply(parts, function(p) attr(p, "asset")), recursive = FALSE)
  common <- Reduce(intersect, lapply(parts, names))
  out <- do.call(rbind, lapply(parts, function(p) p[, common, drop = FALSE]))
  attr(out, "asset") <- assets
  out
}

# ---- layout ------------------------------------------------------------------

# Rank assignment by longest path from a source. This is the first phase of the
# standard layered method, and it is the one that matters scientifically: a node
# appears downstream of everything that feeds it, so reading order matches
# mechanism order. Cycles, which a feedback loop legitimately produces, are broken
# for ranking only and still drawn.
dg_ranks <- function(nodes, edges) {
  ids <- nodes$id
  rank <- stats::setNames(rep(0L, length(ids)), ids)
  if (is.null(edges) || !nrow(edges)) return(rank)
  e <- edges[edges$from %in% ids & edges$to %in% ids, , drop = FALSE]
  if (!nrow(e)) return(rank)

  # Feedback edges are removed before ranking and drawn afterwards. Without this
  # the longest-path relaxation has no fixed point: a SOCS3 loop back onto JAK is
  # a correct piece of biology that would otherwise push every node in the cycle
  # one rank further apart on every pass, and the diagram grows without bound.
  e <- e[!dg_back_edges(ids, e), , drop = FALSE]
  if (!nrow(e)) return(rank)

  for (iter in seq_len(length(ids))) {
    changed <- FALSE
    for (k in seq_len(nrow(e))) {
      want <- rank[[e$from[k]]] + 1L
      if (want > rank[[e$to[k]]]) {
        rank[[e$to[k]]] <- want
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  rank
}

# Depth-first search marking the edges that close a cycle. Which edges these are
# depends on the traversal order, so the node order is the caller's and the
# result is deterministic for a given input.
dg_back_edges <- function(ids, edges) {
  state <- stats::setNames(rep(0L, length(ids)), ids)
  back <- rep(FALSE, nrow(edges))
  visit <- function(v) {
    state[[v]] <<- 1L
    for (k in which(edges$from == v)) {
      w <- edges$to[k]
      if (state[[w]] == 1L) {
        back[k] <<- TRUE
      } else if (state[[w]] == 0L) {
        visit(w)
      }
    }
    state[[v]] <<- 2L
    invisible(NULL)
  }
  for (v in ids) if (state[[v]] == 0L) visit(v)
  back
}

# Within-rank ordering by the barycentre of each node's neighbours in the
# previous rank, which is the cheap, deterministic way to reduce edge crossings.
dg_order_within <- function(nodes, edges, rank) {
  ids <- nodes$id
  pos <- stats::setNames(rep(NA_real_, length(ids)), ids)
  ranks <- sort(unique(rank))
  for (r in ranks) {
    here <- names(rank)[rank == r]
    if (r == min(ranks) || is.null(edges) || !nrow(edges)) {
      pos[here] <- seq_along(here)
      next
    }
    bary <- vapply(here, function(id) {
      parents <- edges$from[edges$to == id]
      p <- pos[intersect(parents, names(pos))]
      p <- p[!is.na(p)]
      if (!length(p)) NA_real_ else mean(p)
    }, numeric(1))
    ord <- order(ifelse(is.na(bary), Inf, bary), here)
    pos[here[ord]] <- seq_along(here)
  }
  pos[is.na(pos)] <- seq_len(sum(is.na(pos)))
  pos
}

dg_layout <- function(nodes, edges, layout, direction, spacing_x, spacing_y) {
  n <- nrow(nodes)
  if (layout == "manual") {
    if (any(is.na(nodes$x)) || any(is.na(nodes$y))) {
      cli::cli_abort(c("{.arg layout} is {.val manual} but some nodes have no position.",
                       i = "Give every node an {.arg x} and {.arg y}, or choose another layout."))
    }
    return(nodes)
  }
  if (layout == "grid") {
    cols <- ceiling(sqrt(n))
    nodes$x <- ((seq_len(n) - 1L) %% cols) * spacing_x
    nodes$y <- -((seq_len(n) - 1L) %/% cols) * spacing_y
    return(dg_apply_fixed(nodes))
  }
  if (layout == "radial") {
    if (n == 1L) {
      nodes$x <- 0; nodes$y <- 0
      return(dg_apply_fixed(nodes))
    }
    # A hub-and-spoke reading: the first node is the hub when it has the most
    # connections, otherwise every node sits on the ring.
    deg <- if (!is.null(edges) && nrow(edges)) {
      vapply(nodes$id, function(i) sum(edges$from == i | edges$to == i), integer(1))
    } else {
      rep(0L, n)
    }
    hub <- if (max(deg) >= n - 1L && max(deg) > 1L) which.max(deg) else NA_integer_
    ring <- setdiff(seq_len(n), hub)
    theta <- seq(pi / 2, pi / 2 - 2 * pi, length.out = length(ring) + 1L)[seq_along(ring)]
    radius <- max(spacing_x, spacing_y) * max(1, length(ring) / 5.2)
    nodes$x <- 0; nodes$y <- 0
    nodes$x[ring] <- radius * cos(theta)
    nodes$y[ring] <- radius * sin(theta)
    return(dg_apply_fixed(nodes))
  }

  rank <- dg_ranks(nodes, edges)
  within <- dg_order_within(nodes, edges, rank)
  r <- unname(rank[nodes$id])
  w <- unname(within[nodes$id])
  # Centre each rank on the axis so the diagram reads as a spine rather than as a
  # left-aligned list.
  centred <- w
  for (rr in unique(r)) {
    idx <- which(r == rr)
    centred[idx] <- w[idx] - mean(w[idx])
  }
  if (direction %in% c("down", "up")) {
    nodes$x <- centred * spacing_x
    nodes$y <- if (direction == "down") -r * spacing_y else r * spacing_y
  } else {
    nodes$x <- if (direction == "right") r * spacing_x else -r * spacing_x
    nodes$y <- -centred * spacing_y
  }
  dg_apply_fixed(nodes)
}

# A node given an explicit position keeps it, whatever the layout decided.
dg_apply_fixed <- function(nodes) {
  fixed_x <- !is.na(nodes$.fixed_x)
  fixed_y <- !is.na(nodes$.fixed_y)
  nodes$x[fixed_x] <- nodes$.fixed_x[fixed_x]
  nodes$y[fixed_y] <- nodes$.fixed_y[fixed_y]
  nodes
}

# ---- geometry ----------------------------------------------------------------

dg_shape_points <- function(shape, x, y, w, h, n = 64) {
  hw <- w / 2; hh <- h / 2
  corner <- min(hw, hh) * 0.34
  switch(
    shape,
    rect = cbind(c(x - hw, x + hw, x + hw, x - hw), c(y - hh, y - hh, y + hh, y + hh)),
    round = dg_round_rect(x, y, hw, hh, corner),
    stadium = dg_round_rect(x, y, hw, hh, hh * 0.98),
    circle = dg_ellipse(x, y, min(hw, hh), min(hw, hh), n),
    ellipse = dg_ellipse(x, y, hw, hh, n),
    diamond = cbind(c(x, x + hw, x, x - hw), c(y - hh, y, y + hh, y)),
    hexagon = cbind(x + hw * c(-0.6, 0.6, 1, 0.6, -0.6, -1),
                    y + hh * c(-1, -1, 0, 1, 1, 0)),
    parallelogram = cbind(x + hw * c(-0.72, 1, 0.72, -1), y + hh * c(-1, -1, 1, 1)),
    cylinder = dg_round_rect(x, y, hw, hh, corner),
    dg_round_rect(x, y, hw, hh, corner)
  )
}

dg_ellipse <- function(x, y, rx, ry, n = 64) {
  t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  cbind(x + rx * cos(t), y + ry * sin(t))
}

dg_round_rect <- function(x, y, hw, hh, r, k = 10) {
  r <- min(r, hw, hh)
  if (r <= 0) {
    return(cbind(c(x - hw, x + hw, x + hw, x - hw), c(y - hh, y - hh, y + hh, y + hh)))
  }
  arc <- function(cx, cy, a0, a1) {
    t <- seq(a0, a1, length.out = k)
    cbind(cx + r * cos(t), cy + r * sin(t))
  }
  rbind(
    arc(x + hw - r, y - hh + r, -pi / 2, 0),
    arc(x + hw - r, y + hh - r, 0, pi / 2),
    arc(x - hw + r, y + hh - r, pi / 2, pi),
    arc(x - hw + r, y - hh + r, pi, 3 * pi / 2)
  )
}

# Where an edge meets a node. Stopping the line at the node's centre and letting
# the shape cover it looks acceptable until the shape is translucent or the
# arrowhead is a bar, at which point the drawing is visibly wrong. This walks the
# node's own outline and finds the crossing.
dg_boundary <- function(shape, cx, cy, w, h, tx, ty) {
  dx <- tx - cx; dy <- ty - cy
  len <- sqrt(dx^2 + dy^2)
  if (!is.finite(len) || len < 1e-9) return(c(cx, cy))
  ux <- dx / len; uy <- dy / len
  if (shape %in% c("circle", "ellipse")) {
    rx <- if (shape == "circle") min(w, h) / 2 else w / 2
    ry <- if (shape == "circle") min(w, h) / 2 else h / 2
    denom <- sqrt((ux / rx)^2 + (uy / ry)^2)
    if (denom < 1e-9) return(c(cx, cy))
    return(c(cx + ux / denom, cy + uy / denom))
  }
  poly <- dg_shape_points(shape, cx, cy, w, h)
  hit <- dg_ray_polygon(cx, cy, ux, uy, poly)
  if (is.na(hit)) c(cx, cy) else c(cx + ux * hit, cy + uy * hit)
}

# Distance from an interior point along a ray to the polygon boundary.
dg_ray_polygon <- function(px, py, ux, uy, poly) {
  n <- nrow(poly)
  best <- NA_real_
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    ax <- poly[i, 1]; ay <- poly[i, 2]
    bx <- poly[j, 1]; by <- poly[j, 2]
    ex <- bx - ax; ey <- by - ay
    den <- ux * ey - uy * ex
    if (abs(den) < 1e-12) next
    t <- ((ax - px) * ey - (ay - py) * ex) / den
    s <- if (abs(ex) > abs(ey)) (px + ux * t - ax) / ex else (py + uy * t - ay) / ey
    if (t > 1e-9 && s >= -1e-9 && s <= 1 + 1e-9) {
      if (is.na(best) || t < best) best <- t
    }
  }
  best
}

# ---- edge glyphs -------------------------------------------------------------

# Every glyph is sized in millimetres, because the diagram's data units are
# millimetres at its natural size. An arrowhead specified as a fraction of the
# data range changes size when a node is added, which is how a figure ends up
# with two visually different activations meaning the same thing.
.dg_head <- list(length = 2.3, width = 1.5, dot = 0.85, bar = 2.6)

dg_head_grobs <- function(seg, head, colour, linewidth) {
  if (identical(head, "none") || !nrow(seg)) return(list())
  ux <- seg$xend - seg$x0h
  uy <- seg$yend - seg$y0h
  len <- sqrt(ux^2 + uy^2)
  len[len < 1e-9] <- 1
  ux <- ux / len; uy <- uy / len
  px <- -uy; py <- ux

  if (head == "arrow") {
    hl <- .dg_head$length; hw <- .dg_head$width
    poly <- do.call(rbind, lapply(seq_len(nrow(seg)), function(i) {
      tipx <- seg$xend[i]; tipy <- seg$yend[i]
      data.frame(
        x = c(tipx, tipx - hl * ux[i] + hw / 2 * px[i], tipx - hl * ux[i] - hw / 2 * px[i]),
        y = c(tipy, tipy - hl * uy[i] + hw / 2 * py[i], tipy - hl * uy[i] - hw / 2 * py[i]),
        grp = paste0("head", i), colour = seg$colour[i], stringsAsFactors = FALSE)
    }))
    return(list(ggplot2::geom_polygon(
      data = poly, ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
      fill = poly$colour[match(unique(poly$grp), poly$grp)][1], colour = NA,
      inherit.aes = FALSE)))
  }
  if (head == "open") {
    hl <- .dg_head$length; hw <- .dg_head$width
    d <- do.call(rbind, lapply(seq_len(nrow(seg)), function(i) data.frame(
      x = c(seg$xend[i] - hl * ux[i] + hw / 2 * px[i], seg$xend[i],
            seg$xend[i] - hl * ux[i] - hw / 2 * px[i]),
      y = c(seg$yend[i] - hl * uy[i] + hw / 2 * py[i], seg$yend[i],
            seg$yend[i] - hl * uy[i] - hw / 2 * py[i]),
      grp = paste0("open", i), colour = seg$colour[i], stringsAsFactors = FALSE)))
    return(list(ggplot2::geom_path(
      data = d, ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
      colour = d$colour[1], linewidth = linewidth, lineend = "round", inherit.aes = FALSE)))
  }
  if (head == "bar") {
    hb <- .dg_head$bar / 2
    d <- data.frame(
      x = seg$xend - hb * px, y = seg$yend - hb * py,
      xend = seg$xend + hb * px, yend = seg$yend + hb * py,
      colour = seg$colour, stringsAsFactors = FALSE)
    return(list(ggplot2::geom_segment(
      data = d, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      colour = d$colour, linewidth = linewidth * 1.25, lineend = "butt", inherit.aes = FALSE)))
  }
  if (head %in% c("ball", "circle")) {
    d <- data.frame(x = seg$xend, y = seg$yend, colour = seg$colour,
                    stringsAsFactors = FALSE)
    return(list(ggplot2::geom_point(
      data = d, ggplot2::aes(x = .data$x, y = .data$y),
      colour = d$colour, fill = if (head == "ball") d$colour else "white",
      shape = 21, size = ps(.dg_head$dot * 2), stroke = linewidth, inherit.aes = FALSE)))
  }
  list()
}

# ---- text --------------------------------------------------------------------

# Wrapping at a measured width rather than a guessed character count. The data
# units are millimetres, the type size is in points, so the number of characters
# that fit is arithmetic rather than taste. 0.52 is the mean advance width of the
# grotesque faces journals specify, as a fraction of the type size.
dg_wrap <- function(label, width_mm, size_pt) {
  char_mm <- 0.52 * size_pt / .PT
  width_mm <- rep_len(width_mm, length(label))
  vapply(seq_along(label), function(i) {
    n <- max(6L, floor(width_mm[i] * 0.9 / max(char_mm, 1e-6)))
    # Wrap each authored line separately. `strwrap()` treats a newline as
    # ordinary whitespace, so wrapping the whole label at once silently runs
    # deliberately separated lines together -- which is exactly what an
    # exclusion box full of reasons must not do.
    lines <- strsplit(label[i], "\n", fixed = TRUE)[[1]]
    if (!length(lines)) return(label[i])
    paste(unlist(lapply(lines, function(l) {
      if (!nzchar(trimws(l))) "" else strwrap(l, width = n)
    })), collapse = "\n")
  }, character(1))
}

dg_label_lines <- function(x) vapply(strsplit(x, "\n", fixed = TRUE), length, integer(1))

# ---- assembly ----------------------------------------------------------------

#' Build a scientific diagram
#'
#' @description
#' Assembles nodes, edges and compartments into a schematic and returns a ggplot,
#' so the result carries a figure contract, obeys venue tokens, composes into a
#' multi-panel plate beside a chart, and is checked and exported by the same
#' machinery as every other vellum figure.
#'
#' Data units are millimetres at the diagram's natural size. That is not a
#' detail: it means a node is 24 mm wide because someone decided it should be
#' 24 mm wide, label wrapping is computed from a measured width against a type
#' size rather than guessed as a character count, and an arrowhead is the same
#' size in every figure this package draws. [diagram_size()] reports the natural
#' size to pass to [save_figure()].
#'
#' @param nodes A list of [dg_node()] objects, or a data frame with at least `id`
#'   and `label`.
#' @param edges A list of [dg_edge()] objects, or a data frame with `from`, `to`
#'   and `type`. May be `NULL` for a diagram of unconnected elements.
#' @param compartments A list of [dg_compartment()] objects.
#' @param layout `"layered"` ranks nodes by longest path from a source, so a node
#'   is drawn downstream of everything that feeds it. `"flow"` is the same in a
#'   single column, for participant flow. `"grid"`, `"radial"` and `"manual"` are
#'   also available.
#' @param direction `"down"`, `"up"`, `"right"` or `"left"`.
#' @param venue Venue whose tokens set type size, rule weight and typeface.
#' @param palette Palette used for node roles that carry no explicit colour.
#' @param node_width,node_height Default node size in millimetres.
#' @param spacing_x,spacing_y Distance between node centres in millimetres.
#' @param base_size Type size in points. Defaults to the venue's own.
#' @param edge_labels Draw the labels attached to edges.
#' @param acyclic Require the graph to be acyclic and fail if it is not. `FALSE`
#'   by default, because a feedback loop is a legitimate mechanism; [fig_dag()]
#'   sets it `TRUE`, because a cycle in a causal DAG is an error.
#'
#' @return A ggplot with the diagram's node and edge tables attached, and its
#'   natural size in the `vellum_diagram` attribute.
#'
#' @seealso [fig_consort()], [fig_dag()], [fig_pathway()], [fig_compartmental()],
#'   [fig_pipeline()], [fig_timeline()].
#' @export
#' @examples
#' diagram(
#'   nodes = list(
#'     dg_node("a", "Ligand", shape = "ellipse"),
#'     dg_node("b", "Receptor"),
#'     dg_node("c", "Response")
#'   ),
#'   edges = list(
#'     dg_edge("a", "b", "binding"),
#'     dg_edge("b", "c", "activation")
#'   )
#' )
diagram <- function(nodes, edges = NULL, compartments = NULL,
                    layout = c("layered", "flow", "grid", "radial", "manual"),
                    direction = c("down", "up", "right", "left"),
                    venue = "nature", palette = "okabe_ito",
                    node_width = 24, node_height = 9,
                    spacing_x = 30, spacing_y = 16,
                    base_size = NULL, edge_labels = TRUE, acyclic = FALSE) {
  layout <- match.arg(layout)
  direction <- match.arg(direction)
  check_number(node_width, "node_width", min = 1)
  check_number(node_height, "node_height", min = 1)
  check_number(spacing_x, "spacing_x", min = 1)
  check_number(spacing_y, "spacing_y", min = 1)
  check_bool(edge_labels, "edge_labels")
  check_bool(acyclic, "acyclic")

  tk <- vellum_tokens(venue, base_size = base_size)
  size_pt <- tk$type$size$annot
  ink <- tk$ink$text
  weak <- tk$ink$weak

  nodes <- dg_prepare_nodes(nodes, node_width, node_height)
  edges <- dg_prepare_edges(edges, nodes$id)
  compartments <- dg_bind(compartments, "compartments")

  if (acyclic && !is.null(edges) && nrow(edges)) {
    cyc <- dg_find_cycle(nodes$id, edges)
    if (!is.null(cyc)) {
      cli::cli_abort(c(
        "The graph contains a cycle: {.val {paste(cyc, collapse = ' -> ')}}.",
        i = "A directed acyclic graph cannot contain one; a cycle here usually means an edge points the wrong way."))
    }
  }

  if (layout == "flow") {
    nodes <- dg_layout(nodes, edges, "layered", direction, 0, spacing_y)
  } else {
    nodes <- dg_layout(nodes, edges, layout, direction, spacing_x, spacing_y)
  }

  edges <- dg_auto_curve(edges, nodes)
  role_cols <- dg_role_colours(nodes$role, palette)
  nodes$fill <- ifelse(is.na(nodes$fill), unname(role_cols$fill[nodes$role]), nodes$fill)
  nodes$colour <- ifelse(is.na(nodes$colour), unname(role_cols$line[nodes$role]), nodes$colour)
  nodes$fill[is.na(nodes$fill)] <- "#FFFFFF"
  nodes$colour[is.na(nodes$colour)] <- ink
  nodes$text_colour[is.na(nodes$text_colour)] <-
    vapply(nodes$fill[is.na(nodes$text_colour)], dg_readable_ink, character(1), ink = ink)

  # A label pushed off-centre has less room, not the same room somewhere else.
  nodes$label_wrapped <- dg_wrap(dg_node_text(nodes),
                                 nodes$width - 2 * abs(nodes$label_dx), size_pt)
  edges$colour[is.na(edges$colour)] <- ink
  edges$linewidth[is.na(edges$linewidth)] <- tk$rule$geom

  # Edge geometry is resolved once, here, because the diagram's extent depends on
  # it: a curved feedback edge or an elbow branch reaches outside the union of the
  # node boxes, and sizing the canvas to the nodes alone clips it.
  edge_geom <- dg_edge_geometry(edges, nodes)

  layers <- c(
    dg_compartment_layers(compartments, nodes, weak, size_pt, tk),
    dg_edge_layers(edges, nodes, size_pt, weak, edge_labels, edge_geom),
    dg_node_layers(nodes, size_pt, tk)
  )

  extent <- dg_extent(nodes, compartments, edge_geom)
  p <- ggplot2::ggplot() +
    layers +
    ggplot2::coord_fixed(ratio = 1, xlim = extent$xlim, ylim = extent$ylim,
                         expand = FALSE, clip = "off") +
    theme_vellum_void(venue = venue, base_size = base_size)

  attr(p, "vellum_diagram") <- list(
    width_mm = diff(extent$xlim), height_mm = diff(extent$ylim),
    nodes = nodes, edges = edges, compartments = compartments,
    venue = venue
  )
  p
}

#' The natural size of a diagram
#'
#' @description
#' A diagram's data units are millimetres, so it has a size before anything is
#' exported. Passing that size to [save_figure()] draws it one to one; passing
#' anything else scales it, and the type sizes then no longer mean what they say.
#'
#' @param plot A diagram from [diagram()] or any `fig_*` schematic recipe.
#' @param width Optional target width in millimetres, or a venue column name, to
#'   report the height that preserves the diagram's proportions.
#' @param venue Venue, when `width` is a column name.
#' @return A named numeric vector of `width` and `height` in millimetres.
#' @export
#' @examples
#' d <- diagram(list(dg_node("a", "Start"), dg_node("b", "End")),
#'              list(dg_edge("a", "b")))
#' diagram_size(d)
#' diagram_size(d, width = "single", venue = "nature")
diagram_size <- function(plot, width = NULL, venue = "nature") {
  meta <- attr(plot, "vellum_diagram", exact = TRUE)
  if (is.null(meta)) {
    cli::cli_abort("This plot is not a diagram; it carries no natural size.")
  }
  nat <- c(width = meta$width_mm, height = meta$height_mm)
  if (is.null(width)) return(round(nat, 2))
  target <- if (is.character(width)) venue_width(venue, width) else width
  check_number(target, "width", min = 1)
  round(c(width = target, height = target * meta$height_mm / meta$width_mm), 2)
}

#' @export
print.vellum_dg_node <- function(x, ...) {
  print.data.frame(x)
  invisible(x)
}

# ---- assembly helpers --------------------------------------------------------

dg_prepare_nodes <- function(nodes, node_width, node_height) {
  n <- dg_bind(nodes, "nodes")
  if (is.null(n) || !nrow(n)) cli::cli_abort("{.arg nodes} must describe at least one node.")
  if (!all(c("id", "label") %in% names(n))) {
    cli::cli_abort("{.arg nodes} needs at least {.field id} and {.field label}.")
  }
  dup <- unique(n$id[duplicated(n$id)])
  if (length(dup)) cli::cli_abort("Duplicated node id{?s}: {.val {dup}}.")

  defaults <- list(shape = "round", role = NA_character_, x = NA_real_, y = NA_real_,
                   width = NA_real_, height = NA_real_, fill = NA_character_,
                   colour = NA_character_, text_colour = NA_character_,
                   label_position = "inside", label_dx = 0, label_dy = 0,
                   count = NA_real_)
  for (nm in names(defaults)) if (is.null(n[[nm]])) n[[nm]] <- defaults[[nm]]
  # Positions given by the caller are remembered before the layout overwrites
  # them, which is what lets a mostly-automatic layout have a pinned node in it.
  n$.fixed_x <- n$x
  n$.fixed_y <- n$y
  n$width[is.na(n$width)] <- node_width
  n$height[is.na(n$height)] <- node_height
  n$role[is.na(n$role)] <- "default"
  n
}

dg_prepare_edges <- function(edges, ids) {
  e <- dg_bind(edges, "edges")
  if (is.null(e) || !nrow(e)) {
    return(data.frame(from = character(), to = character(), type = character(),
                      label = character(), route = character(), colour = character(),
                      linewidth = numeric(), curvature = numeric(),
                      stringsAsFactors = FALSE))
  }
  if (!all(c("from", "to") %in% names(e))) {
    cli::cli_abort("{.arg edges} needs at least {.field from} and {.field to}.")
  }
  for (nm in list(type = "flow", label = NA_character_, route = "straight",
                  colour = NA_character_, linewidth = NA_real_, curvature = 0)) NULL
  if (is.null(e$type)) e$type <- "flow"
  if (is.null(e$label)) e$label <- NA_character_
  if (is.null(e$route)) e$route <- "straight"
  if (is.null(e$colour)) e$colour <- NA_character_
  if (is.null(e$linewidth)) e$linewidth <- NA_real_
  if (is.null(e$curvature)) e$curvature <- 0

  unknown <- setdiff(unique(e$type), names(.dg_edge_types))
  if (length(unknown)) {
    cli::cli_abort("Unknown edge type{?s} {.val {unknown}}. Available: {.val {names(.dg_edge_types)}}.")
  }
  missing <- setdiff(unique(c(e$from, e$to)), ids)
  if (length(missing)) {
    cli::cli_abort(c("Edge{?s} refer to node{?s} that do not exist: {.val {missing}}.",
                     i = "Every {.field from} and {.field to} must name a node."))
  }
  e
}

# Depth-first search for a back edge, returning the cycle so the error can name
# it rather than merely assert one exists.
dg_find_cycle <- function(ids, edges) {
  state <- stats::setNames(rep(0L, length(ids)), ids)
  stack <- character()
  found <- NULL
  visit <- function(v) {
    if (!is.null(found)) return(invisible(NULL))
    state[[v]] <<- 1L
    stack <<- c(stack, v)
    for (w in edges$to[edges$from == v]) {
      if (!is.null(found)) break
      if (state[[w]] == 1L) {
        found <<- c(stack[which(stack == w)[1]:length(stack)], w)
        break
      }
      if (state[[w]] == 0L) visit(w)
    }
    state[[v]] <<- 2L
    stack <<- stack[-length(stack)]
    invisible(NULL)
  }
  for (v in ids) if (state[[v]] == 0L) visit(v)
  found
}

# An edge that passes behind a node it does not connect is not merely untidy: it
# is invisible, and an invisible edge is a missing claim. In a causal diagram laid
# out on one line, the arc from a confounder to the outcome runs exactly along the
# chain and disappears behind every node between them, so the figure silently
# stops asserting the confounding it was drawn to assert.
#
# Rather than curving every long edge on the assumption that it is occluded, this
# tests the actual geometry and bends only what is actually hidden, away from
# whichever side the obstruction sits on.
dg_auto_curve <- function(edges, nodes) {
  if (!nrow(edges)) return(edges)
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  for (k in which(abs(edges$curvature) < 1e-9 & edges$route == "straight")) {
    a <- nodes[idx[[edges$from[k]]], ]
    b <- nodes[idx[[edges$to[k]]], ]
    dx <- b$x - a$x; dy <- b$y - a$y
    len <- sqrt(dx^2 + dy^2)
    if (!is.finite(len) || len < 1e-9) next
    ux <- dx / len; uy <- dy / len
    others <- nodes[!nodes$id %in% c(a$id, b$id), , drop = FALSE]
    if (!nrow(others)) next

    t <- ((others$x - a$x) * ux + (others$y - a$y) * uy) / len
    perp <- (others$x - a$x) * (-uy) + (others$y - a$y) * ux
    clearance <- pmax(others$width, others$height) / 2 + 1.5
    blocking <- t > 0.02 & t < 0.98 & abs(perp) < clearance
    if (!any(blocking)) next

    # Bend away from the side the obstructions sit on, far enough to clear the
    # worst of them with a little air.
    side <- if (sum(perp[blocking] > 0) > sum(perp[blocking] < 0)) -1 else 1
    need <- max(clearance[blocking] - side * perp[blocking]) + 2
    edges$curvature[k] <- side * clamp(need / (len * 0.25) / 2, 0.12, 1.6)
  }
  edges
}

# Role colours come from the palette, not from a private list of pretty colours,
# so a diagram beside a chart in the same figure shares its colour system.
dg_role_colours <- function(roles, palette) {
  uniq <- setdiff(unique(roles), c("default", NA))
  if (!length(uniq)) {
    return(list(fill = stats::setNames(character(), character()),
                line = stats::setNames(character(), character())))
  }
  cols <- rep_len(vellum_pal(palette, n = min(length(uniq), 8L)), length(uniq))
  list(
    fill = stats::setNames(dg_tint(cols, 0.18), uniq),
    line = stats::setNames(cols, uniq)
  )
}

# A tint is mixed to an opaque colour rather than produced with alpha.
#
# Two reasons, and neither is aesthetic. Transparency in submission artwork is a
# recurring production problem -- it flattens unpredictably in CMYK conversion and
# some publishers reject it outright. And an alpha fill has no measurable colour,
# so the contrast check below would be reasoning about "#0072B233" rather than
# about the colour a reader actually sees.
dg_tint <- function(colour, amount, paper = "#FFFFFF") {
  rgb <- grDevices::col2rgb(colour)
  base <- grDevices::col2rgb(paper)[, 1]
  mixed <- rgb * amount + base * (1 - amount)
  grDevices::rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
}

# Enough contrast to be read, decided by measurement rather than by a guess about
# which fills count as dark.
dg_readable_ink <- function(fill, ink) {
  if (is.na(fill)) return(ink)
  if (contrast_ratio(ink, fill) >= 4.5) ink else "#FFFFFF"
}

dg_node_text <- function(nodes) {
  has_count <- !is.na(nodes$count)
  out <- nodes$label
  out[has_count] <- paste0(nodes$label[has_count], " (n = ",
                           format(nodes$count[has_count], big.mark = "\u2009",
                                  trim = TRUE, scientific = FALSE), ")")
  out
}

dg_extent <- function(nodes, compartments, edge_geom = NULL) {
  pad <- 3
  xs <- c(nodes$x - nodes$width / 2, nodes$x + nodes$width / 2)
  ys <- c(nodes$y - nodes$height / 2, nodes$y + nodes$height / 2)
  if (!is.null(edge_geom) && nrow(edge_geom)) {
    xs <- c(xs, edge_geom$x)
    ys <- c(ys, edge_geom$y)
  }
  if (!is.null(compartments) && nrow(compartments)) {
    boxes <- dg_compartment_boxes(compartments, nodes)
    xs <- c(xs, boxes$xmin, boxes$xmax)
    ys <- c(ys, boxes$ymin, boxes$ymax + 3)
  }
  list(xlim = c(min(xs) - pad, max(xs) + pad),
       ylim = c(min(ys) - pad, max(ys) + pad))
}

# ---- layers ------------------------------------------------------------------

dg_compartment_boxes <- function(compartments, nodes) {
  rows <- lapply(seq_len(nrow(compartments)), function(i) {
    ids <- compartments$contains[[i]]
    inside <- nodes[nodes$id %in% ids, , drop = FALSE]
    if (!nrow(inside)) return(NULL)
    pad <- compartments$pad[i] * 10
    data.frame(
      id = compartments$id[i], label = compartments$label[i],
      xmin = min(inside$x - inside$width / 2) - pad,
      xmax = max(inside$x + inside$width / 2) + pad,
      ymin = min(inside$y - inside$height / 2) - pad,
      ymax = max(inside$y + inside$height / 2) + pad,
      fill = compartments$fill[i], colour = compartments$colour[i],
      label_position = compartments$label_position[i],
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(data.frame(id = character(), label = character(), xmin = numeric(),
                      xmax = numeric(), ymin = numeric(), ymax = numeric(),
                      fill = character(), colour = character(),
                      label_position = character(), stringsAsFactors = FALSE))
  }
  out
}

dg_compartment_layers <- function(compartments, nodes, weak, size_pt, tk) {
  if (is.null(compartments) || !nrow(compartments)) return(list())
  boxes <- dg_compartment_boxes(compartments, nodes)
  if (!nrow(boxes)) return(list())
  boxes$fill[is.na(boxes$fill)] <- tk$ink$grid_minor
  boxes$colour[is.na(boxes$colour)] <- weak

  lab <- boxes
  lab$x <- ifelse(lab$label_position == "topleft", lab$xmin + 1.5, (lab$xmin + lab$xmax) / 2)
  lab$y <- ifelse(lab$label_position == "bottom", lab$ymin - 1.2, lab$ymax + 1.2)
  lab$hjust <- ifelse(lab$label_position == "topleft", 0, 0.5)
  lab$vjust <- ifelse(lab$label_position == "bottom", 1, 0)

  list(
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax),
      fill = boxes$fill, colour = boxes$colour,
      linewidth = tk$rule$hairline, linetype = "22", inherit.aes = FALSE),
    ggplot2::geom_text(
      data = lab,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   hjust = .data$hjust, vjust = .data$vjust),
      size = fs(size_pt * 0.92), colour = weak, fontface = "italic",
      family = tk$type$family, inherit.aes = FALSE)
  )
}

# Edge endpoints are computed against each node's own outline, so a line stops at
# the shape rather than at the centre of it.
dg_edge_geometry <- function(edges, nodes) {
  if (!nrow(edges)) return(edges)
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  out <- lapply(seq_len(nrow(edges)), function(k) {
    a <- nodes[idx[[edges$from[k]]], ]
    b <- nodes[idx[[edges$to[k]]], ]
    if (identical(edges$route[k], "elbow") && abs(a$x - b$x) > 1e-6) {
      # A right-angled branch: down from the source, across, into the target's
      # side. This is the routing participant-flow diagrams use, and drawing it
      # as a diagonal makes a branch look like a different kind of relation.
      p0 <- c(a$x, a$y - sign(a$y - b$y) * a$height / 2)
      if (abs(a$y - b$y) < 1e-6) p0 <- c(a$x + sign(b$x - a$x) * a$width / 2, a$y)
      p2 <- dg_boundary(b$shape, b$x, b$y, b$width, b$height,
                        b$x + sign(a$x - b$x) * b$width, b$y)
      mid <- c(p0[1], p2[2])
      return(data.frame(k = k, x = c(p0[1], mid[1], p2[1]), y = c(p0[2], mid[2], p2[2]),
                        stringsAsFactors = FALSE))
    }
    if (abs(edges$curvature[k]) > 1e-9) {
      mx <- (a$x + b$x) / 2; my <- (a$y + b$y) / 2
      dx <- b$x - a$x; dy <- b$y - a$y
      len <- max(sqrt(dx^2 + dy^2), 1e-9)
      # A quadratic Bezier passes through half of its control point's offset, so
      # the control point is placed at twice the requested deviation. Without the
      # doubling a feedback edge asked to clear a 24 mm node bows 8 mm and runs
      # straight through it.
      off <- 2 * edges$curvature[k] * len * 0.25
      ctrl <- c(mx - dy / len * off, my + dx / len * off)
      # A curved edge leaves and arrives along its own tangent, not along the
      # chord. Taking the boundary crossing toward the control point is what makes
      # a there-and-back pair -- entry and recovery between the same two states --
      # meet the box at two separate places instead of converging on one point and
      # looking like a single doubled arrow.
      p0 <- dg_boundary(a$shape, a$x, a$y, a$width, a$height, ctrl[1], ctrl[2])
      p1 <- dg_boundary(b$shape, b$x, b$y, b$width, b$height, ctrl[1], ctrl[2])
      t <- seq(0, 1, length.out = 24)
      return(data.frame(
        k = k,
        x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * ctrl[1] + t^2 * p1[1],
        y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * ctrl[2] + t^2 * p1[2],
        stringsAsFactors = FALSE))
    }
    p0 <- dg_boundary(a$shape, a$x, a$y, a$width, a$height, b$x, b$y)
    p1 <- dg_boundary(b$shape, b$x, b$y, b$width, b$height, a$x, a$y)
    data.frame(k = k, x = c(p0[1], p1[1]), y = c(p0[2], p1[2]), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

dg_edge_layers <- function(edges, nodes, size_pt, weak, edge_labels, geom = NULL) {
  if (!nrow(edges)) return(list())
  geom <- geom %||% dg_edge_geometry(edges, nodes)
  layers <- list()

  # Grouped by everything that affects the drawing, not by type alone. Two
  # activation edges in different colours are one type and two appearances, and
  # grouping on type would silently paint the second in the first's colour.
  style_key <- paste(edges$type, edges$colour, edges$linewidth, sep = "\r")
  for (key in unique(style_key)) {
    ks <- which(style_key == key)
    type <- edges$type[ks[1]]
    spec <- .dg_edge_types[[type]]
    g <- geom[geom$k %in% ks, , drop = FALSE]
    if (!nrow(g)) next

    # The line stops short of the tip for a filled head, so the head is the end
    # of the arrow rather than an overlay on top of a line that runs past it.
    trimmed <- do.call(rbind, lapply(split(g, g$k), function(part) {
      if (spec$head %in% c("arrow", "ball", "circle")) {
        m <- nrow(part)
        back <- if (spec$head == "arrow") .dg_head$length * 0.92 else .dg_head$dot
        dx <- part$x[m] - part$x[m - 1]; dy <- part$y[m] - part$y[m - 1]
        len <- max(sqrt(dx^2 + dy^2), 1e-9)
        part$x[m] <- part$x[m] - dx / len * min(back, len * 0.9)
        part$y[m] <- part$y[m] - dy / len * min(back, len * 0.9)
      }
      part
    }))
    lw <- edges$linewidth[ks][1]
    col <- edges$colour[ks][1]
    layers <- c(layers, list(ggplot2::geom_path(
      data = trimmed, ggplot2::aes(x = .data$x, y = .data$y, group = .data$k),
      colour = col, linewidth = lw, linetype = spec$linetype,
      lineend = "butt", inherit.aes = FALSE)))

    tips <- do.call(rbind, lapply(split(g, g$k), function(part) {
      m <- nrow(part)
      data.frame(x0h = part$x[m - 1], y0h = part$y[m - 1],
                 xend = part$x[m], yend = part$y[m],
                 colour = col, stringsAsFactors = FALSE)
    }))
    layers <- c(layers, dg_head_grobs(tips, spec$head, col, lw))
  }

  if (edge_labels && any(!is.na(edges$label))) {
    ks <- which(!is.na(edges$label))
    mids <- do.call(rbind, lapply(ks, function(k) {
      part <- geom[geom$k == k, , drop = FALSE]
      m <- ceiling(nrow(part) / 2)
      pos <- if (nrow(part) == 2L) {
        c(mean(part$x), mean(part$y))
      } else {
        c(part$x[m], part$y[m])
      }
      data.frame(x = pos[1], y = pos[2], label = edges$label[k], stringsAsFactors = FALSE)
    }))
    layers <- c(layers, list(ggplot2::geom_label(
      data = mids, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = fs(size_pt * 0.88), colour = weak, fill = "white",
      linewidth = 0, label.padding = grid::unit(0.4, "mm"),
      inherit.aes = FALSE)))
  }
  layers
}

dg_node_layers <- function(nodes, size_pt, tk) {
  layers <- list()
  shaped <- nodes[!nodes$shape %in% c("label", "asset"), , drop = FALSE]
  if (nrow(shaped)) {
    poly <- do.call(rbind, lapply(seq_len(nrow(shaped)), function(i) {
      pts <- dg_shape_points(shaped$shape[i], shaped$x[i], shaped$y[i],
                             shaped$width[i], shaped$height[i])
      data.frame(x = pts[, 1], y = pts[, 2], grp = shaped$id[i],
                 stringsAsFactors = FALSE)
    }))
    fills <- stats::setNames(shaped$fill, shaped$id)
    lines <- stats::setNames(shaped$colour, shaped$id)
    layers <- c(layers, list(ggplot2::geom_polygon(
      data = poly, ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
      fill = fills[poly$grp], colour = lines[poly$grp],
      linewidth = tk$rule$border, inherit.aes = FALSE)))
  }

  assets <- nodes[nodes$shape == "asset", , drop = FALSE]
  if (nrow(assets)) {
    store <- attr(nodes, "asset")
    for (i in seq_len(nrow(assets))) {
      svg <- store[[assets$id[i]]]
      if (is.null(svg)) next
      vb <- svg$viewbox
      w <- assets$width[i]
      h <- w / (vb[["width"]] / vb[["height"]])
      grob <- if (inherits(svg, "vellum_raster")) {
        grid::gTree(children = grid::gList(raster_grob(svg)))
      } else {
        asset_annotation_grob(svg)
      }
      layers <- c(layers, list(ggplot2::annotation_custom(
        grob,
        xmin = assets$x[i] - w / 2, xmax = assets$x[i] + w / 2,
        ymin = assets$y[i] - h / 2, ymax = assets$y[i] + h / 2)))
    }
  }

  inside <- nodes[nodes$label_position == "inside" & nodes$shape != "asset", , drop = FALSE]
  if (nrow(inside)) {
    inside$lx <- inside$x + inside$label_dx
    inside$ly <- inside$y + inside$label_dy
    layers <- c(layers, list(ggplot2::geom_text(
      data = inside,
      ggplot2::aes(x = .data$lx, y = .data$ly, label = .data$label_wrapped),
      size = fs(size_pt), colour = inside$text_colour, family = tk$type$family,
      lineheight = tk$type$lineheight, inherit.aes = FALSE)))
  }
  outside <- nodes[nodes$label_position != "inside" | nodes$shape == "asset", , drop = FALSE]
  if (nrow(outside)) {
    below <- outside$label_position != "above"
    outside$ly <- ifelse(below,
                         outside$y - outside$height / 2 - 1.4,
                         outside$y + outside$height / 2 + 1.4)
    layers <- c(layers, list(ggplot2::geom_text(
      data = outside,
      ggplot2::aes(x = .data$x, y = .data$ly, label = .data$label_wrapped),
      vjust = ifelse(below, 1, 0),
      size = fs(size_pt), colour = tk$ink$text, family = tk$type$family,
      lineheight = tk$type$lineheight, inherit.aes = FALSE)))
  }
  layers
}
