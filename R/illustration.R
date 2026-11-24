# Drawing imported illustrations, and restyling them.
#
# Two problems have to be solved together here.
#
# The first is stroke weight. An SVG stroke width is a length in the drawing's own
# coordinate system, so what it becomes on paper depends entirely on the size the
# illustration is finally drawn at. Fixing the weight when the grob is built means
# fixing it before that size is known, which is how a 0.5 pt rule becomes a 0.08 pt
# rule that disappears at the printer. So the geometry is built once, in the
# drawing's own units, and the weights are computed at draw time from the viewport
# that grid has actually established. `makeContent()` is the hook for that.
#
# The second is that a scientific illustration usually arrives in someone else's
# colours. A figure whose schematic is in stock teal and whose data are in the
# venue's palette reads as two figures glued together. Recolouring has to be
# semantic -- "the cell membrane becomes the primary role" -- rather than a global
# hue rotation that destroys the drawing's internal contrast.

# ---- grob construction -------------------------------------------------------

#' Draw an imported illustration as vector geometry
#'
#' @description
#' Turns a parsed illustration into a grid grob whose paths are real paths. The
#' grob keeps the drawing's aspect ratio, resolves its gradients and clipping
#' paths, and computes every stroke weight at draw time from the size the
#' illustration is actually being drawn at, so a weight stated in the SVG survives
#' as a weight on paper.
#'
#' @param x A `vellum_svg` object from [svg_read()] or [vellum_asset()].
#' @param colour Optional single ink. Every fill and stroke is replaced by it,
#'   preserving relative lightness, which is the fastest way to make a borrowed
#'   illustration belong to a figure. See [asset_recolour()] for role-level
#'   control.
#' @param alpha Opacity multiplier applied to the whole drawing.
#' @param keep_aspect Preserve the drawing's proportions within the space it is
#'   given. Leave `TRUE`: a stretched anatomical drawing is a wrong drawing.
#' @param min_linewidth Weight in points below which strokes are lifted. Publishers
#'   set a minimum because thinner rules break up on press; the default of `NULL`
#'   leaves the drawing untouched and lets [venue_check()] report the problem
#'   instead of hiding it.
#' @param name Grob name.
#'
#' @return A grid grob of class `vellum_asset`.
#' @seealso [annotation_asset()] to place one in a ggplot, [asset_recolour()] to
#'   restyle it.
#' @export
#' @examples
#' svg <- svg_read('<svg viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="#3B6EA5"/></svg>')
#' grid::grid.newpage()
#' grid::grid.draw(asset_grob(svg))
asset_grob <- function(x, colour = NULL, alpha = 1, keep_aspect = TRUE,
                       min_linewidth = NULL, name = NULL) {
  if (!inherits(x, "vellum_svg")) {
    cli::cli_abort("{.arg x} must come from {.fn svg_read} or {.fn vellum_asset}.")
  }
  check_number(alpha, "alpha", min = 0, max = 1)
  check_bool(keep_aspect, "keep_aspect")
  if (!is.null(min_linewidth)) check_number(min_linewidth, "min_linewidth", min = 0)
  if (!is.null(colour)) {
    check_colour(colour, "colour")
    x <- asset_recolour(x, ink = colour)
  }

  grid::gTree(
    svg = x, alpha = alpha, keep_aspect = keep_aspect, min_linewidth = min_linewidth,
    name = name %||% "vellum_asset",
    cl = "vellum_asset"
  )
}

# The grob as `annotation_custom()` needs it: nested one level deep.
#
# `GeomCustomAnn` draws its grob with `editGrob(grob, vp = <panel-region
# viewport>)`, which REPLACES the outer grob's own viewport. A `vellum_asset`
# sizes itself in `makeContext()` by measuring the viewport it finds itself in, so
# without a wrapper it measures the whole panel and the illustration is drawn at
# panel size. The wrapper absorbs the substitution; the asset inside then measures
# the region it was actually given. This is the same trap `annotate_north_arrow()`
# documents, and it is silent -- the figure renders, enormously.
asset_annotation_grob <- function(...) {
  grid::gTree(children = grid::gList(asset_grob(...)))
}

#' @exportS3Method grid::makeContext
makeContext.vellum_asset <- function(x) {
  vb <- x$svg$viewbox
  w_in <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  h_in <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  if (isTRUE(x$keep_aspect) && is.finite(w_in) && is.finite(h_in) && w_in > 0 && h_in > 0) {
    aspect <- vb[["width"]] / vb[["height"]]
    if (w_in / h_in > aspect) w_in <- h_in * aspect else h_in <- w_in / aspect
  }
  # The y scale is inverted because SVG counts downward from the top and grid
  # counts upward from the bottom. Flipping the scale rather than the coordinates
  # keeps the parsed geometry identical to the source document, which matters when
  # a coordinate has to be checked against the file.
  vp <- grid::viewport(
    width = grid::unit(w_in, "in"), height = grid::unit(h_in, "in"),
    xscale = c(vb[["x"]], vb[["x"]] + vb[["width"]]),
    yscale = c(vb[["y"]] + vb[["height"]], vb[["y"]])
  )
  x$vp <- if (is.null(x$vp)) vp else grid::vpStack(x$vp, vp)
  x
}

#' @exportS3Method grid::makeContent
makeContent.vellum_asset <- function(x) {
  svg <- x$svg
  # Inches per user unit, measured in the viewport grid has just established.
  # This is the number that turns a stroke width in the file into a weight on
  # paper, and it cannot be known any earlier than this.
  unit_in <- grid::convertWidth(grid::unit(1, "native"), "in", valueOnly = TRUE)
  if (!is.finite(unit_in) || unit_in <= 0) unit_in <- 0

  children <- list()
  for (i in seq_along(svg$shapes)) {
    s <- svg$shapes[[i]]
    gl <- asset_shape_grobs(s, svg, x, unit_in, i)
    if (length(gl)) children <- c(children, gl)
  }
  for (i in seq_along(svg$texts)) {
    t <- svg$texts[[i]]
    children <- c(children, list(grid::textGrob(
      label = t$label, x = t$x, y = t$y, default.units = "native",
      hjust = switch(t$anchor, middle = 0.5, end = 1, 0),
      gp = grid::gpar(
        col = scales::alpha(t$colour, x$alpha),
        fontsize = t$size_user * unit_in * 72.27,
        fontfamily = as.character(resolve_font())
      ),
      name = paste0("text-", i)
    )))
  }
  grid::setChildren(x, do.call(grid::gList, children))
}

# One shape becomes one or two grobs. A closed shape can be filled and stroked by
# a single path, but an open one cannot: `pathGrob()` closes every subpath, so
# stroking an open polyline with it draws a closing edge that is not in the
# drawing. Those get an explicit polyline for the stroke.
asset_shape_grobs <- function(s, svg, x, unit_in, index) {
  pts <- do.call(rbind, s$subpaths)
  if (is.null(pts) || !nrow(pts)) return(list())
  lens <- vapply(s$subpaths, nrow, integer(1))

  fill <- asset_resolve_paint(s$fill, s, svg, s$fill_opacity * s$opacity * x$alpha)
  stroke_alpha <- s$stroke_opacity * s$opacity * x$alpha
  stroke <- if (is.na(s$stroke)) NA else scales::alpha(s$stroke, stroke_alpha)

  lwd <- s$stroke_width_user * unit_in * 96
  if (!is.null(x$min_linewidth)) {
    lwd <- max(lwd, x$min_linewidth / 72.27 * 96)
  }
  if (!is.na(stroke) && lwd <= 0) stroke <- NA

  all_closed <- all(s$closed)
  gp_common <- list(
    lineend = asset_linecap(s$linecap), linejoin = asset_linejoin(s$linejoin),
    lty = asset_dash(s$dash, s$stroke_width_user)
  )

  out <- list()
  if (all_closed) {
    gp <- do.call(grid::gpar, c(gp_common, list(
      fill = fill, col = stroke,
      lwd = if (is.na(stroke)) 0 else lwd)))
    out[[1]] <- grid::pathGrob(pts[, 1], pts[, 2], id.lengths = lens,
                               rule = s$rule, default.units = "native",
                               gp = gp, name = paste0("shape-", index))
  } else {
    if (!is.na(fill[1]) || inherits(fill, "GridPattern")) {
      out[[length(out) + 1L]] <- grid::pathGrob(
        pts[, 1], pts[, 2], id.lengths = lens, rule = s$rule,
        default.units = "native",
        gp = grid::gpar(fill = fill, col = NA, lwd = 0),
        name = paste0("shape-fill-", index))
    }
    if (!is.na(stroke)) {
      out[[length(out) + 1L]] <- grid::polylineGrob(
        pts[, 1], pts[, 2], id.lengths = lens, default.units = "native",
        gp = do.call(grid::gpar, c(gp_common, list(col = stroke, lwd = lwd))),
        name = paste0("shape-stroke-", index))
    }
  }
  if (!length(out)) return(list())

  clip <- asset_clip_grob(s, svg)
  if (!is.null(clip)) {
    return(list(grid::gTree(children = do.call(grid::gList, out),
                            vp = grid::viewport(clip = clip),
                            name = paste0("clipped-", index))))
  }
  out
}

# A gradient reference becomes a real grid gradient. Everything else is a colour.
asset_resolve_paint <- function(value, s, svg, alpha) {
  if (is.null(value) || is.na(value)) return(NA)
  if (!grepl("^url\\(", value)) return(scales::alpha(value, alpha))
  id <- svg_clip_ref(value)
  g <- if (!is.null(id)) svg$gradients[[id]] else NULL
  if (is.null(g) || !length(g$colours)) return(NA)

  cols <- scales::alpha(g$colours, g$alpha * alpha)
  stops <- g$offsets
  ord <- order(stops)
  cols <- cols[ord]; stops <- stops[ord]
  if (length(cols) == 1L) return(cols)

  bbox <- asset_bbox(s)
  coord <- function(key, default, axis) {
    v <- g$coords[[key]]
    if (is.null(v)) return(default)
    if (identical(g$units, "objectBoundingBox")) {
      frac <- if (grepl("%$", v)) svg_number(sub("%$", "", v), 0) / 100 else svg_number(v, 0)
      if (axis == "x") bbox$xmin + frac * (bbox$xmax - bbox$xmin)
      else if (axis == "y") bbox$ymin + frac * (bbox$ymax - bbox$ymin)
      else frac * max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
    } else {
      svg_number(v, default)
    }
  }

  res <- try(if (identical(g$type, "radial")) {
    cx <- coord("cx", (bbox$xmin + bbox$xmax) / 2, "x")
    cy <- coord("cy", (bbox$ymin + bbox$ymax) / 2, "y")
    r <- coord("r", max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin) / 2, "r")
    grid::radialGradient(cols, stops = stops,
                         cx1 = cx, cy1 = cy, r1 = 0,
                         cx2 = cx, cy2 = cy, r2 = max(r, 1e-6),
                         default.units = "native")
  } else {
    grid::linearGradient(cols, stops = stops,
                         x1 = coord("x1", bbox$xmin, "x"), y1 = coord("y1", bbox$ymin, "y"),
                         x2 = coord("x2", bbox$xmax, "x"), y2 = coord("y2", bbox$ymin, "y"),
                         default.units = "native")
  }, silent = TRUE)

  # A device that cannot draw gradients is not a reason to draw nothing. The
  # midpoint stop is the honest single-colour summary of the ramp.
  if (inherits(res, "try-error")) cols[ceiling(length(cols) / 2)] else res
}

asset_bbox <- function(s) {
  pts <- do.call(rbind, s$subpaths)
  list(xmin = min(pts[, 1]), xmax = max(pts[, 1]),
       ymin = min(pts[, 2]), ymax = max(pts[, 2]))
}

asset_clip_grob <- function(s, svg) {
  id <- svg_clip_ref(s$clip)
  if (is.null(id)) return(NULL)
  shapes <- svg$clips[[id]]
  if (is.null(shapes) || !length(shapes)) return(NULL)
  # The clipping geometry is stated in the user space of the element that refers
  # to it, so it takes that element's transform.
  sub <- unlist(lapply(shapes, function(cs) svg_transform_subpaths(cs$subpaths, s$ctm)),
                recursive = FALSE)
  pts <- do.call(rbind, sub)
  if (is.null(pts) || !nrow(pts)) return(NULL)
  grid::pathGrob(pts[, 1], pts[, 2], id.lengths = vapply(sub, nrow, integer(1)),
                 rule = "winding", default.units = "native")
}

# SVG and grid agree on what these joins look like and disagree on how to spell
# one of them: SVG says "miter", grid says "mitre". Passing the SVG spelling
# straight through is an error at draw time, after the figure has been built.
asset_linejoin <- function(x) {
  switch(tolower(x %||% "miter"),
         round = "round", bevel = "bevel",
         miter = , mitre = , `miter-clip` = , arcs = "mitre",
         "mitre")
}

asset_linecap <- function(x) {
  switch(tolower(x %||% "butt"),
         round = "round", square = "square", butt = "butt", "butt")
}

# grid states a dash pattern in multiples of the line width, SVG in user units.
asset_dash <- function(dash, width) {
  if (is.null(dash) || is.na(dash) || !nzchar(dash) || identical(tolower(dash), "none")) {
    return("solid")
  }
  nums <- suppressWarnings(as.numeric(strsplit(trimws(dash), "[,[:space:]]+")[[1]]))
  nums <- nums[is.finite(nums) & nums >= 0]
  if (!length(nums) || width <= 0) return("solid")
  digits <- clamp(round(nums / width), 1, 9)
  if (length(digits) %% 2L == 1L) digits <- c(digits, digits)
  paste(format(as.hexmode(digits)), collapse = "")
}

# ---- recolouring -------------------------------------------------------------

#' Restyle an illustration to belong to the figure
#'
#' @description
#' An illustration borrowed from a corpus arrives in its author's colours. Dropping
#' it into a figure unchanged produces two colour systems in one frame. This
#' rewrites the drawing's palette while keeping its internal structure legible.
#'
#' Three modes, in increasing order of control:
#'
#' * `ink` collapses the drawing to one hue, mapping each original colour to the
#'   lightness it already had. This is what a journal schematic usually wants and
#'   it cannot destroy the drawing's internal contrast, because it preserves the
#'   quantity that carries it.
#' * `palette` maps the drawing's distinct colours, ordered by how much of the
#'   drawing they cover, onto a named vellum palette.
#' * `map` renames specific colours, which is how a semantic role is assigned: the
#'   membrane is this colour, the nucleus is that one.
#'
#' @param x A `vellum_svg` object.
#' @param ink A single colour. The drawing becomes a monochrome study in it.
#' @param palette A vellum palette name, or a vector of colours.
#' @param map A named character vector, `old = new`, matched case-insensitively
#'   against the hex values reported by [svg_palette()].
#' @param stroke Optional single colour for every stroke, applied after the above,
#'   which is how line art gets a uniform outline.
#' @param preserve Colours never rewritten. Defaults to white, so highlights and
#'   knocked-out detail survive; pass `character()` to recolour those too.
#'
#' @return A `vellum_svg` object with its colours rewritten.
#' @export
#' @examples
#' svg <- svg_read(
#'   '<svg viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="#B15928"/></svg>'
#' )
#' svg_palette(asset_recolour(svg, ink = "#1B3A5C"))
asset_recolour <- function(x, ink = NULL, palette = NULL, map = NULL,
                           stroke = NULL, preserve = "#FFFFFF") {
  if (!inherits(x, "vellum_svg")) {
    cli::cli_abort("{.arg x} must come from {.fn svg_read} or {.fn vellum_asset}.")
  }
  n_given <- sum(!is.null(ink), !is.null(palette), !is.null(map))
  if (n_given > 1L) {
    cli::cli_abort(c("Give only one of {.arg ink}, {.arg palette}, or {.arg map}.",
                     i = "They are three ways to decide the same thing."))
  }
  preserve <- toupper(preserve %||% character())

  current <- svg_palette(x)
  originals <- setdiff(unique(toupper(current$colour)), preserve)
  lookup <- NULL

  if (!is.null(ink)) {
    check_colour(ink, "ink")
    if (length(ink) != 1L) cli::cli_abort("{.arg ink} must be a single colour.")
    lookup <- stats::setNames(asset_tint(ink, originals), originals)
  } else if (!is.null(palette)) {
    cols <- if (length(palette) == 1L && !grepl("^#", palette[1])) {
      vellum_pal(palette, n = min(length(originals), 8L))
    } else {
      check_colour(palette, "palette")
      palette
    }
    if (!length(cols)) cli::cli_abort("{.arg palette} resolved to no colours.")
    # Ordered by coverage so the drawing's dominant colour takes the palette's
    # first entry, which is the one the rest of the figure is built on.
    by_use <- stats::aggregate(list(n = current$n), list(colour = toupper(current$colour)), sum)
    by_use <- by_use[order(-by_use$n), , drop = FALSE]
    ordered <- setdiff(by_use$colour, preserve)
    lookup <- stats::setNames(rep_len(cols, length(ordered)), ordered)
  } else if (!is.null(map)) {
    if (!is.character(map) || is.null(names(map))) {
      cli::cli_abort("{.arg map} must be a named character vector of {.code old = new} colours.")
    }
    check_colour(unname(map), "map")
    lookup <- stats::setNames(unname(map), toupper(names(map)))
  }

  apply_one <- function(col) {
    if (is.null(col) || is.na(col) || grepl("^url\\(", col)) return(col)
    up <- toupper(col)
    if (up %in% preserve) return(col)
    # Single-bracket lookup: `[[` on a name that is not present is an error, and
    # a colour the map does not mention is the normal case, not a fault.
    hit <- if (!is.null(lookup)) unname(lookup[up]) else NA_character_
    if (length(hit) != 1L || is.na(hit)) col else hit
  }

  x$shapes <- lapply(x$shapes, function(s) {
    s$fill <- apply_one(s$fill)
    s$stroke <- if (!is.null(stroke) && !is.na(s$stroke)) stroke else apply_one(s$stroke)
    s
  })
  if (!is.null(stroke)) check_colour(stroke, "stroke")
  x$gradients <- lapply(x$gradients, function(g) {
    if (length(g$colours)) g$colours <- vapply(g$colours, apply_one, character(1), USE.NAMES = FALSE)
    g
  })
  x
}

# Map a set of colours onto one hue, keeping each colour's lightness. Working in
# a perceptual space rather than in RGB is what stops a dark outline and a mid
# fill from collapsing onto each other.
asset_tint <- function(ink, colours) {
  if (!length(colours)) return(character())
  target <- as_polar(ink)
  src <- as_polar(colours)
  L <- src[, "L"]
  # Compress toward the ink's own lightness so a drawing built on near-white and
  # near-black does not become a figure with no mid tones.
  Lr <- range(L)
  if (diff(Lr) < 1e-6) {
    L_new <- rep(target[, "L"], length(L))
  } else {
    span <- min(0.72, max(0.32, diff(Lr)))
    centre <- target[, "L"]
    L_new <- clamp(centre + (L - mean(Lr)) / diff(Lr) * span, 0.06, 0.98)
  }
  C_new <- clamp(target[, "C"] * (0.35 + 0.65 * (src[, "C"] / max(src[, "C"], 1e-6))), 0, 0.4)
  from_polar(cbind(L = L_new, C = C_new, h = rep(target[, "h"], length(L))))
}

# ---- placement ---------------------------------------------------------------

#' Place an illustration inside a plot panel
#'
#' @description
#' Adds an imported illustration to a ggplot at a stated physical size, positioned
#' in panel-relative coordinates so it stays put when the data change. The
#' illustration remains vector geometry.
#'
#' @param x A `vellum_svg` object, or a path to an SVG file.
#' @param xmin,xmax,ymin,ymax Extent in data coordinates. Give these, or give
#'   `x_npc`/`y_npc` with `width`.
#' @param x_npc,y_npc Position within the panel, from 0 to 1.
#' @param width Width in millimetres, when positioning by `x_npc`/`y_npc`.
#' @param just Justification at `x_npc`/`y_npc`.
#' @param ... Passed to [asset_grob()].
#'
#' @return A ggplot2 annotation layer.
#' @export
#' @examples
#' library(ggplot2)
#' svg <- svg_read('<svg viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="#3B6EA5"/></svg>')
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   annotation_asset(svg, x_npc = 0.85, y_npc = 0.85, width = 12) +
#'   theme_vellum()
annotation_asset <- function(x, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
                             x_npc = NULL, y_npc = NULL, width = 12,
                             just = c("centre", "centre"), ...) {
  if (is.character(x)) {
    x <- if (grepl("\\.png$", x, ignore.case = TRUE)) raster_read(x) else svg_read(x)
  }
  grob <- if (inherits(x, "vellum_raster")) raster_grob(x, ...) else asset_grob(x, ...)

  if (!is.null(x_npc) || !is.null(y_npc)) {
    check_number(x_npc %||% 0.5, "x_npc", min = 0, max = 1)
    check_number(y_npc %||% 0.5, "y_npc", min = 0, max = 1)
    check_number(width, "width", min = 0.1)
    vb <- x$viewbox
    height <- width / (vb[["width"]] / vb[["height"]])
    # Nested one level deep for the same reason as `annotate_north_arrow()`:
    # `annotation_custom()` replaces the outer grob's viewport, so a grob sized in
    # millimetres has to hand that substitution to a wrapper and keep its own.
    sized <- grid::gTree(
      children = grid::gList(grob),
      vp = grid::viewport(x = x_npc %||% 0.5, y = y_npc %||% 0.5, just = just,
                          width = grid::unit(width, "mm"),
                          height = grid::unit(height, "mm"))
    )
    return(ggplot2::annotation_custom(grid::gTree(children = grid::gList(sized)),
                                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf))
  }

  if (is.null(xmin) || is.null(xmax) || is.null(ymin) || is.null(ymax)) {
    cli::cli_abort(c("Give the illustration a position.",
                     i = "Either {.arg xmin}/{.arg xmax}/{.arg ymin}/{.arg ymax} in data units,",
                     i = "or {.arg x_npc}/{.arg y_npc} with {.arg width} in millimetres."))
  }
  ggplot2::annotation_custom(grid::gTree(children = grid::gList(grob)),
                             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

#' Measure an illustration at its final printed size
#'
#' @description
#' Reports the stroke weights and text sizes the drawing will actually have once it
#' is placed at a given width, in points. This is the measurement that makes an
#' imported illustration checkable: a publisher's minimum rule weight applies to
#' the artwork as printed, not to the numbers in the file.
#'
#' @param x A `vellum_svg` object.
#' @param width Final drawn width in millimetres.
#' @return A list with `linewidths_pt`, `text_pt`, `min_linewidth_pt`,
#'   `height_mm`, and `rasters`.
#' @export
#' @examples
#' svg <- svg_read(
#'   '<svg viewBox="0 0 100 50"><path d="M0,0 L100,50" stroke="#000" stroke-width="0.5"/></svg>'
#' )
#' asset_measure(svg, width = 40)
asset_measure <- function(x, width) {
  if (!inherits(x, "vellum_svg")) {
    cli::cli_abort("{.arg x} must come from {.fn svg_read} or {.fn vellum_asset}.")
  }
  check_number(width, "width", min = 0.1)
  vb <- x$viewbox
  mm_per_unit <- width / vb[["width"]]
  drawn <- vapply(x$shapes, function(s) {
    if (is.na(s$stroke)) NA_real_ else s$stroke_width_user * mm_per_unit
  }, numeric(1))
  drawn <- drawn[is.finite(drawn) & drawn > 0]
  txt <- vapply(x$texts, function(t) t$size_user * mm_per_unit, numeric(1))
  list(
    linewidths_pt = sort(unique(round(mm_pt(drawn), 3))),
    text_pt = sort(unique(round(mm_pt(txt), 3))),
    min_linewidth_pt = if (length(drawn)) mm_pt(min(drawn)) else NA_real_,
    height_mm = width / (vb[["width"]] / vb[["height"]]),
    rasters = x$rasters %||% 0L
  )
}

# ---- discovery ---------------------------------------------------------------

# Find the imported illustrations inside a plot, so that pre-flight can measure
# them. Two placements have to be recognised: `annotation_asset()` wraps the grob
# in a viewport sized in millimetres, and `diagram()` places it with data-unit
# extents that are already millimetres. Anything else has no knowable physical
# width until the plot is built, and is reported without one.
illustration_layers <- function(plot) {
  layers <- tryCatch(plot$layers, error = function(e) NULL)
  if (is.null(layers) && inherits(plot, "patchwork")) {
    layers <- unlist(lapply(plot$patches$plots, function(p) p$layers), recursive = FALSE)
    layers <- c(layers, tryCatch(plot$layers, error = function(e) NULL))
  }
  if (!length(layers)) return(list())

  out <- list()
  for (l in layers) {
    grob <- tryCatch(l$geom_params$grob, error = function(e) NULL)
    if (is.null(grob)) next
    found <- illustration_find(grob)
    if (!length(found)) next
    xmin <- l$geom_params$xmin
    xmax <- l$geom_params$xmax
    span <- if (is.numeric(xmin) && is.numeric(xmax) && is.finite(xmin) && is.finite(xmax)) {
      abs(xmax - xmin)
    } else {
      NULL
    }
    for (f in found) {
      out[[length(out) + 1L]] <- list(svg = f$svg, width_mm = f$width_mm %||% span)
    }
  }
  out
}

illustration_find <- function(grob, depth = 0L) {
  if (depth > 6L || is.null(grob)) return(list())
  if (inherits(grob, "vellum_asset")) {
    return(list(list(svg = grob$svg, width_mm = NULL)))
  }
  if (inherits(grob, "vellum_raster_grob")) {
    return(list(list(svg = attr(grob, "vellum_raster"), width_mm = NULL)))
  }
  kids <- tryCatch(grob$children, error = function(e) NULL)
  if (is.null(kids)) return(list())
  # A viewport width stated in millimetres is the physical size the caller asked
  # for, and it survives into the child that carries the drawing.
  vp_mm <- illustration_vp_mm(grob$vp)
  found <- unlist(lapply(kids, illustration_find, depth = depth + 1L), recursive = FALSE)
  if (!is.null(vp_mm)) {
    found <- lapply(found, function(f) { f$width_mm <- f$width_mm %||% vp_mm; f })
  }
  found
}

illustration_vp_mm <- function(vp) {
  if (is.null(vp)) return(NULL)
  w <- tryCatch(vp$width, error = function(e) NULL)
  if (is.null(w) || !inherits(w, "unit")) return(NULL)
  val <- tryCatch(as.numeric(grid::convertWidth(w, "mm")), error = function(e) NA_real_)
  if (length(val) != 1L || !is.finite(val) || val <= 0) NULL else val
}

# ---- raster illustration -----------------------------------------------------

# Not every scientific illustration is vector, and pretending otherwise would be
# its own dishonesty. Photographic and painted image-bank artwork -- the IYCF and
# UNICEF material a nutrition figure is actually built from -- exists only as
# raster, and there is no conversion that makes it line art.
#
# So raster assets are supported and labelled. `venue_check()` already knows what
# to do with raster content: it reports the resolution requirement and, where a
# publisher requires vector line art, says the figure will need that supplied
# separately. What it must not do is stay silent, which is what happens when a
# raster is pasted in with `annotation_custom()` by hand.

#' Load a raster illustration
#'
#' @description
#' Reads a PNG into a placeable illustration. Use this for painted or
#' photographic image-bank artwork, which has no vector form; use [svg_read()]
#' for line art, which does.
#'
#' The result reports itself as raster, so [venue_check()] applies the venue's
#' resolution requirement to it instead of the vector rules, and says so.
#'
#' @param file Path to a PNG file.
#' @param name Optional display name, used by the licence ledger and the figure's
#'   provenance.
#' @param source,licence,author Optional provenance recorded with the image, for
#'   artwork that does not come from a registered corpus.
#'
#' @return An object of class `vellum_raster`.
#' @seealso [svg_read()] for vector artwork, [annotation_asset()] to place it.
#' @export
#' @examples
#' f <- tempfile(fileext = ".png")
#' png::writePNG(array(runif(300), dim = c(10, 10, 3)), f)
#' raster_read(f)
raster_read <- function(file, name = NULL, source = NULL, licence = NULL,
                        author = NULL) {
  check_string(file, "file")
  check_installed("png", "to read raster illustrations")
  # Argument first, filesystem second: a caller who passed a JPEG should be told
  # that, whether or not the path also happens to be wrong.
  if (!grepl("\\.png$", file, ignore.case = TRUE)) {
    cli::cli_abort(c("{.arg file} must be a PNG.",
                     i = "Convert other formats once, rather than at every render."))
  }
  if (!file.exists(file)) cli::cli_abort("No image at {.path {file}}.")
  img <- png::readPNG(file)
  dims <- dim(img)
  structure(list(
    image = img,
    width_px = dims[2], height_px = dims[1],
    viewbox = c(x = 0, y = 0, width = dims[2], height = dims[1]),
    provenance = list(
      id = basename(file), name = name %||% tools::file_path_sans_ext(basename(file)),
      corpus = source %||% NA_character_,
      licence_recorded = licence %||% "not stated",
      licence_recorded_from = "supplied",
      licence_stated_in_file = NA_character_,
      licence_disputed = FALSE,
      share_alike = asset_is_share_alike(licence %||% ""),
      author = author %||% NA_character_, credit = NA_character_,
      source = source %||% NA_character_, source_url = NA_character_,
      file = file),
    file = file
  ), class = "vellum_raster")
}

#' @export
print.vellum_raster <- function(x, ...) {
  cli::cli_h2("Raster illustration")
  cli::cli_text("{.strong Image} {x$width_px} \u00d7 {x$height_px} px")
  cli::cli_text("{.strong Name} {x$provenance$name}")
  if (!is.na(x$provenance$licence_recorded)) {
    cli::cli_text("{.strong Terms} {x$provenance$licence_recorded}")
  }
  cli::cli_bullets(c("!" = "Raster, so it carries a resolution requirement rather than a rule-weight one."))
  invisible(x)
}

#' @rdname asset_grob
#' @export
raster_grob <- function(x, keep_aspect = TRUE, name = NULL) {
  if (!inherits(x, "vellum_raster")) {
    cli::cli_abort("{.arg x} must come from {.fn raster_read}.")
  }
  check_bool(keep_aspect, "keep_aspect")
  g <- grid::rasterGrob(
    x$image, interpolate = TRUE,
    width = if (keep_aspect) NULL else grid::unit(1, "npc"),
    height = if (keep_aspect) NULL else grid::unit(1, "npc"),
    name = name %||% "vellum_raster")
  # Tagged so pre-flight can find the image again and work out the resolution it
  # will actually have at the figure's printed size.
  attr(g, "vellum_raster") <- x
  class(g) <- c("vellum_raster_grob", class(g))
  g
}

# The effective resolution an image will have at a given printed width. This is
# the number a publisher's dpi requirement is actually about, and it falls as the
# image is enlarged, which is the failure nobody notices until proof stage.
raster_dpi <- function(x, width_mm) {
  x$width_px / (width_mm / 25.4)
}

#' Report the illustrations placed in a figure
#'
#' @description
#' Lists every illustration in a plot with the size it is drawn at and the
#' measurement that governs it: rule weight in points for vector artwork,
#' effective resolution in dots per inch for raster.
#'
#' This is the table to look at when [venue_check()] reports an illustration
#' problem, because it says which drawing is binding and by how much. For raster
#' artwork the effective resolution is fixed by the source image and the size it
#' is enlarged to, so a shortfall is answered by a better original or a smaller
#' placement, and by nothing else.
#'
#' @param plot A ggplot or patchwork.
#' @param width Figure width in millimetres, used for any illustration whose
#'   drawn size cannot be resolved from the plot itself.
#'
#' @return A data frame with one row per illustration. Zero rows if the figure
#'   contains none.
#' @export
#' @examples
#' library(ggplot2)
#' svg <- svg_read(
#'   '<svg viewBox="0 0 100 50"><path d="M0,0 L100,50" stroke="#000" stroke-width="0.5"/></svg>')
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   annotation_asset(svg, x_npc = 0.8, y_npc = 0.8, width = 30) + theme_vellum()
#' illustration_report(p)
illustration_report <- function(plot, width = 89) {
  check_number(width, "width", min = 1)
  found <- illustration_layers(plot)
  if (!length(found)) {
    return(data.frame(illustration = character(), kind = character(),
                      drawn_mm = numeric(), source = character(),
                      measure = character(), value = numeric(),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(found, function(a) {
    w <- a$width_mm %||% width
    if (inherits(a$svg, "vellum_raster")) {
      data.frame(
        illustration = a$svg$provenance$name, kind = "raster",
        drawn_mm = round(w, 1), source = paste0(a$svg$width_px, " px"),
        measure = "effective dpi", value = round(raster_dpi(a$svg, w)),
        stringsAsFactors = FALSE)
    } else {
      m <- asset_measure(a$svg, w)
      data.frame(
        illustration = a$svg$provenance$name %||% "vector illustration",
        kind = "vector", drawn_mm = round(w, 1),
        source = paste0(length(a$svg$shapes), " shapes"),
        measure = "thinnest rule, pt", value = round(m$min_linewidth_pt, 3),
        stringsAsFactors = FALSE)
    }
  })
  out <- do.call(rbind, rows)
  out[order(out$value), , drop = FALSE]
}
