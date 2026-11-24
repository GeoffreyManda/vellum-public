# Themes.
#
# All of these are thin: they read tokens and translate them into ggplot2
# elements. There is no visual constant in this file, by design. Change
# vellum_tokens() and every theme moves with it.
#
# ggplot2 4.0 lets a theme carry the colour palettes and the default geom
# appearance. vellum uses both, so applying theme_vellum() gives audited colours
# and print-correct rule weights with no scale calls at all. That is the single
# largest ergonomic difference between this package and a conventional theme
# collection, where the theme styles the furniture and leaves the data looking
# like a default.

#' Shape and linetype sets for redundant encoding
#'
#' @description
#' Colour must never be the only channel carrying meaning: roughly one man in
#' twelve of northern-European descent has a red-green deficiency, and every
#' figure is eventually photocopied. These are the redundant channels.
#'
#' The shapes are chosen for distinct silhouettes rather than for variety, so they
#' remain separable at 1.5 mm. The linetypes are given as dash-array strings
#' rather than ggplot2's named types, because at a 0.3 pt rule the named dashes
#' are too coarse to distinguish.
#'
#' @param n Number of levels needed.
#' @return A numeric vector of `pch` codes, or a character vector of dash arrays.
#' @export
#' @examples
#' vellum_shapes(4)
#' vellum_linetypes(4)
vellum_shapes <- function(n = 8) {
  set <- c(16, 17, 15, 18, 8, 4, 3, 1)
  check_number(n, "n", min = 1)
  n <- as.integer(n)
  if (n > length(set)) {
    cli::cli_abort(c("Only {length(set)} distinguishable shapes are defined; {n} were requested.",
                     i = "Beyond about six, shape stops being a usable channel."))
  }
  set[seq_len(n)]
}

#' @rdname vellum_shapes
#' @export
vellum_linetypes <- function(n = 6) {
  set <- c("solid", "22", "4222", "42", "1242", "12")
  check_number(n, "n", min = 1)
  n <- as.integer(n)
  if (n > length(set)) {
    cli::cli_abort("Only {length(set)} distinguishable linetypes are defined; {n} were requested.")
  }
  set[seq_len(n)]
}

# Build the palette.* theme elements from a palette name, so the theme carries
# the colour system rather than delegating it to the user's memory.
palette_elements <- function(tokens) {
  pal <- tokens$meta$palette
  entry <- vellum_palettes()
  kind <- entry$kind[match(vellum_key(pal), entry$palette)]
  kind <- if (is.na(kind)) "qualitative" else kind

  discrete_cols <- if (kind == "qualitative") {
    function(n) vellum_pal(pal, n = n)
  } else {
    # An ordered palette used for unordered groups would imply a ranking that is
    # not there, so fall back to Okabe-Ito for discrete aesthetics.
    function(n) okabe_ito(n)
  }
  continuous_cols <- if (kind == "qualitative") vellum_pal("abyss", 256) else vellum_pal(pal, 256)

  list(
    palette.colour.discrete = discrete_cols,
    palette.fill.discrete = discrete_cols,
    palette.colour.continuous = continuous_cols,
    palette.fill.continuous = continuous_cols,
    palette.shape.discrete = function(n) vellum_shapes(n),
    palette.linetype.discrete = function(n) vellum_linetypes(n)
  )
}

#' The vellum theme
#'
#' @description
#' A theme built for paper. Text sizes, rule weights, tick lengths, and mark
#' diameters are physical measurements taken from the target venue's published
#' specification, not screen-relative defaults, so a figure exported at 89 mm is
#' legible at 89 mm rather than legible when zoomed.
#'
#' What it removes and why: the grey panel, because it lowers contrast against
#' every mark drawn on it and costs ink; the minor grid, because it competes with
#' the data at the exact scale the data occupies; the strip background, because a
#' grey box round a facet label is decoration; the on-plot title, because in a
#' journal the title belongs in the caption where it can be typeset and indexed.
#'
#' What it adds: an L-shaped axis spine with outward ticks, which is the
#' convention across all three of Nature, Science, and Cell; audited discrete and
#' continuous palettes carried in the theme itself; and default geom ink and
#' weights, so data marks are correct without per-layer arguments.
#'
#' @param venue Target venue. See [venues()].
#' @param base_size Largest ordinary text size, in points. Defaults to the
#'   venue's permitted maximum.
#' @param family Typeface. Defaults to the venue's preferred installed family.
#' @param grid Which gridlines to draw. Either a word -- `"none"` (default), `"x"`,
#'   `"y"`, `"both"` -- or a flag string in which an uppercase letter turns on a major
#'   gridline and a lowercase one a minor: `"Y"` is major horizontal, `"XY"` both
#'   majors, `"Yy"` major and minor horizontal. A grid is justified when readers must
#'   read values off the axis, and not otherwise.
#' @param axis_style `"spine"` (default, L-shaped rules), `"frame"` (closed
#'   panel border, the Cell Press convention), `"range"` (no axis rules, for use
#'   with [geom_rangeframe()]), or `"none"`.
#' @param ticks `"out"` (default, the journal convention), `"in"`, or `"none"`.
#' @param legend Legend position: `"right"` (default), `"top"`, `"bottom"`,
#'   `"left"`, `"none"`, or `"inside"`.
#' @param palette Default palette name carried by the theme. See
#'   [vellum_palettes()].
#' @param density `"comfortable"`, `"compact"` for dense plates, `"airy"`.
#' @param tokens Optionally supply a [vellum_tokens()] object directly, which
#'   overrides `venue`, `base_size`, `family`, `palette`, and `density`.
#' @param ... Passed to [ggplot2::theme()], so any element can be overridden
#'   inline.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   theme_vellum()
#'
#' # No scale_colour_* call is needed: the theme carries the palette.
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   theme_vellum(venue = "cell", palette = "bright", axis_style = "frame")
#' @export
theme_vellum <- function(venue = "nature", base_size = NULL, family = NULL,
                         grid = "none",
                         axis_style = c("spine", "frame", "range", "none"),
                         ticks = c("out", "in", "none"),
                         legend = c("right", "top", "bottom", "left", "none", "inside"),
                         palette = "okabe_ito",
                         density = c("comfortable", "compact", "airy"),
                         tokens = NULL, ...) {
  g <- resolve_grid(grid)
  axis_style <- match.arg(axis_style)
  ticks <- match.arg(ticks)
  legend <- match.arg(legend)
  density <- match.arg(density)

  tk <- tokens %||% vellum_tokens(venue = venue, base_size = base_size, family = family,
                                  palette = palette, density = density)
  s <- tk$type$size

  txt <- function(size, colour = tk$ink$text, face = "plain", ...) {
    ggplot2::element_text(family = tk$type$family, size = size, colour = colour,
                          face = face, lineheight = tk$type$lineheight, ...)
  }
  line_grid <- ggplot2::element_line(colour = tk$ink$grid, linewidth = tk$rule$grid)
  line_grid_minor <- ggplot2::element_line(colour = tk$ink$grid_minor,
                                           linewidth = tk$rule$grid * 0.75)

  th <- ggplot2::theme(
    # -- roots --------------------------------------------------------------
    # A theme declaring `complete = TRUE` owes ggplot2 its root elements, or any
    # element inheriting from them has nothing to inherit. theme_grey() sets all
    # nine; leaving them out is how a "complete" theme ends up quietly incomplete.
    line = ggplot2::element_line(colour = tk$ink$axis, linewidth = tk$rule$axis,
                                 linetype = 1, lineend = "butt"),
    rect = ggplot2::element_rect(fill = tk$ink$paper, colour = tk$ink$axis,
                                 linewidth = tk$rule$border, linetype = 1),
    point = ggplot2::element_point(colour = tk$ink$data, shape = 16,
                                   size = tk$geom$point, fill = NA,
                                   stroke = tk$geom$stroke),
    polygon = ggplot2::element_polygon(fill = tk$ink$paper, colour = tk$ink$axis,
                                       linewidth = tk$rule$hairline, linetype = 1),
    spacing = tk$space$panel_spacing,
    margins = local({
      m <- as_mm(tk$space$plot_margin)
      ggplot2::margin(m, m, m, m, unit = "mm")
    }),

    # -- text ---------------------------------------------------------------
    text = txt(s$axis_text),
    title = txt(s$axis_title),
    plot.title = txt(
      s$title, face = "bold",
      margin = ggplot2::margin(b = as_mm(tk$space$tag_gap), unit = "mm")
    ),
    plot.subtitle = txt(
      s$axis_text, colour = tk$ink$weak,
      margin = ggplot2::margin(b = as_mm(tk$space$title_gap), unit = "mm")
    ),
    plot.caption = txt(s$small, colour = tk$ink$weak, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.tag = txt(s$tag, face = tk$meta$spec$tag_face),
    plot.tag.position = "topleft",
    plot.tag.location = "margin",

    axis.title.x = txt(s$axis_title, margin = ggplot2::margin(t = as_mm(tk$space$title_gap), unit = "mm")),
    axis.title.y = txt(s$axis_title, angle = 90,
                       margin = ggplot2::margin(r = as_mm(tk$space$title_gap), unit = "mm")),
    axis.text = txt(s$axis_text),
    axis.text.x = txt(s$axis_text, margin = ggplot2::margin(t = as_mm(tk$space$tag_gap), unit = "mm")),
    axis.text.y = txt(s$axis_text, hjust = 1,
                      margin = ggplot2::margin(r = as_mm(tk$space$tag_gap), unit = "mm")),
    axis.text.x.top = txt(s$axis_text,
                          margin = ggplot2::margin(b = as_mm(tk$space$tag_gap), unit = "mm")),
    axis.text.y.right = txt(s$axis_text, hjust = 0,
                            margin = ggplot2::margin(l = as_mm(tk$space$tag_gap), unit = "mm")),
    axis.title.x.top = txt(s$axis_title,
                           margin = ggplot2::margin(b = as_mm(tk$space$title_gap), unit = "mm")),
    axis.title.y.right = txt(s$axis_title, angle = -90,
                             margin = ggplot2::margin(l = as_mm(tk$space$title_gap), unit = "mm")),

    # -- panel --------------------------------------------------------------
    panel.background = ggplot2::element_rect(fill = tk$ink$panel, colour = NA),
    plot.background = ggplot2::element_rect(fill = tk$ink$paper, colour = NA),
    # The parent must NOT be element_blank(). A theme declaring `complete = TRUE` has
    # `inherit.blank = TRUE` forced onto every element, so a blank `panel.grid` blanks
    # all four children however they are set here. Give the parent the line style and
    # switch the individual gridlines off instead.
    panel.grid = line_grid,
    panel.grid.major = line_grid,
    panel.grid.minor = line_grid_minor,
    panel.grid.major.x = if (g$major_x) line_grid else ggplot2::element_blank(),
    panel.grid.major.y = if (g$major_y) line_grid else ggplot2::element_blank(),
    panel.grid.minor.x = if (g$minor_x) line_grid_minor else ggplot2::element_blank(),
    panel.grid.minor.y = if (g$minor_y) line_grid_minor else ggplot2::element_blank(),
    panel.spacing = tk$space$panel_spacing,
    # Gridlines belong under the data. Drawing them over it is a way of saying the
    # furniture matters more than the observations.
    panel.ontop = FALSE,
    panel.border = if (axis_style == "frame") {
      ggplot2::element_rect(fill = NA, colour = tk$ink$axis, linewidth = tk$rule$border)
    } else ggplot2::element_blank(),

    # -- axes ---------------------------------------------------------------
    axis.line = if (axis_style == "spine") {
      ggplot2::element_line(colour = tk$ink$axis, linewidth = tk$rule$axis, lineend = "square")
    } else ggplot2::element_blank(),
    axis.ticks = if (ticks == "none") ggplot2::element_blank() else {
      ggplot2::element_line(colour = tk$ink$tick, linewidth = tk$rule$tick)
    },
    axis.ticks.length = if (ticks == "in") -tk$space$tick_len else tk$space$tick_len,
    # Minor ticks are off by default -- they compete with the data at the scale the
    # data occupies -- but their geometry is still specified, so a user who turns
    # them on gets a shorter tick rather than one the same length as the major.
    axis.minor.ticks.length = ggplot2::rel(0.55),
    axis.minor.ticks.x.bottom = ggplot2::element_blank(),
    axis.minor.ticks.y.left = ggplot2::element_blank(),

    # -- strips -------------------------------------------------------------
    strip.background = ggplot2::element_blank(),
    strip.text = txt(s$strip, face = "bold"),
    strip.text.x = txt(s$strip, face = "bold", hjust = 0,
                       margin = ggplot2::margin(b = as_mm(tk$space$tag_gap), unit = "mm")),
    strip.text.y = txt(s$strip, face = "bold", angle = -90,
                       margin = ggplot2::margin(l = as_mm(tk$space$tag_gap), unit = "mm")),
    strip.placement = "outside",
    # Strip text that has been wrapped or rotated must not be cropped by its own cell.
    strip.clip = "off",
    strip.switch.pad.grid = tk$space$tag_gap,
    strip.switch.pad.wrap = tk$space$tag_gap,

    # -- legend -------------------------------------------------------------
    legend.position = if (legend == "inside") "inside" else legend,
    legend.justification = if (legend %in% c("right", "left")) c(0, 1) else "center",
    legend.direction = if (legend %in% c("top", "bottom")) "horizontal" else "vertical",
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.key.size = tk$space$legend_key,
    legend.key.spacing = tk$space$legend_gap,
    legend.title = txt(s$legend_title),
    legend.text = txt(s$legend_text),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = tk$space$legend_gap,
    legend.byrow = FALSE,
    legend.location = "panel",
    legend.box.just = "left",
    legend.key.justification = "left",
    legend.ticks.length = ggplot2::rel(0.4),
    legend.ticks = ggplot2::element_line(colour = tk$ink$paper, linewidth = tk$rule$hairline),
    legend.frame = ggplot2::element_rect(fill = NA, colour = tk$ink$axis,
                                         linewidth = tk$rule$hairline),

    # -- figure -------------------------------------------------------------
    plot.margin = local({
      m <- as_mm(tk$space$plot_margin)
      ggplot2::margin(m, m, m, m, unit = "mm")
    }),

    # -- data defaults ------------------------------------------------------
    # Marks inherit the token ink and weights, so a bare geom_point() is already
    # print-correct.
    geom = ggplot2::element_geom(
      ink = tk$ink$data, paper = tk$ink$paper, accent = tk$ink$accent,
      linewidth = tk$rule$geom, borderwidth = tk$rule$hairline,
      family = tk$type$family, fontsize = s$annot, pointsize = tk$geom$point,
      pointshape = 16
    ),
    complete = TRUE
  )

  th <- th + do.call(ggplot2::theme, palette_elements(tk))
  extra <- list(...)
  if (length(extra)) th <- th + do.call(ggplot2::theme, extra)
  attr(th, "vellum_tokens") <- tk
  th
}

#' Variants of the vellum theme
#'
#' @description
#' All are [theme_vellum()] with different arguments; none introduce new visual
#' constants. Use them as shorthand.
#'
#' * `theme_vellum_grid()` adds a horizontal grid, for when readers must read
#'   values off the y axis.
#' * `theme_vellum_frame()` closes the panel with a border, the Cell Press
#'   convention and the right choice for heatmaps and image panels where the data
#'   extends to the panel edge.
#' * `theme_vellum_prism()` is the GraphPad Prism idiom common in bench biology:
#'   heavier spine, inward ticks, no grid. Pair it with exact P values from
#'   [annotate_signif()], never a cloud of asterisks.
#' * `theme_vellum_void()` strips everything but the marks, for schematics, maps,
#'   and image plates.
#' * `theme_vellum_dark()` inverts the ink for slides and dark-background talks.
#' * `theme_vellum_slide()` and `theme_vellum_poster()` switch venue, so type and
#'   marks grow together instead of a print figure being enlarged.
#'
#' @inheritParams theme_vellum
#' @param ... Passed to [theme_vellum()].
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot() + theme_vellum_prism()
theme_vellum_grid <- function(...) theme_vellum(grid = "y", ...)

#' @rdname theme_vellum_grid
#' @export
theme_vellum_frame <- function(...) theme_vellum(axis_style = "frame", ...)

#' @rdname theme_vellum_grid
#' @export
theme_vellum_prism <- function(venue = "nature", ...) {
  tk <- vellum_tokens(venue = venue, ...)
  theme_vellum(tokens = tk, grid = "none", axis_style = "spine", ticks = "in",
               axis.line = ggplot2::element_line(colour = tk$ink$axis,
                                                 linewidth = tk$rule$emphasis,
                                                 lineend = "square"),
               axis.ticks = ggplot2::element_line(colour = tk$ink$tick,
                                                  linewidth = tk$rule$emphasis))
}

#' @rdname theme_vellum_grid
#' @export
theme_vellum_void <- function(...) {
  theme_vellum(grid = "none", axis_style = "none", ticks = "none", ...) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks.length = grid::unit(0, "mm"))
}

#' @rdname theme_vellum_grid
#' @export
theme_vellum_dark <- function(venue = "slide", ...) {
  theme_vellum(tokens = vellum_tokens(venue = venue, dark = TRUE, ...), grid = "y")
}

#' @rdname theme_vellum_grid
#' @export
theme_vellum_slide <- function(...) theme_vellum(venue = "slide", grid = "y", ...)

#' @rdname theme_vellum_grid
#' @export
theme_vellum_poster <- function(...) theme_vellum(venue = "poster", grid = "y", ...)

#' A theme matched to a venue's specification
#'
#' Shorthand for [theme_vellum()] with the venue's own type sizes, typeface, and
#' panel-label convention. Also applies the venue's house axis idiom: a closed
#' frame for Cell Press, an L-shaped spine elsewhere.
#'
#' @inheritParams theme_vellum
#' @param ... Passed to [theme_vellum()].
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("cell")
theme_venue <- function(venue = "nature", ...) {
  spec <- venue_spec(venue)
  warn_unverified(spec)
  dots <- list(...)
  if (is.null(dots$axis_style)) {
    dots$axis_style <- if (spec$name == "cell") "frame" else "spine"
  }
  do.call(theme_vellum, c(list(venue = venue), dots))
}

#' Set a vellum theme for the whole session
#'
#' @description
#' Applies the theme as ggplot2's default so every subsequent plot inherits it.
#' Use this at the top of an analysis script: the figures in a paper should be
#' consistent by construction, not by remembering to add a theme to each one.
#'
#' @inheritParams theme_vellum
#' @param ... Passed to [theme_vellum()].
#' @return Invisibly, the previous theme, so it can be restored.
#' @export
#' @examples
#' old <- vellum_set("nature")
#' ggplot2::theme_set(old)
vellum_set <- function(venue = "nature", ...) {
  old <- ggplot2::theme_get()
  ggplot2::theme_set(theme_venue(venue, ...))
  invisible(old)
}

#' Report which theme elements a theme actually sets
#'
#' @description
#' A theme that leaves an element unset is not neutral: the element inherits, and what
#' it inherits from may contradict the venue. This reports every element of the
#' installed ggplot2, whether the supplied theme sets it, and what it resolves to, so
#' coverage is a measurement rather than a claim.
#'
#' Total coverage is not the goal, and chasing it would be a mistake. Roughly a
#' quarter of ggplot2's elements are positional or coordinate-specific variants --
#' `axis.text.y.right`, `axis.ticks.theta`, `panel.grid.minor.y` -- that exist so a
#' theme can stay silent and let the general case apply. Freezing them all would make
#' the theme brittle without making any figure better. What matters is that nothing
#' *material* inherits a value the venue forbids, which is what [venue_check()] tests.
#'
#' @param theme A ggplot2 theme. Defaults to [theme_vellum()].
#' @param set_only Return only the elements the theme sets.
#'
#' @return A data frame with one row per element: its name, whether the theme sets it,
#'   whether it resolves to blank, and its class.
#' @export
#' @examples
#' cov <- theme_coverage()
#' table(cov$set)
#' head(cov[cov$set, ])
theme_coverage <- function(theme = NULL, set_only = FALSE) {
  theme <- theme %||% theme_vellum()
  tree <- names(ggplot2::get_element_tree())
  set <- vapply(tree, function(nm) {
    !is.null(theme[[nm]])
  }, logical(1))
  resolved <- vapply(tree, function(nm) {
    el <- tryCatch(ggplot2::calc_element(nm, theme), error = function(e) NULL)
    if (is.null(el)) "unresolved" else class(el)[1]
  }, character(1))
  out <- data.frame(element = tree, set = unname(set), resolved = unname(resolved),
                    stringsAsFactors = FALSE, row.names = NULL)
  out$family <- sub("[.].*$", "", out$element)
  if (set_only) out <- out[out$set, , drop = FALSE]
  row.names(out) <- NULL
  out
}

# Resolve the grid specification. Accepts the legacy words and the compact flag string
# in which case matters: "X"/"Y" are major gridlines, "x"/"y" minor.
resolve_grid <- function(grid) {
  if (is.logical(grid)) grid <- if (isTRUE(grid)) "XY" else ""
  check_string(grid, "grid")
  if (grid %in% c("none", "x", "y", "both")) {
    grid <- switch(grid, none = "", x = "X", y = "Y", both = "XY")
  }
  parts <- strsplit(grid, "")[[1]]
  bad <- setdiff(parts, c("X", "Y", "x", "y"))
  if (length(bad)) {
    cli::cli_abort(c(
      "{.arg grid} contains {.val {bad}}, which {?is/are} not {?a/} grid flag{?s}.",
      i = 'Use "X"/"Y" for major gridlines and "x"/"y" for minor, or one of "none", "x", "y", "both".'
    ))
  }
  list(major_x = "X" %in% parts, major_y = "Y" %in% parts,
       minor_x = "x" %in% parts, minor_y = "y" %in% parts)
}

#' A theme for maps and spatial panels
#'
#' @description
#' Spatial figures need different furniture from statistical ones. Latitude and
#' longitude tick labels are almost never the point, so the axes go; a graticule is
#' useful but must sit well behind the geography, so it is drawn at minor weight; and
#' the panel takes a faint tint so that white polygons and the page are
#' distinguishable, which matters the moment a country has missing data.
#'
#' Pair it with [ggplot2::coord_sf()] and remember that a map with no axes still owes
#' the reader a scale bar and a north arrow where direction is not obvious.
#'
#' @inheritParams theme_vellum
#' @param graticule Draw the coordinate graticule.
#' @param dark Invert the surface for a talk on a dark background. The panel tint, the
#'   graticule, the frame, and the missing-data colour all move together; setting only
#'   the background is how a dark map ends up with an invisible graticule.
#' @param tint Panel fill, which distinguishes sea from page so that a polygon with no
#'   data is not mistaken for absent geography. `NULL` picks a tint to suit `dark`; set
#'   `"transparent"` for a figure placed over a coloured ground.
#' @param ... Passed to [theme_vellum()].
#' @return A ggplot2 theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point() + theme_vellum_map()
theme_vellum_map <- function(venue = "nature", graticule = TRUE, dark = FALSE,
                            tint = NULL, ...) {
  check_bool(dark, "dark")
  tk <- vellum_tokens(venue = venue, dark = dark)
  # A hair lighter than the paper on a dark ground, a hair darker on a light one: enough
  # to read as sea without competing with the data.
  tint <- tint %||% (if (dark) "#191920" else "#FBFCFE")
  theme_vellum(tokens = tk, grid = if (graticule) "XY" else "none",
               axis_style = "none", ticks = "none", ...) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "mm"),
      panel.background = ggplot2::element_rect(fill = tint, colour = NA),
      panel.grid.major = if (graticule) {
        ggplot2::element_line(colour = tk$ink$grid_minor, linewidth = tk$rule$hairline)
      } else ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = tk$ink$axis,
                                           linewidth = tk$rule$hairline)
    )
}
