# Multivariate and dense-series recipes: many variables, or many points, in one
# panel. Each of these trades an axis for a channel the reader has to learn, so
# each one states what it gave up.

# Normalise a set of numeric columns to a common 0-1 range, keeping the original
# extremes so the axes can be labelled with real numbers rather than with the
# fiction that every variable runs from 0 to 1.
axis_normalise <- function(m, scaling, call = rlang::caller_env()) {
  ranges <- lapply(seq_len(ncol(m)), function(j) range(m[, j], na.rm = TRUE))
  scaled <- switch(
    scaling,
    none = m,
    range = {
      out <- m
      for (j in seq_len(ncol(m))) {
        r <- ranges[[j]]
        out[, j] <- if (diff(r) == 0) 0.5 else (m[, j] - r[1]) / diff(r)
      }
      out
    },
    zscore = {
      out <- m
      for (j in seq_len(ncol(m))) {
        s <- stats::sd(m[, j], na.rm = TRUE)
        out[, j] <- if (is.na(s) || s == 0) 0 else
          (m[, j] - mean(m[, j], na.rm = TRUE)) / s
      }
      out
    }
  )
  list(values = scaled, ranges = ranges)
}

# Pull the numeric matrix for a set of named columns, refusing anything that is
# not a number rather than coercing it to NA and drawing a gap.
numeric_columns <- function(data, columns, call = rlang::caller_env()) {
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.", call = call)
  }
  not_numeric <- columns[!vapply(data[columns], is.numeric, logical(1))]
  if (length(not_numeric)) {
    cli::cli_abort(c(
      "Column{?s} {.val {not_numeric}} {?is/are} not numeric.",
      i = "Axes are positions on a scale; a category has no position to place."
    ), call = call)
  }
  as.matrix(data[columns])
}

#' Radar chart
#'
#' @description
#' A small profile across a few commensurate dimensions, closed into a polygon so
#' that the shape as a whole can be compared between groups.
#'
#' Read the constraints before reaching for it. The area enclosed grows with the
#' square of the values, so a group twice as good on every axis looks four times
#' as good. The shape depends entirely on the order of the axes, which is
#' usually arbitrary, so two orderings of the same data give two different
#' impressions. And the dimensions have to be commensurate: normalising
#' unrelated units to a shared 0-1 range makes them look comparable without
#' making them so.
#'
#' Where those bite, [fig_dotstrip()] or a small-multiple of [fig_lollipop()]
#' puts the same comparison on an aligned scale. A radar earns its place when the
#' *profile* is the finding -- balanced against spiky -- rather than any one
#' value.
#'
#' @param data A data frame in wide form, one row per group.
#' @param group Column naming each row's group.
#' @param axes Numeric columns to use as axes. Defaults to every numeric column
#'   other than `group`.
#' @param scaling `"range"` normalises each axis to its own observed range,
#'   `"none"` keeps the raw values on a shared scale, which is the honest choice
#'   when the axes really are commensurate. Range scaling with two or three
#'   groups is degenerate -- with two, every axis becomes exactly 0 and 1 and the
#'   figure shows the ordering rather than the profile.
#' @param fill_alpha Polygon fill opacity. Zero draws outlines only, which stays
#'   readable with more groups.
#' @param rings Number of grid rings.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   arm = c("Control", "Intervention"),
#'   adherence = c(0.62, 0.81), retention = c(0.70, 0.88),
#'   satisfaction = c(0.55, 0.79), cost = c(0.80, 0.44),
#'   coverage = c(0.48, 0.72)
#' )
#' fig_radar(d, group = "arm", scaling = "none")
fig_radar <- function(data, group, axes = NULL, scaling = c("range", "none"),
                      fill_alpha = 0.15, rings = 4,
                      venue = "nature", palette = "okabe_ito") {
  scaling <- match.arg(scaling)
  check_number(fill_alpha, "fill_alpha", min = 0, max = 1)
  check_number(rings, "rings", min = 1, max = 10)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in wide form.")
  if (!group %in% names(data)) cli::cli_abort("{.arg data} has no column {.val {group}}.")

  axes <- axes %||% setdiff(names(data)[vapply(data, is.numeric, logical(1))], group)
  if (length(axes) < 3L) {
    cli::cli_abort(c(
      "A radar chart needs at least three axes; {length(axes)} {?was/were} given.",
      i = "With two dimensions the polygon collapses to a line, and a scatter plot shows it better."
    ))
  }
  m <- numeric_columns(data, axes)
  norm <- axis_normalise(m, scaling)
  vals <- norm$values
  vals[vals < 0] <- 0

  g <- as.character(data[[group]])
  n_axis <- length(axes)
  theta <- seq(0, 2 * pi, length.out = n_axis + 1L)[seq_len(n_axis)]

  # The polygon is closed by repeating the first vertex. Without it ggplot2 will
  # close it anyway, but the closing segment is then drawn without the line
  # properties of the rest and the shape has a visible seam.
  poly <- do.call(rbind, lapply(seq_along(g), function(i) {
    r <- c(vals[i, ], vals[i, 1])
    a <- c(theta, theta[1])
    data.frame(x = r * sin(a), y = r * cos(a), group = g[i], stringsAsFactors = FALSE)
  }))
  poly$group <- factor(poly$group, levels = unique(g))

  tk <- vellum_tokens(venue)
  ring_r <- seq_len(rings) / rings
  ring_df <- do.call(rbind, lapply(ring_r, function(r) {
    a <- seq(0, 2 * pi, length.out = 121)
    data.frame(x = r * sin(a), y = r * cos(a), r = r)
  }))
  spokes <- data.frame(x = 1.0 * sin(theta), y = 1.0 * cos(theta))
  labels <- data.frame(
    x = 1.13 * sin(theta), y = 1.13 * cos(theta), label = axes,
    hjust = 0.5 - 0.5 * sin(theta), stringsAsFactors = FALSE
  )

  ggplot2::ggplot() +
    ggplot2::geom_path(data = ring_df,
                       ggplot2::aes(.data$x, .data$y, group = .data$r),
                       colour = tk$ink$grid, linewidth = tk$rule$grid) +
    ggplot2::geom_segment(data = spokes,
                          ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                          colour = tk$ink$grid, linewidth = tk$rule$grid) +
    ggplot2::geom_polygon(
      data = poly,
      ggplot2::aes(.data$x, .data$y, group = .data$group,
                   colour = .data$group, fill = .data$group),
      alpha = fill_alpha, linewidth = tk$rule$geom) +
    ggplot2::geom_text(data = labels,
                       ggplot2::aes(.data$x, .data$y, label = .data$label,
                                    hjust = .data$hjust),
                       size = fs(tk$type$size$small), colour = tk$ink$text,
                       family = tk$type$family) +
    scale_colour_vellum(palette) +
    scale_fill_vellum(palette) +
    # Wide enough for the axis names, which sit outside the outer ring and are
    # the one part of a radar chart that cannot be inferred from the shape.
    ggplot2::coord_equal(xlim = c(-1.62, 1.62), ylim = c(-1.3, 1.3), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "bottom")
}

#' Parallel coordinates plot
#'
#' @description
#' One line per observation across several vertical axes, for seeing which
#' variables move together and which observations follow an unusual path.
#'
#' Two properties decide whether it works. Only *adjacent* axes can be compared,
#' so the ordering of the columns is part of the analysis and not a detail;
#' reorder them deliberately. And with more than a few hundred lines the panel
#' saturates, at which point lowering `alpha` stops helping and the figure is
#' showing ink rather than data.
#'
#' Each axis is labelled with its own minimum and maximum, because a normalised
#' axis without them tells the reader the shape and hides the magnitude.
#'
#' @param data A data frame.
#' @param columns Numeric columns to use as axes, in the order they should appear.
#' @param group Optional column mapped to line colour.
#' @param scaling `"range"`, `"zscore"`, or `"none"`.
#' @param alpha Line opacity.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' fig_parallel(iris, columns = names(iris)[1:4], group = "Species")
fig_parallel <- function(data, columns, group = NULL,
                         scaling = c("range", "zscore", "none"), alpha = 0.6,
                         venue = "nature", palette = "okabe_ito") {
  scaling <- match.arg(scaling)
  check_number(alpha, "alpha", min = 0, max = 1)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  if (length(columns) < 2L) cli::cli_abort("A parallel-coordinates plot needs at least two axes.")

  m <- numeric_columns(data, columns)
  norm <- axis_normalise(m, scaling)
  vals <- norm$values

  long <- data.frame(
    id = rep(seq_len(nrow(vals)), times = ncol(vals)),
    axis = factor(rep(columns, each = nrow(vals)), levels = columns),
    value = as.numeric(vals),
    stringsAsFactors = FALSE
  )
  if (!is.null(group)) {
    if (!group %in% names(data)) cli::cli_abort("{.arg data} has no column {.val {group}}.")
    long$group <- rep(as.character(data[[group]]), times = ncol(vals))
  }

  tk <- vellum_tokens(venue)
  ends <- do.call(rbind, lapply(seq_along(columns), function(j) {
    r <- norm$ranges[[j]]
    at <- if (scaling == "none") r else range(vals[, j], na.rm = TRUE)
    data.frame(axis = columns[j], y = at, label = format(r, digits = 3, trim = TRUE),
               vjust = c(1.4, -0.5), stringsAsFactors = FALSE)
  }))
  ends$axis <- factor(ends$axis, levels = columns)

  lines <- if (is.null(group)) {
    ggplot2::geom_line(alpha = alpha, linewidth = tk$rule$geom, colour = tk$ink$data)
  } else {
    ggplot2::geom_line(ggplot2::aes(colour = .data$group), alpha = alpha,
                       linewidth = tk$rule$geom)
  }

  p <- ggplot2::ggplot(long, ggplot2::aes(.data$axis, .data$value, group = .data$id)) +
    lines +
    ggplot2::geom_text(
      data = ends, inherit.aes = FALSE,
      ggplot2::aes(.data$axis, .data$y, label = .data$label, vjust = .data$vjust),
      size = fs(tk$type$size$small), colour = tk$ink$weak, family = tk$type$family) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    theme_vellum(venue = venue, grid = "none", axis_style = "none", ticks = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  if (!is.null(group)) p <- p + scale_colour_vellum(palette)
  p
}

#' Ridgeline plot
#'
#' @description
#' Many distributions stacked and overlapped, for comparing shape across a large
#' number of groups without giving each one a panel. It shows what a box plot
#' throws away -- bimodality, skew, a shoulder -- while staying legible at twenty
#' groups where twenty violins would not be.
#'
#' The overlap is a real cost and is under your control through `scale`. Above
#' about 1 the ridges hide each other's tails, which is usually acceptable, and
#' well above it the figure becomes decorative. The ordering is the other lever:
#' ridgelines only reveal a pattern when the groups have an order, so give them
#' one, whether it is time, dose, or the statistic being compared.
#'
#' Groups with too few observations for a density estimate are refused rather
#' than drawn, because a kernel density through three points is a picture of the
#' bandwidth and not of the data.
#'
#' @param data A data frame.
#' @param x,group Column names giving the values and the grouping.
#' @param scale Ridge height as a multiple of the row spacing. Above 1 the ridges
#'   overlap.
#' @param bandwidth Kernel bandwidth. Defaults to `stats::bw.nrd0()` computed on
#'   the pooled data, so every group is smoothed identically and differences in
#'   shape are differences in the data.
#' @param min_n Smallest group that will be drawn.
#' @param venue Target venue.
#' @param palette Palette name. A sequential palette reads best, since the groups
#'   are ordered.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   value = c(rnorm(200, 5), rnorm(200, 6, 1.4), rnorm(200, 8, 0.8),
#'             rnorm(200, 7.5, 2)),
#'   dose = rep(c("0 mg", "10 mg", "20 mg", "40 mg"), each = 200)
#' )
#' fig_ridgeline(d, x = "value", group = "dose")
fig_ridgeline <- function(data, x, group, scale = 1.6, bandwidth = NULL,
                          min_n = 10, venue = "nature", palette = "abyss") {
  check_number(scale, "scale", min = 0.1)
  check_number(min_n, "min_n", min = 2)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(x, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  v <- as.numeric(data[[x]])
  check_numeric(v, x)
  g <- data[[group]]
  keep <- is.finite(v)
  v <- v[keep]; g <- g[keep]
  gf <- if (is.factor(g)) droplevels(g) else factor(as.character(g), levels = unique(as.character(g)))

  counts <- table(gf)
  thin <- names(counts)[counts < min_n]
  if (length(thin)) {
    cli::cli_abort(c(
      "{length(thin)} group{?s} {?has/have} fewer than {min_n} observations: {.val {thin}}.",
      i = "A kernel density through a handful of points describes the bandwidth, not the distribution.",
      i = "Lower {.arg min_n} to draw them anyway, or show the observations with {.fn fig_dotstrip}."
    ))
  }

  # One bandwidth for every group, computed on the pooled data. Letting each
  # group choose its own makes a narrow group look smooth and a wide one look
  # spiky for reasons that have nothing to do with the distributions.
  bw <- bandwidth %||% stats::bw.nrd0(v)
  lv <- levels(gf)
  dens <- do.call(rbind, lapply(seq_along(lv), function(i) {
    z <- stats::density(v[gf == lv[i]], bw = bw, from = min(v), to = max(v), n = 512)
    data.frame(x = z$x, height = z$y / max(z$y), row = i, group = lv[i],
               stringsAsFactors = FALSE)
  }))
  # Rows run downwards so that the first level is at the top, which is how an
  # ordered sequence reads, and the ridges are drawn back to front so a nearer
  # one overlaps the one behind it rather than the reverse.
  dens$base <- -dens$row
  dens$top <- dens$base + dens$height * scale
  dens$group <- factor(dens$group, levels = lv)

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(dens[order(-dens$row), , drop = FALSE]) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = .data$x, ymin = .data$base, ymax = .data$top,
                   group = .data$row, fill = .data$group),
      colour = tk$ink$text, linewidth = tk$rule$hairline * 2) +
    scale_fill_vellum(palette) +
    ggplot2::scale_y_continuous(breaks = -seq_along(lv), labels = lv,
                                expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(x = x, y = NULL, fill = NULL) +
    theme_vellum(venue = venue, grid = "none") +
    ggplot2::theme(legend.position = "none")
}

#' Horizon graph
#'
#' @description
#' A dense time series folded into bands, so that many series can be stacked in
#' the height one line chart would need. Values above the origin are cut into
#' `bands` layers of equal height, each drawn on the same baseline in a stronger
#' colour than the last; values below it are mirrored and drawn in the opposing
#' hue. Magnitude is read from colour, direction from hue.
#'
#' It buys density with a channel the reader has to learn, so it belongs where
#' the comparison is *between* many series rather than the precise value of any
#' one. Below about five series a set of small-multiple line charts is easier and
#' costs nothing.
#'
#' The origin is the whole argument. A horizon graph around zero shows sign; one
#' around a baseline period shows departure from it; one around the mean shows
#' nothing in particular. Set it deliberately.
#'
#' @param data A data frame in long form.
#' @param x,y Column names giving time and value.
#' @param group Optional column giving the series; each becomes one row.
#' @param bands Number of bands per direction. Three is conventional; more asks
#'   the reader to distinguish more steps of one hue than they reliably can.
#' @param origin The value the bands fold around.
#' @param band_height Height of one band in data units. Defaults to the largest
#'   departure from `origin` divided by `bands`, which uses the full colour range.
#' @param venue Target venue.
#' @param palette A diverging palette; its two arms supply the two directions.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(2)
#' d <- expand.grid(t = 1:120, series = paste("Site", 1:6))
#' d$value <- as.numeric(stats::filter(rnorm(nrow(d)), 0.85, method = "recursive"))
#' fig_horizon(d, x = "t", y = "value", group = "series")
fig_horizon <- function(data, x, y, group = NULL, bands = 3, origin = 0,
                        band_height = NULL, venue = "nature", palette = "strata") {
  check_number(bands, "bands", min = 1, max = 6)
  check_number(origin, "origin")
  bands <- as.integer(bands)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(x, y, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(x = data[[x]], y = as.numeric(data[[y]]), stringsAsFactors = FALSE)
  check_numeric(d$y, y)
  d$series <- if (is.null(group)) "" else as.character(data[[group]])
  d <- d[is.finite(d$y), , drop = FALSE]

  step <- band_height %||% (max(abs(d$y - origin), na.rm = TRUE) / bands)
  if (!is.finite(step) || step <= 0) {
    cli::cli_abort("Every value equals {.arg origin}, so there is nothing to band.")
  }

  # Both directions are drawn on the same positive baseline; that folding is what
  # a horizon graph is. Layer b holds the part of the departure between
  # (b-1) and b band heights.
  layers <- do.call(rbind, lapply(seq_len(bands), function(b) {
    up <- clamp(d$y - origin - (b - 1) * step, 0, step)
    dn <- clamp(origin - d$y - (b - 1) * step, 0, step)
    rbind(
      data.frame(d[c("x", "series")], height = up, band = b, dir = "above"),
      data.frame(d[c("x", "series")], height = dn, band = b, dir = "below")
    )
  }))
  layers <- layers[layers$height > 0 | layers$band == 1L, , drop = FALSE]
  layers$key <- paste(layers$dir, layers$band)

  # A diverging palette's two arms, sampled away from the pale hinge so that even
  # band 1 is visible against the page.
  ramp <- vellum_pal(palette, n = 2 * bands + 3)
  above <- rev(ramp[seq_len(bands)])
  below <- ramp[seq(length(ramp), length(ramp) - bands + 1)]
  fills <- stats::setNames(c(rev(above), rev(below)),
                           c(paste("above", seq_len(bands)), paste("below", seq_len(bands))))

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(layers[order(layers$band), , drop = FALSE]) +
    ggplot2::geom_area(
      ggplot2::aes(.data$x, .data$height, group = .data$key, fill = .data$key),
      position = "identity") +
    ggplot2::scale_fill_manual(values = fills, guide = "none") +
    ggplot2::scale_y_continuous(breaks = c(0, step), labels = c("", format(step, digits = 2)),
                                expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = sprintf("Each band is %s; %d bands per direction, folded about %s.",
                                    format(step, digits = 3), bands, format(origin))) +
    theme_vellum(venue = venue, grid = "none", ticks = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = fs(tk$type$size$small)))

  if (!is.null(group)) {
    p <- p + ggplot2::facet_wrap(~ series, ncol = 1, strip.position = "left") +
      ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
                     panel.spacing.y = ggplot2::unit(0.4, "mm"))
  }
  p
}

#' Manhattan plot
#'
#' @description
#' Association across genomic position, with chromosomes laid end to end and
#' evidence on a negative log scale. The alternating colour is a reading aid for
#' chromosome boundaries and carries no other meaning, which is why it uses two
#' colours and not twenty-two.
#'
#' The genome-wide line is drawn at 5e-8 by default, which is the Bonferroni
#' correction for roughly a million independent common variants in European
#' populations. It is a convention, not a property of your study: a different
#' array, a different population, or a different number of tests moves it. Set it
#' rather than inheriting it.
#'
#' @param data A data frame, one row per variant.
#' @param chrom,pos,p Column names giving chromosome, base-pair position, and the
#'   association P value.
#' @param threshold Genome-wide significance threshold.
#' @param suggestive Suggestive threshold, or `NULL` for none.
#' @param highlight Optional column of logical or character values; rows that are
#'   `TRUE` or non-empty are drawn in the accent colour on top.
#' @param label Optional column of variant labels, drawn for points above
#'   `threshold`.
#' @param venue Target venue.
#' @param palette Palette name; its first two colours alternate by chromosome.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(3)
#' d <- data.frame(
#'   chrom = rep(1:6, each = 400),
#'   pos = unlist(lapply(1:6, function(i) sort(sample.int(2.4e8, 400)))),
#'   p = stats::runif(2400)^3
#' )
#' d$p[500] <- 1e-12
#' fig_manhattan(d)
fig_manhattan <- function(data, chrom = "chrom", pos = "pos", p = "p",
                          threshold = 5e-8, suggestive = 1e-5,
                          highlight = NULL, label = NULL,
                          venue = "nature", palette = "okabe_ito") {
  check_number(threshold, "threshold", min = 0, max = 1)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(chrom, pos, p, highlight, label), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  pv <- as.numeric(data[[p]])
  check_numeric(pv, p)
  if (any(pv <= 0 | pv > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{sum(pv <= 0 | pv > 1, na.rm = TRUE)} P value{?s} {?is/are} outside (0, 1].",
      i = "A P value of exactly zero is an underflow, not a result; floor it at the smallest value your test can resolve and say so."
    ))
  }

  # Chromosomes in genomic order, not alphabetical: 10 follows 9, and X, Y and MT
  # come last. Sorting the labels as text puts chromosome 10 next to chromosome 1.
  raw <- as.character(data[[chrom]])
  clean <- toupper(sub("^chr", "", raw, ignore.case = TRUE))
  numeric_part <- suppressWarnings(as.numeric(clean))
  rank <- ifelse(!is.na(numeric_part), numeric_part,
                 match(clean, c("X", "Y", "MT", "M")) + 1000)
  if (anyNA(rank)) {
    cli::cli_abort("Unrecognised chromosome{?s}: {.val {unique(raw[is.na(rank)])}}.")
  }
  ord <- order(rank, as.numeric(data[[pos]]))

  d <- data.frame(
    chrom = factor(clean[ord], levels = unique(clean[order(rank)])),
    pos = as.numeric(data[[pos]])[ord],
    logp = -log10(pv[ord]),
    stringsAsFactors = FALSE
  )
  if (!is.null(highlight)) {
    h <- data[[highlight]][ord]
    d$highlight <- if (is.logical(h)) h else !is.na(h) & nzchar(as.character(h))
  } else {
    d$highlight <- FALSE
  }
  if (!is.null(label)) d$label <- as.character(data[[label]])[ord]

  # Chromosomes laid end to end, with each one's own span preserved so that
  # position within a chromosome still means something.
  spans <- vapply(split(d$pos, d$chrom), function(z) max(z) - min(z), numeric(1))
  mins <- vapply(split(d$pos, d$chrom), min, numeric(1))
  offset <- cumsum(c(0, utils::head(spans, -1)))
  names(offset) <- names(spans)
  d$gpos <- d$pos - mins[as.character(d$chrom)] + offset[as.character(d$chrom)]
  centres <- offset + spans / 2

  cols <- vellum_pal(palette, 2)
  d$band <- factor(as.integer(d$chrom) %% 2L)
  tk <- vellum_tokens(venue)

  p_obj <- ggplot2::ggplot(d, ggplot2::aes(.data$gpos, .data$logp)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$band),
                        size = tk$geom$point_small, stroke = 0) +
    ggplot2::scale_colour_manual(values = stats::setNames(cols, c("0", "1")), guide = "none") +
    ggplot2::geom_hline(yintercept = -log10(threshold), colour = tk$ink$text,
                        linewidth = tk$rule$geom, linetype = "22") +
    ggplot2::scale_x_continuous(breaks = centres, labels = names(centres),
                                expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(x = "Chromosome",
                  y = expression(-log[10](italic(P)))) +
    theme_vellum(venue = venue, grid = "none")

  if (!is.null(suggestive)) {
    p_obj <- p_obj + ggplot2::geom_hline(
      yintercept = -log10(suggestive), colour = tk$ink$weak,
      linewidth = tk$rule$grid, linetype = "12")
  }
  if (any(d$highlight)) {
    p_obj <- p_obj + ggplot2::geom_point(
      data = d[d$highlight, , drop = FALSE], colour = tk$ink$accent,
      size = tk$geom$point, stroke = 0)
  }
  if (!is.null(label)) {
    top <- d[d$logp >= -log10(threshold) & nzchar(d$label), , drop = FALSE]
    if (nrow(top)) {
      p_obj <- p_obj + ggplot2::geom_text(
        data = top, ggplot2::aes(label = .data$label), vjust = -0.6,
        size = fs(tk$type$size$small), colour = tk$ink$text, family = tk$type$family)
    }
  }
  p_obj
}
