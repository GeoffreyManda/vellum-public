# A third axis.
#
# ggplot2 has no z aesthetic and no 3D coordinate system, and adding one properly is
# out of scope for any theme package. What is in scope is the thing a print figure
# actually needs: a static, vector, themed orthographic projection of a surface, drawn
# with the painter's algorithm so it composes with patchwork and exports as PDF like
# any other layer.
#
# Read the caveat in fig_surface3d() before using it. A projected surface is the least
# accurate way to read a value off a two-predictor response, and for most scientific
# purposes a contour plot or a heatmap is the better figure. It earns its place when
# the SHAPE of the surface is the finding.

#' Orthographic projection of 3D points onto the page
#'
#' @description
#' Projects `(x, y, z)` to page coordinates with a rotation about the vertical axis
#' followed by an elevation tilt, and returns the camera depth alongside so callers can
#' sort back-to-front. No perspective: parallel lines stay parallel, which is what a
#' scientific figure wants, because perspective makes equal quantities at different
#' depths render at different sizes.
#'
#' @param x,y,z Numeric vectors of equal length. Scale them to comparable ranges first
#'   unless the units are already comparable; [fig_surface3d()] does this for you.
#' @param theta Rotation about the vertical axis, in degrees.
#' @param phi Elevation of the camera above the horizon, in degrees. `0` is edge-on
#'   and `90` is directly overhead, which collapses the surface into a heatmap.
#'
#' @return A data frame with `X`, `Y` page coordinates and `depth`, larger being
#'   further from the viewer.
#' @export
#' @examples
#' proj3d(c(0, 1, 0, 1), c(0, 0, 1, 1), c(0, 0.5, 0.5, 1))
proj3d <- function(x, y, z, theta = 35, phi = 25) {
  n <- max(length(x), length(y), length(z))
  x <- recycle_to(x, n, "x"); y <- recycle_to(y, n, "y"); z <- recycle_to(z, n, "z")
  check_number(theta, "theta"); check_number(phi, "phi", min = -90, max = 90)
  t <- theta * pi / 180
  p <- phi * pi / 180
  xr <- x * cos(t) - y * sin(t)
  yr <- x * sin(t) + y * cos(t)
  data.frame(
    X = xr,
    Y = yr * sin(p) + z * cos(p),
    depth = yr * cos(p) - z * sin(p)
  )
}

#' Projected 3D surface
#'
#' @description
#' A static, vector, themed surface for the case where the *shape* of a two-predictor
#' response is the scientific object: a saddle, a ridge, an interaction that reverses.
#'
#' **Read this before using it.** A projected surface is the least accurate way to
#' present a value. The reader must judge a height through a rotation, near facets
#' occlude far ones, and no axis can be drawn that lets a value be read off. Cleveland
#' and McGill's ranking puts this kind of judgement at the bottom. For almost every
#' purpose other than showing shape, a contour plot or a heatmap with an anchored
#' diverging scale -- see [fig_heatmap()] -- answers the question better and takes less
#' space. If you need both, show the heatmap and put the surface beside it.
#'
#' The surface is drawn as quadrilateral facets sorted back-to-front (the painter's
#' algorithm), so it is ordinary vector output: it exports to PDF, embeds fonts,
#' composes with `patchwork`, and carries a `vellum` palette like any other layer.
#'
#' @param data A numeric matrix, with `rownames` and `colnames` giving the x and y
#'   coordinates, or a long data frame.
#' @param x,y,z Column names when `data` is a long data frame.
#' @param theta,phi Viewing angles, passed to [proj3d()].
#' @param aspect Relative extent of the x, y, and z axes after each is scaled to a
#'   common range. Lowering the third element flattens the surface, which is usually
#'   necessary to stop a tall response filling the panel.
#' @param palette Sequential or diverging palette for facet fill.
#' @param midpoint If given, the fill scale becomes diverging and is anchored here.
#' @param wireframe Draw facet edges. At small sizes this reads as a mesh and helps
#'   convey curvature; turn it off for a dense grid where the edges would dominate.
#' @param box Draw the projected base rectangle and vertical corner edges, which give
#'   the eye a reference frame for the rotation.
#' @param venue Target venue, for type and rule weights.
#' @param legend_title Legend title for the z scale.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' g <- expand.grid(x = seq(-3, 3, length.out = 30), y = seq(-3, 3, length.out = 30))
#' g$z <- with(g, sin(x) * cos(y) * exp(-(x^2 + y^2) / 12))
#' fig_surface3d(g, "x", "y", "z", midpoint = 0)
fig_surface3d <- function(data, x = NULL, y = NULL, z = NULL,
                          theta = 35, phi = 25, aspect = c(1, 1, 0.55),
                          palette = if (is.null(midpoint)) "ember" else "strata",
                          midpoint = NULL, wireframe = TRUE, box = TRUE,
                          venue = "nature", legend_title = NULL) {
  if (length(aspect) != 3L) cli::cli_abort("{.arg aspect} must have three values.")

  # -- normalise the input to a matrix on a regular grid ----------------------
  if (is.matrix(data)) {
    m <- data
    xv <- suppressWarnings(as.numeric(rownames(m))) %none% seq_len(nrow(m))
    yv <- suppressWarnings(as.numeric(colnames(m))) %none% seq_len(ncol(m))
    if (anyNA(xv)) xv <- seq_len(nrow(m))
    if (anyNA(yv)) yv <- seq_len(ncol(m))
  } else {
    if (is.null(x) || is.null(y) || is.null(z)) {
      cli::cli_abort("With a data frame, name the {.arg x}, {.arg y} and {.arg z} columns.")
    }
    xv <- sort(unique(as.numeric(data[[x]])))
    yv <- sort(unique(as.numeric(data[[y]])))
    m <- matrix(NA_real_, length(xv), length(yv))
    m[cbind(match(as.numeric(data[[x]]), xv), match(as.numeric(data[[y]]), yv))] <-
      as.numeric(data[[z]])
  }
  nx <- length(xv); ny <- length(yv)
  if (nx < 2L || ny < 2L) cli::cli_abort("Need at least a 2 x 2 grid to build facets.")

  # -- scale each axis to a common range, then apply the aspect --------------
  rescale01 <- function(v) {
    finite <- is.finite(v)
    if (!any(finite)) return(rep(NA_real_, length(v)))
    r <- range(v[finite])
    if (diff(r) == 0) return(ifelse(finite, 0.5, NA_real_))
    (v - r[1]) / diff(r)
  }
  xs <- rescale01(xv) * aspect[1]
  ys <- rescale01(yv) * aspect[2]
  zs_all <- rescale01(as.vector(m)) * aspect[3]
  zs <- matrix(zs_all, nx, ny)

  # -- build the facets ------------------------------------------------------
  ii <- rep(seq_len(nx - 1L), times = ny - 1L)
  jj <- rep(seq_len(ny - 1L), each = nx - 1L)
  id <- seq_along(ii)
  corner <- data.frame(
    id = rep(id, 4L),
    xi = c(ii, ii + 1L, ii + 1L, ii),
    yj = c(jj, jj, jj + 1L, jj + 1L),
    ord = rep(1:4, each = length(id))
  )
  corner <- corner[order(corner$id, corner$ord), , drop = FALSE]
  corner$zs <- zs[cbind(corner$xi, corner$yj)]
  corner$zraw <- m[cbind(corner$xi, corner$yj)]

  # A facet with any missing corner cannot be drawn.
  ok <- !is.na(corner$zs)
  keep <- id[!(id %in% unique(corner$id[!ok]))]
  corner <- corner[corner$id %in% keep, , drop = FALSE]
  if (!nrow(corner)) cli::cli_abort("Every facet has a missing corner; nothing to draw.")

  pr <- proj3d(xs[corner$xi], ys[corner$yj], corner$zs, theta = theta, phi = phi)
  corner$X <- pr$X; corner$Y <- pr$Y; corner$depth <- pr$depth

  # Painter's algorithm: one depth per facet, drawn far to near.
  agg <- stats::aggregate(cbind(depth, zraw) ~ id, corner, mean)
  names(agg) <- c("id", "facet_depth", "fill")
  corner <- merge(corner, agg, by = "id")
  corner <- corner[order(-corner$facet_depth, corner$id, corner$ord), , drop = FALSE]
  corner$id <- factor(corner$id, levels = unique(corner$id))

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(corner, ggplot2::aes(.data$X, .data$Y, group = .data$id,
                                            fill = .data$fill)) +
    ggplot2::geom_polygon(
      colour = if (wireframe) tk$ink$paper else NA,
      linewidth = if (wireframe) tk$rule$hairline else 0
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, fill = legend_title %||% "z") +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(legend.position = "right")

  p <- p + if (is.null(midpoint)) {
    scale_fill_vellum_c(palette)
  } else {
    scale_fill_vellum_div(palette, midpoint = midpoint)
  }

  if (box) {
    # Base rectangle at z = 0 plus the four vertical corner posts, which is the
    # minimum needed for the eye to recover the rotation.
    bx <- range(xs); by <- range(ys); bz <- c(0, aspect[3])
    base <- proj3d(bx[c(1, 2, 2, 1, 1)], by[c(1, 1, 2, 2, 1)], 0, theta, phi)
    posts <- do.call(rbind, lapply(seq_len(4), function(k) {
      cx <- bx[c(1, 2, 2, 1)][k]; cy <- by[c(1, 1, 2, 2)][k]
      pp <- proj3d(cx, cy, bz, theta, phi)
      data.frame(X = pp$X, Y = pp$Y, post = k)
    }))
    p <- p +
      ggplot2::geom_path(data = base, ggplot2::aes(.data$X, .data$Y),
                         inherit.aes = FALSE, linewidth = tk$rule$hairline,
                         colour = tk$ink$weak) +
      ggplot2::geom_path(data = posts,
                         ggplot2::aes(.data$X, .data$Y, group = .data$post),
                         inherit.aes = FALSE, linewidth = tk$rule$hairline,
                         colour = tk$ink$weak, linetype = "12")
  }
  p
}

#' Projected 3D surface with confidence-interval blankets
#'
#' @description
#' Draws a smooth fitted surface between sparse lower and upper interval
#' wireframes. All three surfaces share one transformation and camera, so their
#' vertical separation is meaningful. The result is an ordinary `ggplot`: vector
#' output, composable with `patchwork`, and governed by the venue theme.
#'
#' This display is reserved for cases where the nonlinear *shape* of a
#' two-predictor surface is itself the result. Pair it with a contour or interval-
#' width field whenever readers need to recover values without occlusion.
#'
#' @param data Long data frame containing a complete regular grid.
#' @param x,y,z Column names for the two predictors and fitted estimate.
#' @param lower,upper Column names for the lower and upper interval limits.
#' @param theta,phi Camera rotation and elevation in degrees.
#' @param aspect Relative x, y, and z extents after common scaling.
#' @param palette Sequential palette used for the fitted surface.
#' @param wire_stride Draw every `wire_stride`th row and column of each interval
#'   blanket. Sparse blankets reveal uncertainty without hiding the estimate.
#' @param zlim Optional common z limits. Defaults to the range of both interval
#'   bounds.
#' @param venue Target venue.
#' @param xlab,ylab,zlab Axis labels.
#' @param legend_title Fitted-surface colour legend title.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' g <- expand.grid(x = seq(-2, 2, length.out = 20),
#'                  y = seq(-2, 2, length.out = 20))
#' g$p <- with(g, plogis(x - y + x * y))
#' g$lo <- pmax(0, g$p - 0.08)
#' g$hi <- pmin(1, g$p + 0.08)
#' fig_surface3d_ci(g, "x", "y", "p", "lo", "hi")
fig_surface3d_ci <- function(data, x, y, z, lower, upper,
                             theta = 38, phi = 24,
                             aspect = c(1, 1, 0.62),
                             palette = "verdant", wire_stride = 4L,
                             zlim = NULL, venue = "nature",
                             xlab = x, ylab = y, zlab = z,
                             legend_title = "Estimate") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  cols <- c(x, y, z, lower, upper)
  if (!all(cols %in% names(data))) {
    cli::cli_abort("Columns not found: {.val {setdiff(cols, names(data))}}.")
  }
  if (length(aspect) != 3L || any(!is.finite(aspect)) || any(aspect <= 0)) {
    cli::cli_abort("{.arg aspect} must contain three positive finite values.")
  }
  check_number(wire_stride, "wire_stride", min = 1)

  xv <- sort(unique(as.numeric(data[[x]])))
  yv <- sort(unique(as.numeric(data[[y]])))
  nx <- length(xv); ny <- length(yv)
  if (nx < 2L || ny < 2L || nrow(data) != nx * ny) {
    cli::cli_abort("{.arg data} must contain one complete regular grid.")
  }

  to_matrix <- function(column) {
    out <- matrix(NA_real_, nx, ny)
    at <- cbind(match(as.numeric(data[[x]]), xv),
                match(as.numeric(data[[y]]), yv))
    out[at] <- as.numeric(data[[column]])
    out
  }
  estimate <- to_matrix(z)
  lo <- to_matrix(lower)
  hi <- to_matrix(upper)
  if (any(!is.finite(estimate)) || any(!is.finite(lo)) || any(!is.finite(hi))) {
    cli::cli_abort("The estimate and interval grids must be finite.")
  }
  if (any(lo > estimate) || any(estimate > hi)) {
    cli::cli_abort("Every row must satisfy {.arg lower} <= {.arg z} <= {.arg upper}.")
  }

  zlim <- zlim %||% range(lo, hi)
  if (length(zlim) != 2L || any(!is.finite(zlim)) || diff(zlim) <= 0) {
    cli::cli_abort("{.arg zlim} must contain two increasing finite values.")
  }
  if (min(lo) < zlim[1] || max(hi) > zlim[2]) {
    cli::cli_abort("{.arg zlim} must contain both interval surfaces.")
  }

  norm <- function(v, lim) (v - lim[1]) / diff(lim)
  xs <- norm(xv, range(xv)) * aspect[1]
  ys <- norm(yv, range(yv)) * aspect[2]
  zs <- norm(estimate, zlim) * aspect[3]
  los <- norm(lo, zlim) * aspect[3]
  his <- norm(hi, zlim) * aspect[3]

  ii <- rep(seq_len(nx - 1L), times = ny - 1L)
  jj <- rep(seq_len(ny - 1L), each = nx - 1L)
  id <- seq_along(ii)
  facets <- data.frame(
    id = rep(id, 4L),
    xi = c(ii, ii + 1L, ii + 1L, ii),
    yj = c(jj, jj, jj + 1L, jj + 1L),
    ord = rep(1:4, each = length(id))
  )
  facets <- facets[order(facets$id, facets$ord), , drop = FALSE]
  facets$zscaled <- zs[cbind(facets$xi, facets$yj)]
  facets$zraw <- estimate[cbind(facets$xi, facets$yj)]
  projected <- proj3d(xs[facets$xi], ys[facets$yj], facets$zscaled,
                      theta = theta, phi = phi)
  facets$X <- projected$X
  facets$Y <- projected$Y
  facets$depth <- projected$depth
  order_data <- stats::aggregate(cbind(depth, zraw) ~ id, facets, mean)
  names(order_data) <- c("id", "facet_depth", "fill")
  facets <- merge(facets, order_data, by = "id")
  facets <- facets[order(-facets$facet_depth, facets$id, facets$ord), , drop = FALSE]
  facets$id <- factor(facets$id, levels = unique(facets$id))

  wire_index <- unique(c(1L, seq.int(1L, max(nx, ny),
                                     by = as.integer(wire_stride)), nx, ny))
  wire_index_x <- wire_index[wire_index <= nx]
  wire_index_y <- wire_index[wire_index <= ny]
  make_wire <- function(surface, bound) {
    rows <- do.call(rbind, lapply(wire_index_x, function(i) {
      pr <- proj3d(xs[i], ys, surface[i, ], theta = theta, phi = phi)
      data.frame(X = pr$X, Y = pr$Y, line = paste0("r", i), bound = bound)
    }))
    columns <- do.call(rbind, lapply(wire_index_y, function(j) {
      pr <- proj3d(xs, ys[j], surface[, j], theta = theta, phi = phi)
      data.frame(X = pr$X, Y = pr$Y, line = paste0("c", j), bound = bound)
    }))
    rbind(rows, columns)
  }
  lower_wire <- make_wire(los, "Lower")
  upper_wire <- make_wire(his, "Upper")

  axis_line <- function(axis) {
    u <- seq(0, 1, length.out = 40)
    if (axis == "x") proj3d(u * aspect[1], 0, 0, theta, phi)
    else if (axis == "y") proj3d(0, u * aspect[2], 0, theta, phi)
    else proj3d(0, aspect[2], u * aspect[3], theta, phi)
  }
  axes <- rbind(
    transform(axis_line("x"), axis = "x"),
    transform(axis_line("y"), axis = "y"),
    transform(axis_line("z"), axis = "z")
  )

  axis_labels <- function(values, lim, axis) {
    pos <- norm(values, lim)
    pr <- if (axis == "x") proj3d(pos * aspect[1], 0, 0, theta, phi)
    else if (axis == "y") proj3d(0, pos * aspect[2], 0, theta, phi)
    else proj3d(0, aspect[2], pos * aspect[3], theta, phi)
    data.frame(X = pr$X, Y = pr$Y,
               label = label_vellum()(values), axis = axis)
  }
  xbreaks <- pretty(xv, n = 3)
  xbreaks <- xbreaks[xbreaks >= min(xv) & xbreaks <= max(xv)]
  ybreaks <- pretty(yv, n = 3)
  ybreaks <- ybreaks[ybreaks >= min(yv) & ybreaks <= max(yv)]
  zbreaks <- pretty(zlim, n = 4)
  zbreaks <- zbreaks[zbreaks >= zlim[1] & zbreaks <= zlim[2]]
  labels <- rbind(
    axis_labels(xbreaks, range(xv), "x"),
    axis_labels(ybreaks, range(yv), "y"),
    axis_labels(zbreaks, zlim, "z")
  )
  labels$X <- labels$X + ifelse(labels$axis == "y", -0.035,
                                ifelse(labels$axis == "z", -0.035, 0))
  labels$Y <- labels$Y + ifelse(labels$axis == "x", -0.045,
                                ifelse(labels$axis == "y", 0.018, 0))
  labels$hjust <- ifelse(labels$axis == "y", 1,
                         ifelse(labels$axis == "z", 1, 0.5))

  xmid <- proj3d(0.5 * aspect[1], 0, 0, theta, phi)
  ymid <- proj3d(0, 0.5 * aspect[2], 0, theta, phi)
  zmid <- proj3d(0, aspect[2], 0.5 * aspect[3], theta, phi)
  xangle <- atan2(
    sin(theta * pi / 180) * sin(phi * pi / 180),
    cos(theta * pi / 180)
  ) * 180 / pi
  yangle <- atan2(
    cos(theta * pi / 180) * sin(phi * pi / 180),
    -sin(theta * pi / 180)
  ) * 180 / pi - 180
  titles <- data.frame(
    X = c(xmid$X + 0.025, ymid$X - 0.18, zmid$X - 0.145),
    Y = c(xmid$Y - 0.105, ymid$Y + 0.025, zmid$Y),
    label = c(xlab, ylab, zlab),
    angle = c(xangle, yangle, 90)
  )

  tk <- vellum_tokens(venue)
  ggplot2::ggplot() +
    ggplot2::geom_path(
      data = lower_wire,
      ggplot2::aes(.data$X, .data$Y,
                   group = interaction(.data$bound, .data$line),
                   colour = .data$bound, linetype = .data$bound),
      linewidth = tk$rule$hairline, alpha = 0.48
    ) +
    ggplot2::geom_polygon(
      data = facets,
      ggplot2::aes(.data$X, .data$Y, group = .data$id, fill = .data$fill),
      colour = NA
    ) +
    ggplot2::geom_path(
      data = upper_wire,
      ggplot2::aes(.data$X, .data$Y,
                   group = interaction(.data$bound, .data$line),
                   colour = .data$bound, linetype = .data$bound),
      linewidth = tk$rule$hairline, alpha = 0.58
    ) +
    ggplot2::geom_path(
      data = axes, ggplot2::aes(.data$X, .data$Y, group = .data$axis),
      colour = tk$ink$text, linewidth = tk$rule$axis
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(.data$X, .data$Y, label = .data$label, hjust = .data$hjust),
      family = tk$type$family, size = fs(tk$type$size$axis_text),
      colour = tk$ink$text
    ) +
    ggplot2::geom_text(
      data = titles,
      ggplot2::aes(.data$X, .data$Y, label = .data$label, angle = .data$angle),
      family = tk$type$family, size = fs(tk$type$size$axis_title),
      colour = tk$ink$text
    ) +
    scale_fill_vellum_c(palette, name = legend_title) +
    ggplot2::scale_colour_manual(
      name = "95% interval", values = c(Lower = "#0072B2", Upper = "#D55E00")
    ) +
    ggplot2::scale_linetype_manual(
      name = "95% interval", values = c(Lower = "22", Upper = "solid")
    ) +
    ggplot2::coord_equal(clip = "off") +
    theme_vellum_void(venue = venue) +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(fs(5), fs(8), fs(9), fs(10))
    )
}
