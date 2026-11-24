# The ramp engine.
#
# A scientific colour ramp is not a list of pretty colours; it is a measurement
# scale. Three properties make it one, and all three are constructed here rather
# than hoped for:
#
#   1. monotone lightness, so the ramp survives greyscale and all three
#      dichromacies for free;
#   2. lightness linear in ramp position, so equal steps in the data are equal
#      steps in perception;
#   3. every colour inside the destination gamut, so the publisher's colour
#      conversion cannot silently reorder the scale.
#
# Failure to do (1) is precisely why jet and rainbow manufacture false contours:
# non-monotone L* creates visual edges where the data is smooth, and plateaus in
# L* hide real structure.

#' Construct a perceptually engineered colour ramp
#'
#' @description
#' Builds a ramp from a specification rather than from hand-chosen hex values.
#' The path is interpolated in a perceptually uniform polar space, then lightness
#' is resampled to be exactly linear in ramp position, then every colour is
#' brought into the sRGB gamut by reducing chroma at fixed lightness and hue.
#'
#' The result is measurable after the fact: pass it to [palette_audit()] and the
#' lightness range, monotonicity, deviation from linear, and colour-vision
#' separation are all reported as numbers.
#'
#' @param n Number of colours to return.
#' @param L Lightness endpoints in the units of `space`, as `c(from, to)`. For
#'   `"oklch"` the range is 0-1; for `"lch"` it is 0-100. Defaults to a wide but
#'   printable range that keeps the dark end off pure black and the light end off
#'   pure white, both of which are unreadable against typical page furniture.
#' @param C Chroma at the ramp's midpoint, or `c(from, mid, to)` to shape the
#'   chroma envelope explicitly. A single value produces a parabolic envelope
#'   that fades chroma toward both ends, which is what keeps the extremes from
#'   looking artificially neon.
#' @param h Hue angle in degrees, or `c(from, to)` for a hue path. Hue is
#'   interpolated the short way round unless `hue_direction` says otherwise.
#' @param hue_direction `"short"`, `"long"`, `"cw"`, or `"ccw"`.
#' @param type `"sequential"`, `"diverging"`, or `"cyclic"`.
#' @param pivot For diverging ramps, the lightness of the midpoint: `"light"`
#'   (the usual choice, a pale neutral hinge) or `"dark"`.
#' @param linearise Resample so lightness is affine in position. Leave `TRUE`
#'   unless you are deliberately demonstrating what happens without it.
#' @param space `"oklch"` (default) or `"lch"`.
#' @param rev Reverse the returned ramp.
#'
#' @return A character vector of `n` hex colours, with class `vellum_ramp` and
#'   the construction specification kept in the `spec` attribute so the recipe
#'   travels with the result.
#'
#' @examples
#' vellum_ramp(7, h = c(280, 60))                      # deep violet to amber
#' vellum_ramp(9, h = c(250, 20), type = "diverging")   # blue to red, light hinge
#' @export
vellum_ramp <- function(n = 256,
                        L = NULL, C = NULL, h = c(250, 80),
                        hue_direction = c("short", "long", "cw", "ccw"),
                        type = c("sequential", "diverging", "cyclic"),
                        pivot = c("light", "dark"),
                        linearise = TRUE,
                        space = c("oklch", "lch"),
                        rev = FALSE) {
  space <- match.arg(space)
  type <- match.arg(type)
  pivot <- match.arg(pivot)
  hue_direction <- match.arg(hue_direction)
  check_number(n, "n", min = 1)
  check_bool(linearise, "linearise")
  n <- as.integer(n)

  rng <- space_L_range(space)
  # Defaults expressed as fractions of the space's lightness range, so the same
  # numbers mean the same thing in OKLCh and CIE LCh.
  L <- L %||% (rng[2] * c(0.22, 0.96))
  C <- C %||% (space_C_max(space) * 0.30)

  if (length(L) != 2L) cli::cli_abort("{.arg L} must be two values: start and end lightness.")
  if (length(h) < 1L || length(h) > 3L) cli::cli_abort("{.arg h} must have one to three values.")

  spec <- list(n = n, L = L, C = C, h = h, hue_direction = hue_direction,
               type = type, pivot = pivot, linearise = linearise, space = space, rev = rev)

  path <- switch(
    type,
    sequential = ramp_path_sequential(n, L, C, h, hue_direction, space),
    diverging  = ramp_path_diverging(n, L, C, h, hue_direction, space, pivot),
    cyclic     = ramp_path_cyclic(n, L, C, h, space)
  )

  if (linearise) path <- linearise_L(path, type = type, pivot = pivot)
  path <- gamut_map(path[, "L"], path[, "C"], path[, "h"], space = space)

  cols <- from_polar(path, space = space)
  if (rev) cols <- rev(cols)
  structure(cols, class = c("vellum_ramp", "character"), spec = spec)
}

# -- path construction --------------------------------------------------------

interp_hue <- function(h, n, direction) {
  if (length(h) == 1L) return(rep(h, n))
  if (length(h) == 2L) {
    from <- h[1]; to <- h[2]
    d <- (to - from) %% 360
    delta <- switch(
      direction,
      short = if (d > 180) d - 360 else d,
      long  = if (d > 180) d else d - 360,
      ccw   = d,
      cw    = d - 360
    )
    return((from + delta * seq(0, 1, length.out = max(n, 2))[seq_len(n)]) %% 360)
  }
  # Three hues: interpolate through the middle anchor.
  half <- ceiling(n / 2)
  c(interp_hue(h[1:2], half, direction), interp_hue(h[2:3], n - half + 1, direction)[-1])
}

# A parabolic chroma envelope: full chroma in the middle, tapering to the ends.
# Neon extremes are the commonest visual failure of hand-built ramps.
chroma_envelope <- function(C, n, taper = c(0.35, 0.55)) {
  if (length(C) == 3L) {
    half <- ceiling(n / 2)
    return(c(seq_between(C[1], C[2], half), seq_between(C[2], C[3], n - half + 1)[-1]))
  }
  if (length(C) == 2L) return(seq_between(C[1], C[2], n))
  t <- seq(0, 1, length.out = max(n, 2))[seq_len(n)]
  # Weight so that position 0 keeps taper[1] of peak chroma and position 1 keeps
  # taper[2]; the peak sits slightly past the middle, which reads as warmer.
  w <- taper[1] + (1 - taper[1]) * sin(pi * t)^0.9
  w <- w * seq_between(1, taper[2] / max(taper[1], 1e-6) * taper[1] + (1 - taper[1]), n)
  C * clamp(w, 0, 1)
}

ramp_path_sequential <- function(n, L, C, h, direction, space) {
  cbind(L = seq_between(L[1], L[2], n),
        C = chroma_envelope(C, n),
        h = interp_hue(h, n, direction))
}

ramp_path_diverging <- function(n, L, C, h, direction, space, pivot) {
  rng <- space_L_range(space)
  if (length(h) < 2L) cli::cli_abort("A diverging ramp needs at least two hues in {.arg h}.")
  hue_lo <- h[1]
  hue_hi <- h[length(h)]

  # Symmetric arms meeting at a near-neutral hinge. The hinge sits at one extreme
  # of the lightness range and both ends at the other, so distance from the
  # midpoint reads as magnitude in lightness as well as in hue.
  #
  # The direction matters and is easy to get wrong: a dark hinge requires LIGHT
  # ends, not darker ends. Running from dark ends to a darker hinge compresses the
  # whole ramp into the region where sRGB cannot hold chroma at all, and gamut
  # mapping then collapses both arms to the same near-black.
  if (pivot == "light") {
    L_end <- min(L); L_mid <- max(L)
  } else {
    L_end <- max(L); L_mid <- min(L)
  }
  Cmax <- if (length(C) == 1L) C else max(C)

  half <- ceiling(n / 2)
  arm <- function(hue) {
    t <- seq(0, 1, length.out = half)
    cbind(L = seq_between(L_end, L_mid, half),
          C = Cmax * (1 - t)^0.75,
          h = rep(hue, half))
  }
  lo <- arm(hue_lo)
  hi <- arm(hue_hi)[seq(half, 1), , drop = FALSE]
  out <- rbind(lo, if (n %% 2L == 1L) hi[-1, , drop = FALSE] else hi)
  out[seq_len(n), , drop = FALSE]
}

ramp_path_cyclic <- function(n, L, C, h, space) {
  hue0 <- h[1]
  t <- seq(0, 1, length.out = n + 1)[seq_len(n)]
  # Lightness rises and falls symmetrically so the two ends meet without a seam.
  cbind(L = mean(L) + diff(range(L)) / 2 * sin(2 * pi * t - pi / 2),
        C = rep(if (length(C) == 1L) C else mean(C), n),
        h = (hue0 + 360 * t) %% 360)
}

#' Resample a ramp so lightness is linear in position
#'
#' @description
#' The step that turns a plausible gradient into a measurement scale. The
#' constructed path is treated as a curve, the cumulative lightness change along
#' it is computed, and the curve is re-sampled at equal lightness intervals.
#' After this, a fixed step in the data is a fixed perceptual step, and the ramp
#' is readable in greyscale because lightness alone encodes the value.
#'
#' Diverging ramps are linearised arm by arm so the hinge stays exactly at the
#' midpoint and the two arms stay symmetric.
#'
#' @param path A matrix with columns `L`, `C`, `h`, as produced internally by
#'   [vellum_ramp()].
#' @param type `"sequential"`, `"diverging"`, or `"cyclic"`.
#' @param pivot `"light"` or `"dark"`, for diverging ramps.
#'
#' @return A matrix with the same dimensions as `path`.
#' @export
#' @examples
#' p <- cbind(L = c(0.2, 0.3, 0.9), C = c(0.1, 0.15, 0.05), h = c(250, 200, 80))
#' linearise_L(p, "sequential")
linearise_L <- function(path, type = c("sequential", "diverging", "cyclic"),
                        pivot = c("light", "dark")) {
  type <- match.arg(type)
  pivot <- match.arg(pivot)
  n <- nrow(path)
  if (n < 3L) return(path)

  resample <- function(p, dir = NULL) {
    m <- nrow(p)
    if (m < 3L) return(p)
    # Work in a signed coordinate that always increases, because stats::approx()
    # requires increasing x and would silently return nonsense for a ramp running
    # light to dark. Flipping the sign rather than reversing the matrix keeps the
    # arithmetic in one place.
    dir <- dir %||% (if (p[m, "L"] >= p[1, "L"]) 1 else -1)
    # Monotone-ise: a non-monotone construction cannot be linearised, so enforce
    # the running extreme in the direction the ramp is going.
    s <- cummax(dir * p[, "L"])
    if (abs(s[m] - s[1]) < .Machine$double.eps^0.5) return(p)
    target <- seq(s[1], s[m], length.out = m)
    idx <- seq_len(m)
    # Interpolate every channel against lightness. Hue is unwrapped first so a
    # path crossing 0/360 does not fold back on itself.
    hu <- unwrap_hue(p[, "h"])
    pos <- stats::approx(x = s, y = idx, xout = target, ties = "ordered", rule = 2)$y
    cbind(
      L = dir * target,
      C = stats::approx(idx, p[, "C"], xout = pos, rule = 2)$y,
      h = stats::approx(idx, hu, xout = pos, rule = 2)$y %% 360
    )
  }

  if (type == "diverging") {
    half <- ceiling(n / 2)
    # Both arms run from an end toward the hinge, so `pivot` fixes the direction
    # each must take. Inferring it from an arm's own endpoints instead lets a
    # non-monotone construction invert the sign and fold that arm back on itself,
    # and lets the two arms be linearised in opposite directions about a hinge
    # that is meant to be symmetric.
    dir <- if (pivot == "light") 1 else -1
    back <- seq(n, half)
    lo <- resample(path[seq_len(half), , drop = FALSE], dir = dir)
    hi <- resample(path[back, , drop = FALSE], dir = dir)[seq(length(back), 1), , drop = FALSE]
    out <- rbind(lo, hi[-1, , drop = FALSE])
    return(out[seq_len(n), , drop = FALSE])
  }
  if (type == "cyclic") {
    half <- ceiling(n / 2)
    return(rbind(resample(path[seq_len(half), , drop = FALSE]),
                 path[seq(half + 1, n), , drop = FALSE])[seq_len(n), , drop = FALSE])
  }
  resample(path)
}

# Unwrap a hue sequence so successive steps never jump more than 180 degrees.
unwrap_hue <- function(h) {
  if (length(h) < 2L) return(h)
  d <- diff(h)
  d <- d - 360 * round(d / 360)
  cumsum(c(h[1], d))
}

#' Interpolate a ramp through given colours, perceptually
#'
#' @description
#' The counterpart to [vellum_ramp()] for when you already have anchor colours --
#' a brand colour, a collaborator's figure, a journal's house hue -- and want a
#' ramp through them that still behaves like a measurement scale. Interpolation
#' happens in the perceptual space, lightness is linearised, and the result is
#' gamut-mapped, so anchors are honoured but the ramp's properties are not left
#' to chance.
#'
#' @param colours Two or more anchor colours, dark to light or as you want them
#'   ordered.
#' @param n Number of output colours.
#' @param space `"oklch"` or `"lch"`.
#' @param linearise Linearise lightness after interpolation.
#' @param type `"sequential"`, `"diverging"`, or `"cyclic"`.
#'
#' @return A character vector of `n` hex colours with class `vellum_ramp`.
#' @export
#' @examples
#' ramp_through(c("#08306B", "#4292C6", "#F7FBFF"), 9)
ramp_through <- function(colours, n = 256, space = c("oklch", "lch"),
                         linearise = TRUE,
                         type = c("sequential", "diverging", "cyclic")) {
  space <- match.arg(space)
  type <- match.arg(type)
  check_colour(colours, "colours")
  if (length(colours) < 2L) cli::cli_abort("{.arg colours} needs at least two anchors.")
  check_number(n, "n", min = 1)
  n <- as.integer(n)

  anchors <- as_polar(colours, space = space)
  anchors[, "h"] <- unwrap_hue(anchors[, "h"])
  at <- seq(0, 1, length.out = nrow(anchors))
  out_at <- seq(0, 1, length.out = max(n, 2))[seq_len(n)]
  path <- cbind(
    L = stats::approx(at, anchors[, "L"], xout = out_at, rule = 2)$y,
    C = stats::approx(at, anchors[, "C"], xout = out_at, rule = 2)$y,
    h = stats::approx(at, anchors[, "h"], xout = out_at, rule = 2)$y %% 360
  )
  if (linearise) path <- linearise_L(path, type = type)
  path <- gamut_map(path[, "L"], path[, "C"], path[, "h"], space = space)
  structure(from_polar(path, space = space),
            class = c("vellum_ramp", "character"),
            spec = list(anchors = colours, n = n, space = space,
                        linearise = linearise, type = type))
}

#' @export
print.vellum_ramp <- function(x, ...) {
  spec <- attr(x, "spec")
  cli::cli_text("{.strong vellum ramp} \u00b7 {length(x)} colours{if (!is.null(spec$type)) paste0(' \u00b7 ', spec$type) else ''}{if (!is.null(spec$space)) paste0(' \u00b7 ', spec$space) else ''}")
  Lv <- lightness(x)
  cli::cli_text("CIE L* {sprintf('%.1f', min(Lv))}\u2013{sprintf('%.1f', max(Lv))} \u00b7 monotone {if (is_monotone(Lv)) 'yes' else 'no'}")
  show_swatches(x)
  invisible(x)
}

is_monotone <- function(x, tol = 0.5) {
  d <- diff(x)
  all(d >= -tol) || all(d <= tol)
}

# Print colour swatches inline using ANSI background colour when the console
# supports it, and fall back to hex codes when it does not.
show_swatches <- function(x, max_n = 48) {
  if (!cli::num_ansi_colors() > 1) {
    cli::cli_verbatim(paste(utils::head(unname(x), 12), collapse = " "))
    return(invisible())
  }
  cols <- if (length(x) > max_n) x[round(seq(1, length(x), length.out = max_n))] else x
  bars <- vapply(cols, function(cl) cli::make_ansi_style(cl, bg = TRUE)("  "), character(1))
  cli::cli_verbatim(paste0(bars, collapse = ""))
  invisible()
}
