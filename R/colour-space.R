# Colour-space primitives.
#
# Ramps are constructed in a perceptually uniform polar space so that a straight
# path through the space is a straight path through perception. OKLCh is the
# default because CIE LCh has a well-known hue non-uniformity in the blues that
# makes blue-to-purple ramps drift visibly; OKLab was designed to fix exactly
# that. CIE LCh remains available because it is the space most scientific
# colour work is reported in.
#
# Auditing, however, is always reported in CIE L* (0-100), because that is the
# quantity the literature on greyscale survival and perceptual uniformity cites.

.spaces <- c("oklch", "lch")

space_L_range <- function(space) {
  switch(space, oklch = c(0, 1), lch = c(0, 100),
         cli::cli_abort("Unsupported space {.val {space}}."))
}

space_C_max <- function(space) {
  switch(space, oklch = 0.45, lch = 150,
         cli::cli_abort("Unsupported space {.val {space}}."))
}

#' Convert between hex colours and perceptual polar coordinates
#'
#' @description
#' Thin, explicit wrappers over `farver` so that the rest of the package never
#' handles a raw matrix layout by accident. `space = "oklch"` gives OKLab's polar
#' form, in which lightness runs 0 to 1; `space = "lch"` gives CIE LCh(ab), in
#' which lightness runs 0 to 100.
#'
#' @param x For `as_polar()`, a character vector of colours. For `from_polar()`,
#'   a three-column matrix or data frame of `L`, `C`, `h`.
#' @param space `"oklch"` (default) or `"lch"`.
#'
#' @return `as_polar()` returns a matrix with columns `L`, `C`, `h`.
#'   `from_polar()` returns a character vector of hex colours.
#'
#' @examples
#' as_polar(c("#E69F00", "#0072B2"))
#' from_polar(as_polar("#E69F00"))
#' @export
as_polar <- function(x, space = "oklch") {
  check_colour(x, "x")
  space <- match.arg(space, .spaces)
  rgb <- t(grDevices::col2rgb(x))
  out <- farver::convert_colour(rgb, from = "rgb", to = space)
  colnames(out) <- c("L", "C", "h")
  out
}

#' @rdname as_polar
#' @export
from_polar <- function(x, space = "oklch") {
  space <- match.arg(space, .spaces)
  m <- as.matrix(x)
  if (ncol(m) != 3L) cli::cli_abort("{.arg x} must have three columns: L, C, h.")
  rgb <- farver::convert_colour(m, from = space, to = "rgb")
  rgb <- clamp(round(rgb), 0, 255)
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255)
}

# Non-clipping conversion to linear sRGB.
#
# `farver::convert_colour(to = "rgb")` clamps its output to 0-255, which is
# correct for display but useless for gamut detection: an impossible colour comes
# back as a plausible one. Detecting that a colour is out of gamut requires
# seeing the negative and greater-than-one values, so the transform is done here
# with published constants instead.
#
# These matrices are verified numerically against farver on in-gamut colours in
# the test suite, so a transcription error cannot pass silently.

# Ottosson's OKLab inverse transform.
.OKLAB_TO_LMS <- matrix(c(
  1,  0.3963377774,  0.2158037573,
  1, -0.1055613458, -0.0638541728,
  1, -0.0894841775, -1.2914855480
), nrow = 3, byrow = TRUE)

.LMS_TO_LINEAR_SRGB <- matrix(c(
   4.0767416621, -3.3077115913,  0.2309699292,
  -1.2684380046,  2.6097574011, -0.3413193965,
  -0.0041960863, -0.7034186147,  1.7076147010
), nrow = 3, byrow = TRUE)

# IEC 61966-2-1 sRGB primaries, D65 white.
.XYZ_TO_LINEAR_SRGB <- matrix(c(
   3.2404542, -1.5371385, -0.4985314,
  -0.9692660,  1.8760108,  0.0415560,
   0.0556434, -0.2040259,  1.0572252
), nrow = 3, byrow = TRUE)

.D65 <- c(95.047, 100.000, 108.883)

polar_to_rect <- function(L, C, h) {
  rad <- h * pi / 180
  cbind(L = L, a = C * cos(rad), b = C * sin(rad))
}

lab_to_xyz <- function(lab) {
  fy <- (lab[, 1] + 16) / 116
  fx <- fy + lab[, 2] / 500
  fz <- fy - lab[, 3] / 200
  finv <- function(t) ifelse(t^3 > 0.008856, t^3, (116 * t - 16) / 903.3)
  cbind(finv(fx) * .D65[1], finv(fy) * .D65[2], finv(fz) * .D65[3])
}

#' Convert perceptual polar coordinates to unclipped linear sRGB
#'
#' @description
#' The transform that makes gamut detection possible. Unlike a display
#' conversion, this one is allowed to return values below 0 and above 1, which is
#' exactly the information needed to tell whether a colour can be shown at all.
#'
#' @param L,C,h Numeric vectors of lightness, chroma, and hue angle.
#' @param space `"oklch"` or `"lch"`.
#' @return A three-column matrix of linear sRGB values, not clipped.
#' @export
#' @examples
#' polar_to_linear_srgb(0.6, 0.12, 60)   # all within 0-1: displayable
#' polar_to_linear_srgb(0.6, 0.40, 60)   # outside 0-1: impossible in sRGB
polar_to_linear_srgb <- function(L, C, h, space = "oklch") {
  space <- match.arg(space, .spaces)
  n <- max(length(L), length(C), length(h))
  rect <- polar_to_rect(recycle_to(L, n, "L"), recycle_to(C, n, "C"), recycle_to(h, n, "h"))
  if (space == "oklch") {
    lms <- rect %*% t(.OKLAB_TO_LMS)
    lin <- (lms^3) %*% t(.LMS_TO_LINEAR_SRGB)
  } else {
    lin <- (lab_to_xyz(rect) / 100) %*% t(.XYZ_TO_LINEAR_SRGB)
  }
  colnames(lin) <- c("r", "g", "b")
  lin
}

#' Is a perceptual colour inside the sRGB gamut?
#'
#' A colour specified in a perceptual space can be outside the range any screen
#' or printer can show. Naive code converts anyway and lets the RGB values clip,
#' which shifts both lightness and hue and silently destroys a designed ramp.
#' This test lets [gamut_map()] avoid that.
#'
#' @param L,C,h Numeric vectors of equal length, or length one, giving lightness,
#'   chroma, and hue angle in `space`.
#' @param space `"oklch"` or `"lch"`.
#' @param tol Tolerance in encoded sRGB units, where 1 is full scale. The default
#'   of `0.5/255` is half an 8-bit code value: a colour counts as displayable if it
#'   rounds to a valid 8-bit triple. Testing in the encoded domain matters, because
#'   that is where quantisation happens -- a tolerance set in linear light would be
#'   far too tight near black and too loose near white, and would reject colours
#'   that have merely been round-tripped through a hex string.
#'
#' @return A logical vector.
#' @export
#' @examples
#' in_gamut(0.6, 0.12, 60)   # a plausible amber
#' in_gamut(0.6, 0.40, 60)   # far outside anything sRGB can show
#'
#' # A colour that came from a hex string is always displayable, by definition.
#' p <- as_polar("#4292C6")
#' in_gamut(p[, "L"], p[, "C"], p[, "h"])
in_gamut <- function(L, C, h, space = "oklch", tol = 0.5 / 255) {
  lin <- polar_to_linear_srgb(L, C, h, space = space)
  enc <- srgb_encode(lin)
  ok <- enc >= -tol & enc <= 1 + tol
  !is.na(rowSums(enc)) & apply(ok, 1, all)
}

# The sRGB transfer function, extended through zero with an odd (sign-preserving)
# continuation so out-of-gamut negatives stay measurable instead of becoming NaN.
# This is the standard way to keep the encoding invertible outside the unit cube.
srgb_encode <- function(x) {
  a <- abs(x)
  sign(x) * ifelse(a <= 0.0031308, 12.92 * a, 1.055 * a^(1 / 2.4) - 0.055)
}

#' Map a colour into the sRGB gamut by reducing chroma only
#'
#' @description
#' The correct way to bring an out-of-gamut perceptual colour into sRGB is to
#' hold lightness and hue fixed and reduce chroma until the colour fits. Chroma
#' is the one channel in a well-designed scientific ramp that carries no
#' information, so it is the one channel that can be sacrificed. Clipping RGB
#' channels instead, which is what most code does, changes lightness and hue and
#' therefore breaks both greyscale survival and the designed hue path.
#'
#' The search is a bisection, so the result is the most saturated in-gamut colour
#' at that lightness and hue, to within `tol`.
#'
#' @param L,C,h Numeric vectors of lightness, chroma, and hue angle.
#' @param space `"oklch"` or `"lch"`.
#' @param tol Chroma tolerance for the bisection, in the units of `space`.
#'
#' @return A matrix with columns `L`, `C`, `h`, gamut-mapped.
#' @export
#' @examples
#' # An impossible chroma is pulled back, but lightness and hue are preserved.
#' gamut_map(0.6, 0.40, 60)
gamut_map <- function(L, C, h, space = "oklch", tol = NULL) {
  space <- match.arg(space, .spaces)
  tol <- tol %||% (space_C_max(space) * 1e-4)
  n <- max(length(L), length(C), length(h))
  L <- recycle_to(L, n, "L"); C <- recycle_to(C, n, "C"); h <- recycle_to(h, n, "h")

  ok <- in_gamut(L, C, h, space)
  need <- which(!ok)
  if (length(need)) {
    lo <- rep(0, length(need))
    hi <- C[need]
    # 40 bisection steps resolve any realistic chroma range far below tol.
    for (i in seq_len(40L)) {
      mid <- (lo + hi) / 2
      fits <- in_gamut(L[need], mid, h[need], space)
      lo <- ifelse(fits, mid, lo)
      hi <- ifelse(fits, hi, mid)
      if (all(hi - lo < tol)) break
    }
    C[need] <- lo
  }
  cbind(L = L, C = C, h = h)
}

#' Simulate colour-vision deficiency
#'
#' @description
#' Uses the physiologically based model of Machado, Oliveira and Fernandes (2009)
#' as implemented in `colorspace`. Delegation is deliberate: that model is a
#' table of published per-severity matrices, and `colorspace` is a mature,
#' widely audited implementation of it. Reproducing those constants by hand would
#' add risk and no capability.
#'
#' @param x A character vector of colours.
#' @param type `"deutan"` (green-weak, the most common), `"protan"` (red-weak),
#'   `"tritan"` (blue-weak), or `"grey"` for luminance-correct desaturation,
#'   which stands in for greyscale printing and photocopying.
#' @param severity Between 0 and 1. `1` is dichromacy; intermediate values are
#'   anomalous trichromacy.
#'
#' @return A character vector of colours.
#'
#' @references
#' Machado GM, Oliveira MM, Fernandes LAF (2009). A physiologically-based model
#' for simulation of color vision deficiency. *IEEE Transactions on
#' Visualization and Computer Graphics* 15(6):1291-1298.
#'
#' @export
#' @examples
#' cvd(c("#009E73", "#D55E00"), "deutan")
#' cvd(c("#009E73", "#D55E00"), "grey")
cvd <- function(x, type = c("deutan", "protan", "tritan", "grey"), severity = 1) {
  check_colour(x, "x")
  type <- match.arg(type)
  check_number(severity, "severity", min = 0, max = 1)
  check_installed("colorspace", "to simulate colour-vision deficiency")
  out <- switch(
    type,
    deutan = colorspace::deutan(x, severity = severity),
    protan = colorspace::protan(x, severity = severity),
    tritan = colorspace::tritan(x, severity = severity),
    grey   = colorspace::desaturate(x, amount = severity)
  )
  toupper(substr(out, 1, 7))
}

#' CIEDE2000 colour difference
#'
#' @description
#' The CIE's current recommendation for perceived colour difference (CIE
#' 142-2001). A difference of about 1 is the just-noticeable difference under
#' ideal viewing; the thresholds this package audits against are stated in
#' [palette_audit()] rather than hidden.
#'
#' @param x A character vector of colours.
#' @param y Optional second vector. If omitted, all pairs within `x` are
#'   compared and a full matrix is returned.
#'
#' @return A numeric matrix of `Delta E00` values.
#' @export
#' @examples
#' delta_e(c("#E69F00", "#56B4E9", "#009E73"))
delta_e <- function(x, y = NULL) {
  check_colour(x, "x")
  a <- t(grDevices::col2rgb(x))
  b <- if (is.null(y)) a else t(grDevices::col2rgb(y))
  m <- farver::compare_colour(a, b, from_space = "rgb", method = "cie2000", white_from = "D65")
  dimnames(m) <- list(x, if (is.null(y)) x else y)
  m
}

#' Minimum pairwise colour separation
#'
#' The weakest link in a qualitative palette: the closest pair. This is the
#' number that decides whether a reader can tell two groups apart.
#'
#' @param x A character vector of colours.
#' @return A single `Delta E00` value, or `Inf` for fewer than two colours.
#' @export
#' @examples
#' min_separation(okabe_ito())
min_separation <- function(x) {
  if (length(x) < 2L) return(Inf)
  m <- delta_e(x)
  min(m[lower.tri(m)])
}

#' CIE L*, relative luminance, and contrast ratio
#'
#' `lightness()` returns CIE L* on 0-100, the standard scale for reporting
#' perceptual lightness. `luminance()` returns WCAG relative luminance on 0-1.
#' `contrast_ratio()` returns the WCAG 2.1 ratio between two colours, where 4.5
#' is the threshold for normal body text and 3 for large text and graphical
#' objects.
#'
#' @param x,y Character vectors of colours.
#' @return Numeric vectors.
#' @export
#' @examples
#' lightness(c("#FFFFFF", "#767676", "#000000"))
#' contrast_ratio("#1A1A1A", "#FFFFFF")
lightness <- function(x) {
  check_colour(x, "x")
  unname(farver::convert_colour(t(grDevices::col2rgb(x)), from = "rgb", to = "lab")[, 1])
}

#' @rdname lightness
#' @export
luminance <- function(x) {
  check_colour(x, "x")
  rgb <- t(grDevices::col2rgb(x)) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(lin %*% c(0.2126, 0.7152, 0.0722))
}

#' @rdname lightness
#' @export
contrast_ratio <- function(x, y) {
  lx <- luminance(x); ly <- luminance(y)
  n <- max(length(lx), length(ly))
  lx <- recycle_to(lx, n, "x"); ly <- recycle_to(ly, n, "y")
  (pmax(lx, ly) + 0.05) / (pmin(lx, ly) + 0.05)
}
