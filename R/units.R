#' Physical unit conversion for ggplot2 geometry
#'
#' @description
#' ggplot2's size arguments are not points, and this is the most common reason a
#' figure that looks correct on screen is non-compliant on paper. These
#' converters make the arithmetic explicit so that every visual constant in a
#' figure is traceable to a physical measurement at final print size.
#'
#' The conversions are:
#'
#' * `element_text(size =)` is already in points, so no conversion is needed.
#' * `geom_text(size =)` is multiplied internally by `.pt = 72.27/25.4`, so a
#'   6 pt annotation needs `size = fs(6)`.
#' * `linewidth =` becomes `lwd = linewidth * .pt`, and R's `lwd` unit is
#'   1/96 inch. A rule of *w* points therefore needs `linewidth = lw(w)`.
#' * `geom_point(size =)` renders at approximately *size* millimetres of
#'   diameter, because `.pt` points is exactly 1 mm. Use `ps()` to be explicit.
#'
#' @param pt Size in typographic points (1 pt = 1/72.27 inch).
#' @param mm Size in millimetres.
#'
#' @return A numeric value in the unit ggplot2 expects, one per input. All
#'   five converters are vectorised, so a whole set of measured weights can be
#'   reported in one call; `NA` converts to `NA`.
#'
#' @examples
#' lw(0.25)  # Nature's minimum 0.25 pt hairline as a ggplot2 linewidth
#' fs(6)     # 6 pt annotation text as a geom_text size
#' ps(1.8)   # a 1.8 mm point as a geom_point size
#' @name units
NULL

# ggplot2's internal constants, restated here so the arithmetic is auditable
# rather than borrowed from an unexported object.
.PT <- 72.27 / 25.4      # points per millimetre  (~2.845276)
.LWD_PER_INCH <- 96      # R's lwd unit is 1/96 inch
.PT_PER_INCH <- 72.27

#' @rdname units
#' @export
lw <- function(pt) {
  check_measure(pt, "pt")
  pt * (.LWD_PER_INCH / .PT_PER_INCH) / .PT
}

#' @rdname units
#' @export
fs <- function(pt) {
  check_measure(pt, "pt")
  pt / .PT
}

#' @rdname units
#' @export
ps <- function(mm) {
  check_measure(mm, "mm")
  mm
}

#' @rdname units
#' @export
pt_mm <- function(pt) {
  check_measure(pt, "pt")
  pt / .PT
}

#' @rdname units
#' @export
mm_pt <- function(mm) {
  check_measure(mm, "mm")
  mm * .PT
}

#' Invert a ggplot2 linewidth back to points
#'
#' Used by [venue_check()] to report the rendered weight of a theme element in
#' the units a publisher specifies.
#'
#' @param linewidth A ggplot2 `linewidth` value, or a vector of them.
#' @return The rendered rule weight in typographic points, one per input.
#' @export
#' @examples
#' linewidth_pt(0.5)               # ggplot2's default rule, in points
#' linewidth_pt(c(0.117, 0.5, 1))  # vectorised, for reporting a whole theme
linewidth_pt <- function(linewidth) {
  check_numeric(linewidth, "linewidth")
  linewidth * .PT / (.LWD_PER_INCH / .PT_PER_INCH)
}

#' A grid unit in millimetres
#'
#' Shorthand used throughout the theme layer, where every spacing constant is a
#' physical measurement.
#'
#' @param x Numeric length in millimetres.
#' @return A [grid::unit()] object.
#' @export
#' @examples
#' u_mm(1.2)
u_mm <- function(x) grid::unit(x, "mm")

#' Convert a grid unit to millimetres
#'
#' @param x A [grid::unit()] object.
#' @param dim Which dimension to resolve against, `"width"` or `"height"`.
#' @return A numeric length in millimetres.
#' @export
#' @examples
#' as_mm(grid::unit(1, "inch"))
as_mm <- function(x, dim = c("width", "height")) {
  dim <- match.arg(dim)
  conv <- if (dim == "width") grid::convertWidth else grid::convertHeight
  as.numeric(conv(x, "mm"))
}
