# Design tokens.
#
# Every visual constant in this package is defined once, here, as a physical
# measurement, and everything downstream reads tokens. No theme function contains
# a bare number. The purpose is not tidiness: it is that a figure's geometry has
# to be derivable from the venue it is going to, and a magic number in a leaf
# function cannot be.

#' Resolve a typeface, and say what happened
#'
#' @description
#' Silent font substitution is how a figure ends up submitted in the wrong
#' typeface. This walks a preference list, returns the first family actually
#' installed, and reports the outcome the first time a substitution is needed.
#'
#' @param families Preferred families, most preferred first.
#' @param fallback Family to use if none are installed.
#' @return The resolved family name, with the full preference list in the
#'   `preferred` attribute.
#' @export
#' @examples
#' resolve_font(c("Helvetica", "Arial"))
#' vellum_fonts()
resolve_font <- function(families = c("Helvetica", "Arial", "Helvetica Neue",
                                      "Nimbus Sans", "DejaVu Sans"),
                         fallback = "sans") {
  available <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
  hit <- families[families %in% available]
  if (length(hit)) {
    out <- hit[1]
  } else {
    out <- fallback
    warn_once(
      paste0("font-", paste(families, collapse = "-")),
      c("None of {.val {families}} is installed; falling back to {.val {fallback}}.",
        i = "Journals that name a typeface mean it. Install one, or set {.arg family} explicitly.")
    )
  }
  structure(device_safe_family(out), preferred = families)
}

# Keep the family renderable by whatever device is currently open.
#
# R's classic `pdf()` and `postscript()` devices use a private font database of
# fourteen PostScript families. Asking them for Arial is not a substitution warning
# but a hard error at draw time, which is how a figure that renders in RStudio dies
# inside R CMD check or a headless script.
#
# `save_figure()` sidesteps this by always using cairo and ragg devices, which read
# system fonts. This handles the other case: drawing on whatever device the user
# already has open. The substitutions are metric near-equivalents, and the note is
# emitted once so the user knows the rendered figure is not the submitted one.
device_safe_family <- function(family) {
  dev <- names(grDevices::dev.cur())
  if (!length(dev) || !dev %in% c("pdf", "postscript")) return(family)
  usable <- tryCatch(names(grDevices::pdfFonts()), error = function(e) character())
  if (family %in% usable) return(family)

  alias <- c(
    "Arial" = "Helvetica", "Helvetica Neue" = "Helvetica",
    "Nimbus Sans" = "Helvetica", "DejaVu Sans" = "Helvetica",
    "Inter" = "Helvetica", "Source Sans 3" = "Helvetica",
    "Times New Roman" = "Times", "Courier New" = "Courier",
    "Nimbus Mono PS" = "Courier"
  )
  mapped <- unname(alias[family])
  sub <- if (length(mapped) && !is.na(mapped)) mapped else "sans"
  if (!sub %in% usable && !sub %in% c("sans", "serif", "mono")) sub <- "sans"
  warn_once(
    paste0("device-font-", family),
    c("The current {.code {dev}()} device cannot render {.val {family}}; drawing with {.val {sub}}.",
      i = "R's classic pdf/postscript devices carry only the base PostScript families.",
      i = "{.fn save_figure} uses cairo and ragg devices, which do embed {.val {family}}.")
  )
  sub
}

#' @rdname resolve_font
#' @export
vellum_fonts <- function() {
  available <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
  wanted <- c("Helvetica", "Arial", "Helvetica Neue", "Nimbus Sans", "DejaVu Sans",
              "Inter", "Source Sans 3", "Times New Roman", "Courier", "Courier New")
  data.frame(family = wanted, installed = wanted %in% available,
             role = c(rep("journal sans-serif", 5), rep("modern sans-serif", 2),
                      "serif", rep("monospace (sequences)", 2)),
             stringsAsFactors = FALSE)
}

#' Design tokens for a figure
#'
#' @description
#' The single source of truth for a figure's geometry, ink, typography, and
#' spacing. Tokens are derived from the target venue's published specification and
#' scale coherently: change `base_size` and every rule weight, tick length, point
#' diameter, and margin moves with it, because they are all expressed as ratios
#' of a reference and converted through the physical-unit helpers in [lw()].
#'
#' Text sizes are clamped to the venue's permitted range, so a token set for
#' Nature cannot produce text above 7 pt or below 5 pt no matter what
#' `base_size` is asked for. That clamp is the mechanism that makes
#' [venue_check()] pass by construction rather than by luck.
#'
#' @param venue Venue name, used for the type-size range, panel-label style, and
#'   permitted typefaces. See [venues()].
#' @param base_size Size of the largest ordinary text element, in points.
#'   Defaults to the venue's maximum, which for Nature is 7 pt.
#' @param scale_ratio Ratio between successive steps of the type scale. 1.125,
#'   the major second, is the default: at 5-8 pt a larger ratio produces jumps
#'   that read as inconsistency rather than hierarchy.
#' @param family Typeface. Defaults to the first of the venue's permitted
#'   families that is installed.
#' @param ink Ink for text and axes. Defaults to pure black, which is deliberate:
#'   a near-black separates into a four-colour rich black in CMYK, and small text
#'   set in it prints soft and misregistered. On a dark surface it becomes a near
#'   white instead.
#' @param accent Accent colour for reference lines, highlights, and annotation.
#' @param palette Default discrete palette name, carried by the theme.
#' @param density `"comfortable"` (default), `"compact"` for dense multi-panel
#'   plates, or `"airy"` for slides.
#' @param dark Invert the surface: light ink on a dark ground, for a talk given on a
#'   dark background. Deliberately independent of `venue` -- a slide is a screen
#'   destination but most talks are given on white, so the background is a separate
#'   decision from the type sizes. [theme_vellum_dark()] sets it.
#'
#' @return An object of class `vellum_tokens`: a list with `type`, `ink`, `rule`,
#'   `space`, `geom`, and `meta` components.
#'
#' @examples
#' vellum_tokens("nature")
#' vellum_tokens("slide")$type$size
#' @export
vellum_tokens <- function(venue = "nature", base_size = NULL, scale_ratio = 1.125,
                          family = NULL, ink = NULL, accent = NULL,
                          palette = "okabe_ito",
                          density = c("comfortable", "compact", "airy"),
                          dark = FALSE) {
  density <- match.arg(density)
  check_bool(dark, "dark")
  spec <- venue_spec(venue)
  base_size <- base_size %||% spec$text_max
  check_number(base_size, "base_size", min = 1)
  check_number(scale_ratio, "scale_ratio", min = 1, max = 2)

  family <- family %||% as.character(resolve_font(spec$families))
  mono <- as.character(resolve_font(c("Courier", "Courier New", "Nimbus Mono PS", "mono"), "mono"))

  # A slide or poster is a screen destination, but that does NOT make it a dark
  # one: most talks are given on white. Tying the background to the venue was a bug
  # -- it produced dark ink on dark paper for `theme_vellum_slide()`. Inversion is
  # now an explicit choice, made by `theme_vellum_dark()`.
  screen <- spec$name %in% c("slide", "poster")
  ink <- ink %||% (if (dark) "#F2F2F2" else "#000000")
  accent <- accent %||% "#D55E00"

  # Type scale. One step down for secondary text, two for the smallest labels,
  # everything clamped into the venue's permitted range.
  step <- function(k) base_size / scale_ratio^k
  fit <- function(x) clamp(round(x, 2), spec$text_min, spec$text_max)
  size <- list(
    axis_title  = fit(step(0)),
    axis_text   = fit(step(1)),
    strip       = fit(step(0)),
    legend_title= fit(step(1)),
    legend_text = fit(step(1)),
    annot       = fit(step(1)),
    small       = fit(step(2)),
    title       = fit(step(-1)),
    tag         = spec$tag_size
  )

  # Everything physical scales against the venue's own type size, so a token set
  # for a poster is not a print figure enlarged: its rules and marks grow too.
  k <- base_size / 7
  dens <- switch(density, compact = 0.72, comfortable = 1, airy = 1.35)

  rule_pt <- list(
    axis     = 0.5 * k,
    tick     = 0.5 * k,
    grid     = 0.3 * k,
    border   = 0.5 * k,
    geom     = 0.6 * k,
    hairline = max(0.25, 0.25 * k),   # never thinner than the 0.25 pt print floor
    emphasis = 1.0 * k
  )

  structure(list(
    type = list(
      family = family, family_mono = mono, size = size,
      lineheight = 1.15, base_size = base_size, scale_ratio = scale_ratio
    ),
    ink = list(
      text = ink,
      axis = ink,
      tick = ink,
      # The grid must be visible enough to read a value against and faint enough
      # to disappear when you are not.
      grid = if (dark) "#3A3A3E" else "#DCDCDC",
      grid_minor = if (dark) "#2A2A2E" else "#EDEDED",
      # #767676 is the lightest grey that still reaches WCAG 4.5:1 on white, so it
      # is the floor for any text that must actually be read.
      weak = if (dark) "#9A9AA0" else "#767676",
      data = ink,
      paper = if (dark) "#101012" else "#FFFFFF",
      panel = "transparent",
      accent = accent,
      na = if (dark) "#4A4A52" else pal_na(palette)
    ),
    rule = list(
      axis     = lw(rule_pt$axis),
      tick     = lw(rule_pt$tick),
      grid     = lw(rule_pt$grid),
      border   = lw(rule_pt$border),
      geom     = lw(rule_pt$geom),
      hairline = lw(rule_pt$hairline),
      emphasis = lw(rule_pt$emphasis),
      pt = rule_pt
    ),
    space = list(
      tick_len      = u_mm(0.9 * k * dens),
      panel_spacing = u_mm(2.6 * k * dens),
      legend_key    = u_mm(3.0 * k),
      legend_gap    = u_mm(1.2 * k * dens),
      legend_margin = u_mm(0.8 * k * dens),
      plot_margin   = u_mm(1.0 * k * dens),
      tag_gap       = u_mm(0.8 * k),
      title_gap     = u_mm(1.2 * k * dens)
    ),
    geom = list(
      point       = ps(1.8 * k),
      point_small = ps(1.1 * k),
      point_large = ps(2.6 * k),
      stroke      = lw(0.4 * k),
      text        = fs(size$annot),
      interval    = lw(rule_pt$geom),
      interval_in = lw(rule_pt$emphasis * 1.4),
      alpha       = 0.85,
      alpha_light = 0.20
    ),
    meta = list(venue = spec$name, spec = spec, palette = palette,
                density = density, screen = screen, dark = dark)
  ), class = c("vellum_tokens", "list"))
}

#' @export
print.vellum_tokens <- function(x, ...) {
  cli::cli_h1("vellum tokens \u00b7 {x$meta$venue} \u00b7 {x$meta$density}")
  s <- x$type$size
  cli::cli_text("{.strong Type} {x$type$family} \u00b7 axis title {s$axis_title} pt \u00b7 axis text {s$axis_text} pt \u00b7 panel tag {s$tag} pt")
  cli::cli_text("{.strong Rules} axis {sprintf('%.2f', x$rule$pt$axis)} pt \u00b7 grid {sprintf('%.2f', x$rule$pt$grid)} pt \u00b7 data {sprintf('%.2f', x$rule$pt$geom)} pt \u00b7 hairline floor {sprintf('%.2f', x$rule$pt$hairline)} pt")
  cli::cli_text("{.strong Marks} point {sprintf('%.2f', x$geom$point)} mm \u00b7 tick {sprintf('%.2f', as_mm(x$space$tick_len))} mm \u00b7 panel gap {sprintf('%.2f', as_mm(x$space$panel_spacing))} mm")
  cli::cli_text("{.strong Ink} text {x$ink$text} \u00b7 grid {x$ink$grid} \u00b7 weak {x$ink$weak} \u00b7 accent {x$ink$accent}")
  cli::cli_text("{.strong Palette} {x$meta$palette}")
  invisible(x)
}
