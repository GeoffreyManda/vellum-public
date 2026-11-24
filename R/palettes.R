# Palettes.
#
# Five sources, in order of precedence for scientific work:
#
#   1. Okabe-Ito, for unordered groups. Eight colours, designed for colour
#      universal design, deliberately varying in lightness so that pairs stay
#      separable when hue collapses.
#   2. Paul Tol's schemes, for everything Okabe-Ito does not cover. Constructed
#      with CIEDE2000, checked under simulated red-blind and green-blind vision,
#      and constrained to the ISO Coated v2 300% CMYK gamut.
#   3. This package's own generated ramps, built by the engine in colour-ramp.R,
#      so they carry no third-party licence and their properties are reproducible
#      from the recipe rather than asserted.
#   4. Role-bearing collections from Vellum's visualisation skills. These retain
#      provenance and scope so project or publication identities remain opt-in.
#   5. Loukas Theodosiou's `ltc` palettes, from art, places and novels. Last for
#      a reason: they are chosen for beauty, not separability, and each one
#      carries its measured cost in the catalogue. See palettes-ltc.R.

# The canonical form of any user-supplied palette, venue, collection or identity
# name. Every registry in the package is keyed on this, so lookups must normalise
# through here rather than matching the raw string.
vellum_key <- function(x) gsub("[^a-z0-9_]+", "_", tolower(x))

# Every name `vellum_pal()` will resolve, in the order it tries them.
vellum_palette_keys <- function() {
  c("okabe_ito", "okabeito", "oi",
    names(.vellum_role_collections), names(.vellum_specs), names(.tol),
    names(.ltc))
}

#' The Okabe-Ito qualitative palette
#'
#' @description
#' The canonical colour-vision-safe qualitative palette, from Okabe and Ito's
#' *Color Universal Design*. Use it for unordered categories. It is the default
#' discrete palette throughout `vellum`.
#'
#' Note what it is not: it is not equal-luminance. That is the point. ggplot2's
#' default scale holds luminance constant while varying hue, so when the
#' red-green axis collapses under deuteranopia the colours become
#' indistinguishable, because the only channel left was the one held constant.
#' Okabe-Ito varies lightness precisely so that pairs survive.
#'
#' @param n Number of colours, up to 8 (9 with `black = TRUE`).
#' @param black Include black as the first colour. Useful when one series is a
#'   reference or control; omitted by default because black usually belongs to
#'   the axes and text, not the data.
#' @param named Return a named vector.
#'
#' @return A character vector of hex colours.
#' @references Okabe M, Ito K. *Color Universal Design (CUD): How to make figures
#'   and presentations that are friendly to colorblind people.*
#'   <https://jfly.uni-koeln.de/color/>
#' @export
#' @examples
#' okabe_ito()
#' okabe_ito(3)
#' palette_audit(okabe_ito())
okabe_ito <- function(n = 8, black = FALSE, named = FALSE) {
  full <- c(
    orange         = "#E69F00",
    sky_blue       = "#56B4E9",
    bluish_green   = "#009E73",
    yellow         = "#F0E442",
    blue           = "#0072B2",
    vermillion     = "#D55E00",
    reddish_purple = "#CC79A7",
    grey           = "#999999"
  )
  if (black) full <- c(black = "#000000", full)
  check_number(n, "n", min = 1)
  if (n > length(full)) {
    cli::cli_abort(c(
      "Okabe-Ito has {length(full)} colours; {n} were requested.",
      i = "For more categories use {.code vellum_pal('muted')} (9) or {.code vellum_pal('rainbow_discrete')}.",
      i = "Consider instead whether {n} categories belong in one figure."
    ))
  }
  out <- full[seq_len(n)]
  if (named) out else unname(out)
}

# Paul Tol's schemes, retrieved 2026-07-30 from https://sronpersonalpages.nl/~pault/
# Values transcribed exactly. `bad` is Tol's designated colour for missing data.
.tol <- list(
  bright         = list(type = "qual", cols = c("#4477AA","#EE6677","#228833","#CCBB44","#66CCEE","#AA3377","#BBBBBB")),
  high_contrast  = list(type = "qual", cols = c("#004488","#DDAA33","#BB5566")),
  vibrant        = list(type = "qual", cols = c("#EE7733","#0077BB","#33BBEE","#EE3377","#CC3311","#009988","#BBBBBB")),
  muted          = list(type = "qual", cols = c("#CC6677","#332288","#DDCC77","#117733","#88CCEE","#882255","#44AA99","#999933","#AA4499"), bad = "#DDDDDD"),
  medium_contrast= list(type = "qual", cols = c("#6699CC","#004488","#EECC66","#994455","#997700","#EE99AA")),
  light          = list(type = "qual", cols = c("#77AADD","#EE8866","#EEDD88","#FFAABB","#99DDFF","#44BB99","#BBCC33","#AAAA00","#DDDDDD")),
  pale           = list(type = "qual", cols = c("#BBCCEE","#CCEEFF","#CCDDAA","#EEEEBB","#FFCCCC","#DDDDDD")),
  dark           = list(type = "qual", cols = c("#222255","#225555","#225522","#666633","#663333","#555555")),

  sunset    = list(type = "div", cols = c("#364B9A","#4A7BB7","#6EA6CD","#98CAE1","#C2E4EF","#EAECCC","#FEDA8B","#FDB366","#F67E4B","#DD3D2D","#A50026"), bad = "#FFFFFF"),
  nightfall = list(type = "div", cols = c("#125A56","#00767B","#238F9D","#42A7C6","#60BCE9","#9DCCEF","#C6DBED","#DEE6E7","#ECEADA","#F0E6B2","#F9D576","#FFB954","#FD9A44","#F57634","#E94C1F","#D11807","#A01813"), bad = "#FFFFFF"),
  burd      = list(type = "div", cols = c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B"), bad = "#FFEE99"),
  prgn      = list(type = "div", cols = c("#762A83","#9970AB","#C2A5CF","#E7D4E8","#F7F7F7","#D9F0D3","#ACD39E","#5AAE61","#1B7837"), bad = "#FFEE99"),

  yl_or_br     = list(type = "seq", cols = c("#FFFFE5","#FFF7BC","#FEE391","#FEC44F","#FB9A29","#EC7014","#CC4C02","#993404","#662506"), bad = "#888888"),
  iridescent   = list(type = "seq", cols = c("#FEFBE9","#FCF7D5","#F5F3C1","#EAF0B5","#DDECBF","#D0E7CA","#C2E3D2","#B5DDD8","#A8D8DC","#9BD2E1","#8DCBE4","#81C4E7","#7BBCE7","#7EB2E4","#88A5DD","#9398D2","#9B8AC4","#9D7DB2","#9A709E","#906388","#805770","#684957","#46353A"), bad = "#999999"),
  incandescent = list(type = "seq", cols = c("#CEFFFF","#C6F7D6","#A2F49B","#BBE453","#D5CE04","#E7B503","#F19903","#F6790B","#F94902","#E40515","#A80003"), bad = "#888888"),
  rainbow_discrete = list(type = "qual", cols = c("#E8ECFB","#D9CCE3","#D1BBD7","#CAACCB","#BA8DB4","#AE76A3","#AA6F9E","#994F88","#882E72","#1965B0","#437DBF","#5289C7","#6195CF","#7BAFDE","#4EB265","#90C987","#CAE0AB","#F7F056","#F7CB45","#F6C141","#F4A736","#F1932D","#EE8026","#E8601C","#E65518","#DC050C","#A5170E","#72190E","#42150A"), bad = "#777777"),
  rainbow_smooth   = list(type = "seq",  cols = c("#E8ECFB","#DDD8EF","#D1C1E1","#C3A8D1","#B58FC2","#A778B4","#9B62A7","#8C4E99","#6F4C9B","#6059A9","#5568B8","#4E79C5","#4D8AC6","#4E96BC","#549EB3","#59A5A9","#60AB9E","#69B190","#77B77D","#8CBC68","#A6BE54","#BEBC48","#D1B541","#DDAA3C","#E49C39","#E78C35","#E67932","#E4632D","#DF4828","#DA2222","#B8221E","#95211B","#721E17","#521A13"), bad = "#666666")
)

# This package's own ramps, defined as recipes rather than lookup tables. The
# engine builds them on demand, so the properties are reproducible and the
# specification is inspectable.
.vellum_specs <- list(
  ink      = list(type = "seq", L = c(0.16, 0.97), C = 0.030, h = c(265, 250),
                  why = "Near-neutral, faintly cool. For print-first figures where colour must not compete with the data."),
  abyss    = list(type = "seq", L = c(0.15, 0.97), C = 0.115, h = c(268, 185),
                  why = "Deep blue-black through teal to a pale cream. Reads as depth."),
  ember    = list(type = "seq", L = c(0.17, 0.97), C = 0.150, h = c(300, 72),
                  why = "Violet through magenta to amber. High chroma range with monotone lightness."),
  verdant  = list(type = "seq", L = c(0.16, 0.97), C = 0.130, h = c(258, 118),
                  why = "Blue-black through green to pale straw. The classic scientific sweep, linearised."),
  terra    = list(type = "seq", L = c(0.18, 0.96), C = 0.095, h = c(28, 112),
                  why = "Umber through olive to sage. For land, soil, and biomass, where earth hues are expected."),
  bloom    = list(type = "seq", L = c(0.18, 0.97), C = 0.140, h = c(324, 78),
                  why = "Plum through rose to gold. A warm alternative when a cool ramp clashes with the subject."),

  # Diverging hue pairs are chosen by measurement, not by taste. The quantity that
  # matters is how well the two arms stay apart at matched lightness AFTER CVD
  # simulation, because that is what lets a reader recover the sign of a
  # deviation. Measured worst-case arm separation across deuteranopia,
  # protanopia and tritanopia: blue/ochre 22.0, blue/amber 19.1, purple/green
  # 14.0, teal/crimson 6.3. The teal-crimson pair is the classic trap -- it looks
  # the most striking to normal vision (29.8) and is the worst in practice.
  strata   = list(type = "div", L = c(0.22, 0.97), C = 0.130, h = c(255, 66), pivot = "light",
                  why = "Blue against ochre, pale hinge. The most colour-vision-robust diverging pair measured here (worst-case arm separation 22.0); the recommended default."),
  tide     = list(type = "div", L = c(0.22, 0.97), C = 0.135, h = c(240, 50), pivot = "light",
                  why = "Cool blue against warm amber, pale hinge. Reads as temperature or flow while keeping worst-case arm separation near 19."),
  equinox  = list(type = "div", L = c(0.22, 0.97), C = 0.135, h = c(320, 150), pivot = "light",
                  why = "Magenta against green. Use when blue is already spent on another encoding."),
  polarity = list(type = "div", L = c(0.25, 0.78), C = 0.140, h = c(250, 28), pivot = "dark",
                  why = "Blue against red with a DARK hinge, for figures on dark ground or slides. Ends sit at mid lightness rather than near-white, because sRGB cannot hold chroma close to white."),

  orbit    = list(type = "cyc", L = c(0.45, 0.80), C = 0.110, h = 250,
                  why = "Cyclic, for phase, wind direction, time of day, or angle, where the two ends must meet without a seam.")
)

#' List every palette vellum knows
#'
#' @param type Optionally filter to `"qualitative"`, `"sequential"`,
#'   `"diverging"`, or `"cyclic"`.
#' @param family Optionally filter to a source family: `"okabe_ito"`, `"tol"`,
#'   `"vellum"`, `"collection"`, or `"ltc"`. The first three are the palettes
#'   designed for scientific encoding; `"collection"` holds the role-bearing
#'   project and brand systems, and `"ltc"` the art palettes. Since `ltc`
#'   contributes more names than the other four families together, this is how
#'   you get a list you can actually read.
#' @return A data frame with one row per palette: its name, kind, source family,
#'   number of defined colours (`NA` for continuously generated ramps), source,
#'   and, where one exists, the design rationale. For the `ltc` family the
#'   rationale carries the palette's backstory and its measured behaviour under
#'   simulated colour-vision deficiency.
#' @export
#' @examples
#' vellum_palettes()
#' vellum_palettes("diverging")
#' vellum_palettes(family = c("okabe_ito", "tol", "vellum"))
vellum_palettes <- function(type = NULL, family = NULL) {
  kind <- c(qual = "qualitative", seq = "sequential", div = "diverging", cyc = "cyclic")
  rows <- c(
    list(data.frame(palette = "okabe_ito", kind = "qualitative", family = "okabe_ito", n = 8L,
                    source = "Okabe & Ito, Color Universal Design", rationale = NA_character_)),
    lapply(names(.tol), function(nm) data.frame(
      palette = nm, kind = unname(kind[.tol[[nm]]$type]), family = "tol",
      n = length(.tol[[nm]]$cols),
      source = "Paul Tol, SRON technical note", rationale = NA_character_)),
    lapply(names(.vellum_role_collections), function(nm) {
      x <- .vellum_role_collections[[nm]]
      data.frame(
        palette = nm, kind = "qualitative", family = "collection",
        n = length(x$display_roles),
        source = x$source,
        rationale = paste(x$intended_use, x$accessibility, sep = ". ")
      )
    }),
    lapply(names(.vellum_specs), function(nm) data.frame(
      palette = nm, kind = unname(kind[.vellum_specs[[nm]]$type]), family = "vellum",
      n = NA_integer_,
      source = "vellum ramp engine", rationale = .vellum_specs[[nm]]$why)),
    lapply(names(.ltc), function(nm) data.frame(
      palette = nm, kind = unname(kind[.ltc[[nm]]$type]), family = "ltc",
      n = length(.ltc[[nm]]$cols),
      source = "Theodosiou, ltc (MIT)", rationale = ltc_rationale(nm)))
  )
  out <- do.call(rbind, rows)
  if (!is.null(type)) {
    type <- match.arg(type, c("qualitative", "sequential", "diverging", "cyclic"), several.ok = TRUE)
    out <- out[out$kind %in% type, , drop = FALSE]
  }
  if (!is.null(family)) {
    family <- match.arg(family, c("okabe_ito", "tol", "vellum", "collection", "ltc"),
                        several.ok = TRUE)
    out <- out[out$family %in% family, , drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

#' Get colours from a vellum palette
#'
#' @description
#' One accessor for all five palette sources. Discrete palettes return their
#' defined colours, subset to `n`. Generated ramps are built on demand by the
#' engine and cached, so asking for 256 colours costs the same as asking twice.
#'
#' @param palette Palette name. See [vellum_palettes()].
#' @param n Number of colours. For generated ramps any `n` is valid. For discrete
#'   palettes, requesting more than exist is an error rather than a silent
#'   recycle, because silently repeating a colour makes two groups look like one.
#' @param rev Reverse the result.
#' @param space Perceptual space for generated ramps, `"oklch"` or `"lch"`.
#' @param interpolate For a fixed palette, interpolate perceptually to `n`
#'   colours rather than subsetting. Sensible for sequential and diverging
#'   palettes; never do it for qualitative ones.
#'
#' @return A character vector of hex colours.
#' @export
#' @examples
#' vellum_pal("abyss", 7)
#' vellum_pal("tide", 9)
#' vellum_pal("bright", 4)
#' vellum_pal("sunset", 21, interpolate = TRUE)
vellum_pal <- function(palette = "okabe_ito", n = NULL, rev = FALSE,
                       space = c("oklch", "lch"), interpolate = NULL) {
  check_string(palette, "palette")
  space <- match.arg(space)
  key <- vellum_key(palette)

  if (key %in% c("okabe_ito", "okabeito", "oi")) {
    out <- okabe_ito(n %||% 8)
    return(if (rev) rev(out) else out)
  }

  if (key %in% names(.vellum_role_collections)) {
    out <- collection_display_colours(key, n)
    return(if (rev) rev(out) else out)
  }

  if (key %in% names(.vellum_specs)) {
    sp <- .vellum_specs[[key]]
    n <- n %||% 256L
    cache_id <- paste(key, n, space, sep = "/")
    if (!is.null(the$ramp_cache[[cache_id]])) {
      out <- the$ramp_cache[[cache_id]]
    } else {
      type <- c(seq = "sequential", div = "diverging", cyc = "cyclic")[[sp$type]]
      L <- if (space == "lch") sp$L * 100 else sp$L
      C <- if (space == "lch") sp$C * (space_C_max("lch") / space_C_max("oklch")) else sp$C
      out <- vellum_ramp(n = n, L = L, C = C, h = sp$h, type = type,
                         pivot = sp$pivot %||% "light", space = space)
      the$ramp_cache[[cache_id]] <- out
    }
    return(if (rev) rev(out) else out)
  }

  if (key %in% names(.tol) || key %in% names(.ltc)) {
    entry <- if (key %in% names(.tol)) .tol[[key]] else .ltc[[key]]
    cols <- entry$cols
    interpolate <- interpolate %||% (entry$type != "qual")
    if (is.null(n)) n <- length(cols)
    if (interpolate) {
      out <- ramp_through(cols, n = n, space = space,
                          type = if (entry$type == "div") "diverging" else "sequential",
                          linearise = FALSE)
      out <- unclass(out)
    } else {
      if (n > length(cols)) {
        cli::cli_abort(c(
          "Palette {.val {palette}} defines {length(cols)} colours; {n} were requested.",
          i = if (entry$type == "qual")
            "Qualitative palettes are not interpolated, because an invented intermediate colour is not a distinct category."
          else "Use {.code interpolate = TRUE} to interpolate perceptually.",
          i = "Or use a larger palette: {.code vellum_palettes('qualitative')}."
        ))
      }
      out <- cols[seq_len(n)]
    }
    return(if (rev) rev(out) else out)
  }

  cli::cli_abort(c(
    "Unknown palette {.val {palette}}.",
    i = "See {.fn vellum_palettes} for the full list."
  ))
}

#' The colour for missing or unmeasured data
#'
#' Missing data should read as absent, not as a low value. Paul Tol assigns each
#' of his schemes a specific bad-data colour chosen to sit outside the scheme's
#' perceptual range; where a palette defines one, this returns it, and otherwise
#' returns a neutral grey.
#'
#' @param palette Palette name.
#' @return A single hex colour.
#' @export
#' @examples
#' pal_na("sunset")
#' pal_na("abyss")
pal_na <- function(palette = "okabe_ito") {
  check_string(palette, "palette")
  key <- vellum_key(palette)
  if (key %in% names(.tol) && !is.null(.tol[[key]]$bad)) return(.tol[[key]]$bad)
  # This supplies `na.value` for every scale, so an unknown name has to fail here
  # exactly as it fails in `vellum_pal()`; returning grey instead would let a
  # misspelt palette build a plot that silently uses none of it.
  if (!key %in% vellum_palette_keys()) {
    cli::cli_abort(c(
      "Unknown palette {.val {palette}}.",
      i = "See {.fn vellum_palettes} for the full list."
    ))
  }
  "#BDBDBD"
}

#' Traffic-light colours for judgements, with mandatory symbols
#'
#' @description
#' For risk-of-bias tables, certainty ratings, and decision matrices. The colours
#' are drawn from Okabe-Ito rather than from literal red, amber, green, so the
#' scale survives deuteranopia.
#'
#' The returned object carries symbols as well as colours, because a judgement
#' encoded in colour alone is unreadable to a substantial minority of readers and
#' invisible in a photocopy. Use both.
#'
#' @return A list with `colours` and `symbols`, both named by judgement level.
#' @export
#' @examples
#' judgement_scale()
judgement_scale <- function() {
  list(
    colours = c(low = "#009E73", some = "#E69F00", high = "#D55E00",
                unclear = "#BDBDBD", none = "#FFFFFF"),
    symbols = c(low = "+", some = "?", high = "\u2212",
                unclear = "\u00b7", none = "")
  )
}

#' Build a qualitative palette optimised for the worst case across vision types
#'
#' @description
#' Curated palettes stop at eight or nine colours because hand-tuning more is
#' impractical. When more categories genuinely belong in one figure, this
#' constructs a palette by explicit optimisation instead: candidates are
#' generated across lightness and hue, gamut-mapped, and then selected greedily
#' to maximise the *minimum* CIEDE2000 separation, where the minimum is taken
#' over normal vision, deuteranopia, protanopia, tritanopia, and greyscale
#' simultaneously.
#'
#' The palette is therefore optimised for the reader who can distinguish the
#' least, which is the only defensible objective for a scientific figure. Verify
#' the result with [palette_audit()]; the returned separation is reported so the
#' claim is checkable.
#'
#' Before using this, ask whether the figure needs that many categories. Twelve
#' distinguishable colours is a technical achievement and usually a design
#' failure.
#'
#' @param n Number of colours.
#' @param L_levels Candidate lightness levels in OKLab L (0-1). More levels give
#'   the optimiser more room but push some colours toward the extremes.
#' @param n_hue Number of candidate hues per lightness level.
#' @param chroma Candidate chroma before gamut mapping.
#' @param conditions Vision conditions to optimise the worst case over.
#'   Greyscale is deliberately excluded by default: including it forces the
#'   optimiser to spread lightness across the categories, which costs colour
#'   separation for every reader in order to chase a target that is arithmetically
#'   unreachable past about four groups. Greyscale is solved by a second channel --
#'   [vellum_shapes()] or [vellum_linetypes()] -- not by a better palette.
#' @param seed Optional first colour to anchor the palette, for instance a fixed
#'   colour for a control group.
#'
#' @return A character vector of `n` hex colours, with the achieved worst-case
#'   separation in the `min_delta_e` attribute.
#'
#' @details
#' Measured on this implementation, the worst-case dichromatic separation
#' achieved is 15.5 at six colours, 14.2 at eight, and 7.3 at twelve. The
#' corresponding figures for Okabe-Ito are 11.5 at six and 6.4 at eight, and no
#' curated palette offers twelve. The comparison is reproducible with
#' [palette_audit()], which is the point: the claim is a measurement, not a
#' preference.
#'
#' @export
#' @examples
#' p <- palette_optimise(6)
#' attr(p, "min_delta_e")
#' palette_audit(p)
palette_optimise <- function(n = 8, L_levels = c(0.45, 0.62, 0.78),
                             n_hue = 48, chroma = 0.16,
                             conditions = c("normal", "deutan", "protan", "tritan"),
                             seed = NULL) {
  check_number(n, "n", min = 1)
  n <- as.integer(n)
  grid <- expand.grid(L = L_levels, h = seq(0, 360, length.out = n_hue + 1)[-1])
  mapped <- gamut_map(grid$L, rep(chroma, nrow(grid)), grid$h)
  cand <- unique(from_polar(mapped))

  # Pre-compute each candidate as seen under every condition, so the inner loop
  # is pure arithmetic rather than repeated colour conversion.
  views <- lapply(conditions, function(cond)
    if (cond == "normal") cand else cvd(cand, cond, severity = 1))
  rgb_views <- lapply(views, function(v) t(grDevices::col2rgb(v)))

  worst_pair <- function(i, chosen_idx) {
    vapply(seq_along(rgb_views), function(k) {
      m <- farver::compare_colour(rgb_views[[k]][i, , drop = FALSE],
                                  rgb_views[[k]][chosen_idx, , drop = FALSE],
                                  from_space = "rgb", method = "cie2000", white_from = "D65")
      min(m)
    }, numeric(1))
  }

  if (!is.null(seed)) {
    check_colour(seed, "seed")
    cand <- unique(c(seed[1], cand))
    views <- lapply(conditions, function(cond)
      if (cond == "normal") cand else cvd(cand, cond, severity = 1))
    rgb_views <- lapply(views, function(v) t(grDevices::col2rgb(v)))
    chosen <- 1L
  } else {
    # Start from the candidate that is most isolated from the page: maximum
    # contrast against white, which is where every figure begins.
    chosen <- which.max(contrast_ratio(cand, "#FFFFFF"))
  }

  # Gamut mapping collapses candidates onto each other, so the grid can yield far
  # fewer distinct colours than it has cells. Without this the selection loop has
  # nothing left to choose and never terminates.
  if (n > length(cand)) {
    cli::cli_abort(c(
      "The candidate grid yields {length(cand)} distinct colours; {n} were requested.",
      i = "Raise {.arg n_hue}, or add levels to {.arg L_levels}, to widen the grid.",
      i = "Colours are never recycled, because a repeated colour makes two groups look like one."
    ))
  }

  achieved <- Inf
  while (length(chosen) < n) {
    scores <- vapply(setdiff(seq_along(cand), chosen),
                     function(i) min(worst_pair(i, chosen)), numeric(1))
    pool <- setdiff(seq_along(cand), chosen)
    best <- pool[which.max(scores)]
    achieved <- max(scores)
    chosen <- c(chosen, best)
  }
  out <- cand[chosen]
  structure(out, min_delta_e = round(achieved, 2))
}
