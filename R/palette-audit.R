# Palette auditing.
#
# A palette that cannot be audited should not be published. Everything a
# scientific palette claims about itself is measurable, so this measures it and
# reports numbers rather than reassurance.

# Thresholds are stated here, in one place, rather than buried in the checks.
# They are design decisions, so they are open to argument and to override.
.audit_thresholds <- list(
  # Minimum pairwise CIEDE2000 for a qualitative palette. About 1 is the
  # just-noticeable difference in ideal side-by-side viewing; small marks on a
  # busy page, seen non-adjacently, need far more. 15 is a workable floor for
  # confident discrimination and 25 is comfortable.
  qual_min_pass = 15,
  qual_min_good = 25,
  # The same, after simulating dichromacy. A palette that separates well under
  # normal vision and collapses under deuteranopia is the single most common
  # accessibility failure in published figures.
  #
  # These two numbers are calibrated against the field standard rather than
  # chosen for severity. Measured here, Okabe-Ito's worst-case dichromatic
  # separation is 50.6 at two colours, 11.5 at four through six, 11.1 at seven,
  # and 6.4 at all eight. A threshold above about 12 would therefore mark the
  # recommended colour-universal-design palette as failing, which would make the
  # audit useless. 8 is set where colours genuinely begin to collide, and 12
  # marks Okabe-Ito-grade separation.
  qual_cvd_pass = 8,
  qual_cvd_good = 12,
  # Sequential and diverging ramps must span enough lightness to survive
  # greyscale. Below about 40 L* of range the ramp becomes a flat wash when
  # photocopied.
  L_range_pass = 40,
  L_range_good = 55,
  # Deviation from linear lightness, as a fraction of the ramp's own L* range.
  L_linear_pass = 0.10,
  L_linear_good = 0.05,
  # Contrast of the palette's darkest colour against white, for readability of
  # thin marks. WCAG 2.1 sets 3.0 for graphical objects.
  contrast_pass = 3.0,
  # Coefficient of variation of the CIEDE2000 distance between consecutive
  # colours of an ordered ramp. This is perceptual uniformity measured directly:
  # 0 would mean every step is exactly equal. Below about 0.20 the steps read as
  # even; above about 0.45 the ramp visibly crowds in places and stretches in
  # others, which is what makes a value hard to read off a colour bar.
  uniform_pass = 0.45,
  uniform_good = 0.20
)

# CIEDE2000 distance between each consecutive pair of an ordered ramp.
adjacent_de <- function(cols) {
  if (length(cols) < 2L) return(numeric(0))
  a <- t(grDevices::col2rgb(cols[-length(cols)]))
  b <- t(grDevices::col2rgb(cols[-1]))
  vapply(seq_len(nrow(a)), function(i) {
    farver::compare_colour(a[i, , drop = FALSE], b[i, , drop = FALSE],
                           from_space = "rgb", method = "cie2000", white_from = "D65")[1, 1]
  }, numeric(1))
}

# For a diverging ramp: the smallest colour difference between positions that are
# mirror images about the hinge. These are the pairs a reader must tell apart to
# recover the sign of a deviation.
arm_separation <- function(cols) {
  n <- length(cols)
  if (n < 3L) return(Inf)
  k <- floor(n / 2)
  lo <- cols[seq_len(k)]
  hi <- rev(cols)[seq_len(k)]
  min(vapply(seq_len(k), function(i) {
    farver::compare_colour(t(grDevices::col2rgb(lo[i])), t(grDevices::col2rgb(hi[i])),
                           from_space = "rgb", method = "cie2000", white_from = "D65")[1, 1]
  }, numeric(1)))
}

# Infer whether a palette is qualitative, sequential, or diverging from its
# lightness profile alone.
#
# The discriminator for diverging is not "the extreme is near the middle" -- many
# qualitative palettes happen to have their lightest colour in the middle of the
# vector. It is that BOTH arms are monotone in lightness, which is the defining
# structural property of a diverging scale and is not satisfied by an arbitrary
# ordering of unrelated hues.
infer_palette_type <- function(Lv) {
  n <- length(Lv)
  if (n < 4L) return("qualitative")
  if (is_monotone(Lv, tol = 2)) return("sequential")
  half <- ceiling(n / 2)
  arm_a <- Lv[seq_len(half)]
  arm_b <- rev(Lv[seq(n - half + 1, n)])
  ends_similar <- abs(Lv[1] - Lv[n]) <= 20
  if (is_monotone(arm_a, tol = 2) && is_monotone(arm_b, tol = 2) && ends_similar) {
    return("diverging")
  }
  "qualitative"
}

#' Largest prefix of a palette that stays separable
#'
#' @description
#' Curated palettes degrade as you use more of them, and the point at which they
#' degrade is measurable rather than a matter of opinion. This returns the largest
#' number of leading colours whose worst-case dichromatic separation still meets
#' `threshold`.
#'
#' It answers the question people actually have: *how many of these can I use?*
#' For Okabe-Ito the answer is seven -- the eighth colour, grey, collides with the
#' reddish purple under deuteranopia at a separation of 6.4.
#'
#' @param palette A palette name or a character vector of colours.
#' @param threshold Minimum acceptable worst-case CIEDE2000 separation.
#' @param conditions Vision conditions to take the worst case over.
#' @return An integer: the largest safe number of leading colours, with the
#'   achieved separation in the `separation` attribute.
#' @export
#' @examples
#' palette_max_safe("okabe_ito")
#' palette_max_safe("bright")
palette_max_safe <- function(palette, threshold = 10,
                             conditions = c("deutan", "protan", "tritan")) {
  cols <- if (length(palette) == 1L && palette %in% vellum_palettes()$palette) {
    vellum_pal(palette)
  } else palette
  check_colour(cols, "palette")
  # `seq(2L, 1L)` counts down, which would index past the end of a one-colour
  # palette and measure the separation against an NA. A single colour has no pair.
  if (length(cols) < 2L) return(structure(1L, separation = Inf))
  best <- 1L
  achieved <- Inf
  for (n in seq(2L, length(cols))) {
    sub <- cols[seq_len(n)]
    s <- min(vapply(conditions, function(cd) min_separation(cvd(sub, cd)), numeric(1)))
    if (s < threshold) break
    best <- n
    achieved <- s
  }
  structure(best, separation = round(achieved, 1))
}

#' Audit a palette
#'
#' @description
#' Measures every property a scientific palette implicitly claims, and reports
#' the numbers:
#'
#' * minimum pairwise CIEDE2000 separation under normal vision, deuteranopia,
#'   protanopia, tritanopia, and luminance-correct greyscale;
#' * CIE L\* range, and whether lightness is monotone;
#' * for ordered palettes, the maximum deviation from linear lightness, which is
#'   what decides whether equal data steps look like equal steps;
#' * contrast of the darkest and lightest colours against white;
#' * which pair is the weakest link, by name.
#'
#' The verdict is `"pass"`, `"warn"`, or `"fail"` against thresholds that are
#' documented and overridable rather than hidden. `print()` gives a readable
#' report; `plot()` gives a diagnostic figure showing the palette as each reader
#' would see it.
#'
#' @param colours A character vector of colours, or a palette name.
#' @param type `"qualitative"`, `"sequential"`, `"diverging"`, or `"auto"` to
#'   infer from the lightness profile.
#' @param n When `colours` is a palette name, how many colours to draw.
#' @param severity CVD severity to audit at, 0 to 1. Auditing at `1` (dichromacy)
#'   is the conservative choice and the default.
#' @param thresholds Optionally override the pass thresholds; see the source of
#'   `vellum:::.audit_thresholds` for the defaults and their justification.
#'
#' @return An object of class `vellum_audit`.
#'
#' @examples
#' palette_audit(okabe_ito())
#' palette_audit("abyss", n = 9)
#'
#' # The failure this exists to catch: ggplot2's default three-group scale
#' # separates well until the red-green axis collapses.
#' palette_audit(c("#F8766D", "#00BA38", "#619CFF"))
#' @export
palette_audit <- function(colours, type = c("auto", "qualitative", "sequential", "diverging"),
                          n = NULL, severity = 1, thresholds = NULL) {
  type <- match.arg(type)
  if (length(colours) == 1L && !grepl("^#", colours) &&
      inherits(try(grDevices::col2rgb(colours), silent = TRUE), "try-error")) {
    colours <- vellum_pal(colours, n = n)
  } else if (length(colours) == 1L && colours %in% c(vellum_palettes()$palette)) {
    colours <- vellum_pal(colours, n = n)
  }
  check_colour(colours, "colours")
  cols <- toupper(substr(grDevices::rgb(t(grDevices::col2rgb(colours)), maxColorValue = 255), 1, 7))
  thr <- utils::modifyList(.audit_thresholds, thresholds %||% list())

  Lv <- lightness(cols)
  if (type == "auto") type <- infer_palette_type(Lv)

  conditions <- c(normal = "normal", deuteranopia = "deutan",
                  protanopia = "protan", tritanopia = "tritan", greyscale = "grey")
  seen <- lapply(conditions, function(cd) if (cd == "normal") cols else cvd(cols, cd, severity = severity))
  names(seen) <- names(conditions)

  sep <- vapply(seen, min_separation, numeric(1))
  worst_cond <- names(sep)[which.min(sep)]

  # Name the weakest pair under the worst condition, because "improve the
  # palette" is not actionable but "these two colours collide" is.
  worst_pair <- NA_character_
  if (length(cols) > 1L) {
    m <- delta_e(seen[[worst_cond]])
    diag(m) <- Inf
    ij <- which(m == min(m), arr.ind = TRUE)[1, ]
    worst_pair <- paste(cols[ij[1]], "vs", cols[ij[2]])
  }

  L_range <- diff(range(Lv))
  monotone <- is_monotone(Lv, tol = 1)
  linear_dev <- NA_real_
  if (type %in% c("sequential", "diverging") && length(Lv) > 2L) {
    if (type == "sequential") {
      target <- seq(Lv[1], Lv[length(Lv)], length.out = length(Lv))
      linear_dev <- max(abs(Lv - target))
    } else {
      half <- ceiling(length(Lv) / 2)
      a <- Lv[seq_len(half)]
      b <- rev(Lv[seq(length(Lv) - half + 1, length(Lv))])
      dev <- function(v) max(abs(v - seq(v[1], v[length(v)], length.out = length(v))))
      linear_dev <- max(dev(a), dev(b))
    }
  }

  contrast_dark <- max(contrast_ratio(cols, "#FFFFFF"))
  contrast_light <- min(contrast_ratio(cols, "#FFFFFF"))

  checks <- list()
  add <- function(id, label, value, verdict, detail) {
    checks[[id]] <<- list(id = id, label = label, value = value,
                          verdict = verdict, detail = detail)
  }
  grade <- function(x, pass, good) if (is.na(x)) "skip" else if (x >= good) "pass" else if (x >= pass) "warn" else "fail"

  # The checks differ by palette family, because the families make different
  # claims and are therefore falsified by different evidence.
  #
  # Applying the qualitative criterion to an ordered ramp is a category error and
  # a common one: adjacent steps of a smooth 256-colour ramp are supposed to be
  # nearly identical, so a minimum-pairwise-separation test would condemn every
  # good ramp and reward a banded one. What an ordered ramp must instead show is
  # that its steps are EVENLY spaced and that its ORDER survives.

  if (type == "qualitative") {
    add("sep_normal", "Separation, normal vision", sep[["normal"]],
        grade(sep[["normal"]], thr$qual_min_pass, thr$qual_min_good),
        sprintf("min \u0394E00 = %.1f (pass \u2265 %g, good \u2265 %g)",
                sep[["normal"]], thr$qual_min_pass, thr$qual_min_good))

    for (cd in c("deuteranopia", "protanopia", "tritanopia")) {
      add(paste0("sep_", cd), paste0("Separation, ", cd), sep[[cd]],
          grade(sep[[cd]], thr$qual_cvd_pass, thr$qual_cvd_good),
          sprintf("min \u0394E00 = %.1f (pass \u2265 %g, Okabe-Ito grade \u2265 %g)",
                  sep[[cd]], thr$qual_cvd_pass, thr$qual_cvd_good))
    }

    # Greyscale for unordered groups is arithmetically unreachable past about
    # four categories: eight greys separated by DeltaE00 12 would consume essentially
    # the whole lightness axis, leaving nothing for hue. Reported as information
    # with the correct remedy, never as a failure -- the answer is a second
    # channel, not a better palette.
    add("sep_grey", "Separation, greyscale", sep[["greyscale"]],
        if (sep[["greyscale"]] >= thr$qual_cvd_pass) "pass" else "warn",
        sprintf(paste0("min \u0394E00 = %.1f. Expected for unordered groups; also encode ",
                       "group with shape or linetype (see vellum_shapes())."),
                sep[["greyscale"]]))

  } else {
    # -- ordered ramps ------------------------------------------------------
    add("L_range", "Lightness range", L_range,
        grade(L_range, thr$L_range_pass, thr$L_range_good),
        sprintf("CIE L* spans %.1f (%.1f to %.1f)", L_range, min(Lv), max(Lv)))

    if (type == "sequential") {
      add("L_monotone", "Lightness monotone", as.numeric(monotone),
          if (monotone) "pass" else "fail",
          if (monotone) "lightness moves in one direction throughout"
          else "lightness reverses, manufacturing false contours (the jet problem)")
    }

    if (!is.na(linear_dev)) {
      frac <- linear_dev / max(L_range, 1e-9)
      add("L_linear", "Lightness linear in position", linear_dev,
          if (frac <= thr$L_linear_good) "pass" else if (frac <= thr$L_linear_pass) "warn" else "fail",
          sprintf("max deviation %.1f L* = %.1f%% of range (pass \u2264 %g%%)",
                  linear_dev, 100 * frac, 100 * thr$L_linear_pass))
    }

    # Perceptual uniformity: equal data steps should be equal perceptual steps,
    # so the CIEDE2000 distances between consecutive colours should all be about
    # the same. The coefficient of variation of those distances measures it.
    #
    # Measured on a subsample of at most 32 colours. In a 256-step ramp the
    # distance between neighbours is around 0.4 DeltaE00, which is at the level of
    # 8-bit RGB quantisation, so the variability of those distances would be
    # dominated by rounding noise rather than by the ramp's design.
    unif_cols <- if (length(cols) > 32L) cols[round(seq(1, length(cols), length.out = 32L))] else cols
    adj <- adjacent_de(unif_cols)
    cv <- if (length(adj) > 1L && mean(adj) > 0) stats::sd(adj) / mean(adj) else NA_real_
    add("uniformity", "Step uniformity", cv,
        if (is.na(cv)) "skip" else if (cv <= thr$uniform_good) "pass"
        else if (cv <= thr$uniform_pass) "warn" else "fail",
        sprintf("adjacent \u0394E00 varies by CV %.2f (pass \u2264 %g); mean step %.1f over %d samples",
                cv, thr$uniform_pass, mean(adj), length(unif_cols)))

    # Order preservation: the question that actually matters for an ordered ramp
    # under colour-vision deficiency is not whether two colours differ, but
    # whether the reader still sees the scale running the same way. A ramp whose
    # perceived lightness stays monotone is readable by every reader.
    #
    # For a diverging ramp the test is applied arm by arm: a diverging scale is
    # V-shaped in lightness by design, so requiring overall monotonicity would
    # condemn every correct diverging palette.
    order_monotone <- if (type == "diverging") {
      function(v) {
        k <- ceiling(length(v) / 2)
        is_monotone(v[seq_len(k)], tol = 1.5) &&
          is_monotone(v[seq(length(v) - k + 1, length(v))], tol = 1.5)
      }
    } else {
      function(v) is_monotone(v, tol = 1.5)
    }
    order_ok <- vapply(c("deuteranopia", "protanopia", "tritanopia", "greyscale"),
                       function(cd) order_monotone(lightness(seen[[cd]])), logical(1))
    add("order_cvd", "Order survives colour-vision deficiency", sum(order_ok),
        if (all(order_ok)) "pass" else if (sum(order_ok) >= 3L) "warn" else "fail",
        sprintf("monotone under %d of 4 simulated conditions%s",
                sum(order_ok),
                if (all(order_ok)) "" else
                  paste0(" \u2014 fails: ", paste(names(order_ok)[!order_ok], collapse = ", "))))

    if (type == "diverging") {
      # The two arms must be separable by hue at matched lightness, otherwise a
      # positive deviation and an equal negative one look identical. In greyscale
      # they necessarily do look identical, because a light-hinge diverging ramp
      # is a symmetric function of lightness -- an inherent property of the form,
      # not a defect in the colours, so it is reported rather than failed.
      arm <- arm_separation(cols)
      arm_cvd <- vapply(c("deuteranopia", "protanopia", "tritanopia"),
                        function(cd) arm_separation(seen[[cd]]), numeric(1))
      add("arm_hue", "Arms separable by hue", min(arm_cvd),
          grade(min(arm_cvd), thr$qual_cvd_pass, thr$qual_cvd_good),
          sprintf("matched-lightness arms differ by \u0394E00 %.1f normal, %.1f worst simulated",
                  arm, min(arm_cvd)))
      add("arm_grey", "Arms in greyscale", arm_separation(seen[["greyscale"]]), "warn",
          sprintf(paste0("\u0394E00 %.1f. Any symmetric diverging ramp is a symmetric ",
                         "function of lightness, so the sign of a deviation is not ",
                         "recoverable in greyscale by construction. State the direction ",
                         "in the axis or legend label rather than expecting colour to carry it."),
                  arm_separation(seen[["greyscale"]])))
    }
  }

  add("contrast", "Darkest colour vs white", contrast_dark,
      grade(contrast_dark, thr$contrast_pass, 4.5),
      sprintf("contrast ratio %.1f:1 (WCAG graphical objects \u2265 %.1f:1)",
              contrast_dark, thr$contrast_pass))

  verdicts <- vapply(checks, `[[`, character(1), "verdict")
  overall <- if (any(verdicts == "fail")) "fail" else if (any(verdicts == "warn")) "warn" else "pass"

  structure(list(
    colours = cols, type = type, n = length(cols), severity = severity,
    seen = seen, separation = sep, worst_condition = worst_cond, worst_pair = worst_pair,
    lightness = Lv, L_range = L_range, monotone = monotone, linear_dev = linear_dev,
    contrast_dark = contrast_dark, contrast_light = contrast_light,
    checks = checks, verdict = overall, thresholds = thr
  ), class = "vellum_audit")
}

#' @export
print.vellum_audit <- function(x, ...) {
  head <- switch(x$verdict,
                 pass = "Palette audit \u00b7 pass",
                 warn = "Palette audit \u00b7 pass with warnings",
                 fail = "Palette audit \u00b7 fail")
  cli::cli_h1(head)
  cli::cli_text("{x$n} colours \u00b7 {x$type} \u00b7 CVD severity {x$severity}")
  show_swatches(x$colours)

  cli::cli_par()
  for (ck in x$checks) {
    if (identical(ck$verdict, "skip")) next
    fn <- switch(ck$verdict,
                 pass = cli::cli_alert_success,
                 warn = cli::cli_alert_warning,
                 fail = cli::cli_alert_danger)
    fn("{ck$label}: {ck$detail}")
  }
  cli::cli_end()

  if (!is.na(x$worst_pair)) {
    cli::cli_text("{.strong Weakest pair} under {x$worst_condition}: {x$worst_pair}")
  }
  if (x$verdict != "pass") {
    tips <- c(i = "Inspect it as each reader sees it with {.code plot()} on this object.")
    cvd_ids <- intersect(c("sep_deuteranopia", "sep_protanopia", "sep_tritanopia"), names(x$checks))
    cvd_fail <- length(cvd_ids) > 0L &&
      any(vapply(x$checks[cvd_ids], function(ck) identical(ck$verdict, "fail"), logical(1)))
    if (cvd_fail && x$type == "qualitative" && x$n > 2L) {
      safe <- palette_max_safe(x$colours, threshold = x$thresholds$qual_cvd_pass)
      tips <- c(tips, i = sprintf(
        "The first %d colours are separable (\u0394E00 %.1f); the collision starts at colour %d.",
        safe, attr(safe, "separation"), safe + 1L))
    }
    tips <- c(tips, i = "For more separable alternatives see {.fn vellum_palettes} or {.fn palette_optimise}.")
    cli::cli_bullets(tips)
  }
  invisible(x)
}

#' Diagnostic plot for a palette audit
#'
#' Renders the palette as a reader with normal vision, deuteranopia, protanopia,
#' tritanopia, and in greyscale would see it, annotated with the worst-case
#' separation in each case, together with the lightness profile against the linear
#' ideal.
#'
#' @param x A `vellum_audit` object from [palette_audit()].
#' @param ... Ignored.
#' @return A `ggplot` or `patchwork` object.
#' @method plot vellum_audit
#' @export
#' @examples
#' plot(palette_audit(okabe_ito()))
plot.vellum_audit <- function(x, ...) {
  # Panel 1: the palette as each reader sees it.
  grid_df <- do.call(rbind, lapply(seq_along(x$seen), function(i) {
    data.frame(condition = names(x$seen)[i], i = seq_along(x$colours),
               fill = unname(x$seen[[i]]), stringsAsFactors = FALSE)
  }))
  grid_df$condition <- factor(grid_df$condition, levels = names(x$seen))
  lab <- sprintf("%s  (min \u0394E00 %.0f)", names(x$separation), x$separation)
  levels(grid_df$condition) <- lab

  p_swatch <- ggplot(grid_df, aes(x = .data$i, y = 1, fill = .data$fill)) +
    geom_tile(width = 1, height = 1) +
    scale_fill_identity() +
    facet_wrap(~condition, ncol = 1, strip.position = "left") +
    labs(title = "As each reader sees it") +
    theme_vellum(grid = "none", axis_style = "none", ticks = "none") +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text.y.left = element_text(angle = 0, hjust = 1))

  # Panel 2: the lightness profile, against the linear ideal.
  Ldf <- data.frame(i = seq_along(x$lightness), L = x$lightness,
                    fill = x$colours, stringsAsFactors = FALSE)
  ideal <- data.frame(i = c(1, nrow(Ldf)),
                      L = c(x$lightness[1], x$lightness[length(x$lightness)]))
  p_light <- ggplot(Ldf, aes(x = .data$i, y = .data$L)) +
    {if (x$type != "qualitative")
      geom_line(data = ideal, aes(x = .data$i, y = .data$L),
                linetype = "22", linewidth = lw(0.4), colour = "#767676")} +
    geom_line(linewidth = lw(0.6), colour = "#1A1A1A") +
    geom_point(aes(fill = .data$fill), shape = 21, size = ps(2.2),
               stroke = lw(0.4), colour = "#1A1A1A") +
    scale_fill_identity() +
    labs(x = "Position in palette", y = "CIE L*",
         title = "Lightness profile",
         subtitle = if (x$type == "qualitative") "Variation is desirable here"
         else "Dashed line is the linear ideal") +
    theme_vellum()

  if (has_pkg("patchwork")) {
    return(patchwork::wrap_plots(p_swatch, p_light, ncol = 1,
                                 heights = c(length(x$seen) * 0.55, 1)))
  }
  cli::cli_inform("Install {.pkg patchwork} to see both diagnostic panels together.")
  p_light
}

#' Compare palettes side by side under every vision condition
#'
#' @description
#' The demonstration form of [palette_audit()]. Give it several palettes and it
#' renders each one as a reader with normal vision, deuteranopia, protanopia,
#' tritanopia, and greyscale would see it, annotated with the worst-case
#' separation. This is how to settle an argument about a palette without asserting
#' anything.
#'
#' @param ... Named palettes: either character vectors of colours or palette
#'   names. Names become row labels.
#' @param n Number of colours to draw from any palette given by name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' palette_compare(
#'   `ggplot2 default` = c("#F8766D", "#00BA38", "#619CFF"),
#'   `Okabe-Ito` = okabe_ito(3),
#'   `Tol high-contrast` = "high_contrast",
#'   n = 3
#' )
palette_compare <- function(..., n = NULL) {
  pals <- list(...)
  if (!length(pals)) cli::cli_abort("Supply at least one palette.")
  if (is.null(names(pals)) || any(!nzchar(names(pals)))) {
    cli::cli_abort("Every palette must be named; the names become the row labels.")
  }
  resolve <- function(p) {
    if (length(p) == 1L && p %in% vellum_palettes()$palette) vellum_pal(p, n = n) else p
  }
  conditions <- c(normal = "normal", deuteranopia = "deutan", protanopia = "protan",
                  tritanopia = "tritan", greyscale = "grey")

  rows <- lapply(names(pals), function(nm) {
    cols <- resolve(pals[[nm]])
    do.call(rbind, lapply(names(conditions), function(cn) {
      seen <- if (conditions[[cn]] == "normal") cols else cvd(cols, conditions[[cn]])
      data.frame(palette = nm, condition = cn, i = seq_along(seen),
                 fill = seen, sep = min_separation(seen), stringsAsFactors = FALSE)
    }))
  })
  df <- do.call(rbind, rows)
  df$palette <- factor(df$palette, levels = names(pals))
  df$condition <- factor(df$condition, levels = names(conditions))

  lab <- unique(df[c("palette", "condition", "sep")])
  lab$txt <- sprintf("%.0f", lab$sep)
  lab$i <- max(df$i) + 0.9

  ggplot(df, aes(x = .data$i, y = .data$condition, fill = .data$fill)) +
    geom_tile(width = 0.94, height = 0.82) +
    geom_text(data = lab, aes(x = .data$i, y = .data$condition, label = .data$txt),
              inherit.aes = FALSE, hjust = 0, size = fs(5.5), colour = "#4D4D4D") +
    scale_fill_identity() +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(expand = expansion(add = c(0.5, 2.2))) +
    facet_wrap(~palette, ncol = 1, strip.position = "top", scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = "The same palette, as five readers see it",
         subtitle = "Numbers are the minimum CIEDE2000 separation within each row") +
    theme_vellum(grid = "none", axis_style = "none", ticks = "none") +
    theme(axis.text.x = element_blank(), panel.grid = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"))
}
