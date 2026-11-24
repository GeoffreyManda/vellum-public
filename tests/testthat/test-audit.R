test_that("the audit catches the failure it exists to catch", {
  # ggplot2's default three-group scale separates well under normal vision and
  # collapses under deuteranopia, because it holds luminance constant while
  # varying hue. That is the single most common accessibility failure in published
  # figures and the reason this package exists.
  a <- palette_audit(c("#F8766D", "#00BA38", "#619CFF"))
  expect_equal(a$type, "qualitative")
  expect_gt(a$separation[["normal"]], 30)          # looks fine
  expect_lt(a$separation[["deuteranopia"]], 8)     # is not fine
  expect_equal(a$verdict, "fail")
})

test_that("Okabe-Ito passes at the sizes it is meant for", {
  expect_equal(palette_audit(okabe_ito(5))$verdict %in% c("pass", "warn"), TRUE)
  # The eighth colour, grey, collides with the reddish purple under deuteranopia.
  expect_equal(as.integer(palette_max_safe("okabe_ito")), 7L)
})

test_that("palette type is inferred from lightness structure, not position", {
  # Okabe-Ito's lightest colour happens to sit near the middle of the vector, which
  # a naive diverging test mistakes for a hinge. The discriminator is that both
  # arms must be monotone.
  expect_equal(palette_audit(okabe_ito())$type, "qualitative")
  expect_equal(palette_audit("abyss", n = 9)$type, "sequential")
  expect_equal(palette_audit("strata", n = 11)$type, "diverging")
})

test_that("ordered ramps are judged as ramps, not as category sets", {
  a <- palette_audit("abyss", n = 9)
  # A smooth ramp's neighbours are supposed to be close, so a minimum-pairwise
  # separation test must not be applied to it.
  expect_null(a$checks$sep_normal)
  expect_false(is.null(a$checks$uniformity))
  expect_false(is.null(a$checks$order_cvd))
  expect_equal(a$verdict, "pass")
})

test_that("every shipped ramp passes its own audit", {
  for (pal in c("ink", "abyss", "ember", "verdant", "terra", "bloom")) {
    expect_equal(palette_audit(pal, n = 9)$verdict, "pass", info = pal)
  }
  # Diverging ramps warn on greyscale sign recovery, which is inherent to the form
  # rather than a defect, so "warn" is the best achievable verdict.
  for (pal in c("strata", "tide", "equinox", "polarity")) {
    expect_true(palette_audit(pal, n = 11)$verdict %in% c("pass", "warn"), info = pal)
    expect_equal(palette_audit(pal, n = 11)$checks$order_cvd$verdict, "pass", info = pal)
  }
})

test_that("the audit rejects a rainbow scale for the documented reasons", {
  a <- palette_audit(grDevices::rainbow(11), type = "sequential")
  expect_equal(a$verdict, "fail")
  expect_equal(a$checks$L_monotone$verdict, "fail")   # false contours
  expect_equal(a$checks$L_linear$verdict, "fail")     # not perceptually uniform
})

test_that("the optimiser beats curated palettes on worst-case separation", {
  worst <- function(cols) {
    min(vapply(c("deutan", "protan", "tritan"),
               function(c) min_separation(cvd(cols, c)), numeric(1)))
  }
  for (n in c(6L, 8L)) {
    p <- palette_optimise(n)
    expect_length(p, n)
    expect_gt(worst(p), worst(okabe_ito(n)))
  }
  # It reports what it achieved, so the claim is checkable.
  expect_true(is.numeric(attr(palette_optimise(5), "min_delta_e")))
})

test_that("qualitative palettes refuse to interpolate", {
  expect_error(vellum_pal("high_contrast", 8), "not interpolated")
  expect_error(okabe_ito(12), "colours")
  # Ordered palettes may be interpolated, because an intermediate value exists.
  expect_length(vellum_pal("sunset", 21, interpolate = TRUE), 21L)
})

test_that("missing-data colours come from the palette where defined", {
  expect_equal(pal_na("sunset"), "#FFFFFF")
  expect_equal(pal_na("burd"), "#FFEE99")
  expect_equal(pal_na("abyss"), "#BDBDBD")
})

test_that("audit objects print and plot", {
  a <- palette_audit(okabe_ito(4))
  # cli writes its report to the message stream, not stdout.
  expect_match(paste(capture.output(print(a), type = "message"), collapse = "\n"),
               "Palette audit")
  expect_s3_class(suppressWarnings(plot(a)), c("patchwork", "ggplot"))
})

test_that("palette_compare requires names and builds", {
  expect_error(palette_compare(okabe_ito(3)), "must be named")
  p <- palette_compare(`Okabe-Ito` = okabe_ito(3),
                       `ggplot2 default` = c("#F8766D", "#00BA38", "#619CFF"))
  expect_s3_class(p, "ggplot")
})
