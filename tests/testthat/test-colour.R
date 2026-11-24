test_that("the native colour matrices agree with farver on in-gamut colours", {
  # The transcribed OKLab and sRGB matrices exist so gamut detection can see
  # out-of-range values, which farver clips away. If a constant were mistyped, the
  # reconstruction of an in-gamut colour would drift, so this test is what makes
  # those hand-entered numbers trustworthy.
  set.seed(42)
  cols <- grDevices::rgb(stats::runif(300), stats::runif(300), stats::runif(300))
  ref <- t(grDevices::col2rgb(cols))

  for (space in c("oklch", "lch")) {
    p <- as_polar(cols, space = space)
    lin <- polar_to_linear_srgb(p[, "L"], p[, "C"], p[, "h"], space = space)
    enc <- ifelse(lin <= 0.0031308, 12.92 * lin, 1.055 * pmax(lin, 0)^(1 / 2.4) - 0.055) * 255
    # Well under one 8-bit quantisation step.
    expect_lt(max(abs(enc - ref)), 1)
  }
})

test_that("gamut detection sees out-of-range colours", {
  expect_true(in_gamut(0.6, 0.12, 60))
  expect_false(in_gamut(0.6, 0.40, 60))
  expect_false(in_gamut(0.95, 0.30, 150))
})

test_that("gamut mapping reduces chroma only", {
  gm <- gamut_map(0.6, 0.40, 60)
  expect_equal(unname(gm[1, "L"]), 0.6, tolerance = 1e-12)   # lightness untouched
  expect_equal(unname(gm[1, "h"]), 60, tolerance = 1e-12)    # hue untouched
  expect_lt(gm[1, "C"], 0.40)                        # chroma sacrificed
  expect_true(in_gamut(gm[1, "L"], gm[1, "C"], gm[1, "h"]))

  # An already-valid colour must pass through unchanged.
  keep <- gamut_map(0.6, 0.10, 60)
  expect_equal(unname(keep[1, "C"]), 0.10, tolerance = 1e-12)
})

test_that("polar conversion round-trips", {
  cols <- c("#E69F00", "#0072B2", "#009E73", "#000000", "#FFFFFF")
  for (space in c("oklch", "lch")) {
    back <- from_polar(as_polar(cols, space = space), space = space)
    expect_equal(back, toupper(cols), info = space)
  }
})

test_that("sequential ramps are monotone, linear, and in gamut", {
  for (pal in c("ink", "abyss", "ember", "verdant", "terra", "bloom")) {
    cols <- vellum_pal(pal, 9)
    L <- lightness(cols)
    expect_true(all(diff(L) > 0), info = pal)
    expect_gt(diff(range(L)), 80)
    ideal <- seq(L[1], L[length(L)], length.out = length(L))
    # Within 5% of the ramp's own lightness range.
    expect_lt(max(abs(L - ideal)) / diff(range(L)), 0.05)
    p <- as_polar(cols)
    expect_true(all(in_gamut(p[, "L"], p[, "C"], p[, "h"])), info = pal)
  }
})

test_that("diverging ramps have symmetric monotone arms and a neutral hinge", {
  for (pal in c("strata", "tide", "equinox", "polarity")) {
    cols <- vellum_pal(pal, 11)
    L <- lightness(cols)
    lo <- L[1:6]; hi <- rev(L[6:11])
    expect_true(all(diff(lo) > 0) || all(diff(lo) < 0), info = pal)
    expect_true(all(diff(hi) > 0) || all(diff(hi) < 0), info = pal)
    # The arms must reach comparable extremes, or equal deviations would not look
    # equal. CIE L* differs slightly from the OKLab L the ramp was built in.
    expect_lt(abs(L[1] - L[11]), 6, label = pal)
    # The hinge is near-neutral.
    hinge <- as_polar(cols[6])
    expect_lt(hinge[1, "C"], 0.02, label = pal)
  }
})

test_that("lightness linearisation works in both directions", {
  # stats::approx needs increasing x, so a light-to-dark ramp is the regression
  # case: it silently produced garbage before the signed-coordinate fix.
  for (Lspec in list(c(0.20, 0.97), c(0.97, 0.20))) {
    cols <- vellum_ramp(9, L = Lspec, h = c(280, 60))
    L <- lightness(cols)
    ideal <- seq(L[1], L[9], length.out = 9)
    expect_lt(max(abs(L - ideal)) / diff(range(L)), 0.05)
  }
})

test_that("ramp_through honours its anchors", {
  anchors <- c("#08306B", "#4292C6", "#F7FBFF")
  r <- ramp_through(anchors, 7)
  expect_equal(length(r), 7L)
  expect_lt(delta_e(r[1], anchors[1])[1, 1], 3)
  expect_lt(delta_e(r[7], anchors[3])[1, 1], 3)
  expect_true(all(diff(lightness(r)) > 0))
})

test_that("CIEDE2000 and CVD simulation behave", {
  expect_equal(delta_e("#E69F00", "#E69F00")[1, 1], 0, tolerance = 1e-9)
  expect_gt(delta_e("#000000", "#FFFFFF")[1, 1], 90)
  expect_equal(min_separation("#E69F00"), Inf)

  # Simulation must change a red-green pair and leave a grey essentially alone.
  expect_false(identical(cvd("#009E73", "deutan"), "#009E73"))
  expect_lt(delta_e(cvd("#808080", "deutan"), "#808080")[1, 1], 2)

  # Severity 0 is the identity.
  expect_lt(delta_e(cvd("#009E73", "deutan", severity = 0), "#009E73")[1, 1], 1)
})

test_that("contrast ratio matches WCAG reference values", {
  expect_equal(contrast_ratio("#000000", "#FFFFFF"), 21, tolerance = 1e-6)
  expect_equal(contrast_ratio("#FFFFFF", "#FFFFFF"), 1, tolerance = 1e-9)
  # #767676 is the canonical lightest grey passing 4.5:1 on white.
  expect_gte(contrast_ratio("#767676", "#FFFFFF"), 4.5)
  expect_lt(contrast_ratio("#777777", "#FFFFFF"), 4.6)
})

test_that("lightness is CIE L* on 0 to 100", {
  expect_equal(lightness("#000000"), 0, tolerance = 1e-6)
  expect_equal(lightness("#FFFFFF"), 100, tolerance = 1e-6)
})
