test_that("unit converters match hand-computed physical values", {
  # A rule of w points needs linewidth = w * (96/72.27) / (72.27/25.4).
  expect_equal(lw(1), (96 / 72.27) / (72.27 / 25.4), tolerance = 1e-12)
  expect_equal(lw(0.25), 0.1167156, tolerance = 1e-6)  # Nature's minimum hairline

  # ggplot2's default linewidth of 0.5 renders at about 1.07 pt.
  expect_equal(linewidth_pt(0.5), 1.07098, tolerance = 1e-5)

  # lw() and linewidth_pt() must be exact inverses.
  for (p in c(0.1, 0.25, 0.5, 1, 2.5)) {
    expect_equal(linewidth_pt(lw(p)), p, tolerance = 1e-12)
  }

  # geom_text size is points divided by .pt.
  expect_equal(fs(6), 6 / (72.27 / 25.4), tolerance = 1e-12)
  expect_equal(mm_pt(pt_mm(7)), 7, tolerance = 1e-12)

  # .pt points is exactly one millimetre, which is why point size reads as mm.
  expect_equal(pt_mm(72.27 / 25.4), 1, tolerance = 1e-12)
})

test_that("unit converters reject non-numbers but accept vectors and gaps", {
  expect_error(lw("thin"), "must be numeric")
  expect_error(fs(TRUE), "must be numeric")
  expect_error(lw(Inf), "must be finite")

  # Vectorised, so a whole set of measured weights converts in one call, and a
  # measurement that does not exist converts to one that does not exist rather
  # than aborting the report it appears in.
  expect_length(lw(c(0.25, 0.5, 1)), 3L)
  expect_equal(lw(c(0.25, 0.5)), c(lw(0.25), lw(0.5)))
  expect_true(is.na(mm_pt(NA_real_)))
  expect_length(fs(numeric()), 0L)
})

test_that("grid unit helpers round-trip", {
  expect_equal(as_mm(u_mm(12.5)), 12.5, tolerance = 1e-9)
  expect_equal(as_mm(grid::unit(1, "inch")), 25.4, tolerance = 1e-6)
})
