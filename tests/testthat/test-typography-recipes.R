library(ggplot2)

test_that("number labels use a true minus and never a comma", {
  f <- label_vellum()
  out <- f(c(-1500, 0, 1500, 250000))
  expect_true(grepl("\u2212", out[1]))   # U+2212 MINUS SIGN, not hyphen
  expect_false(grepl("-", out[1], fixed = TRUE))
  expect_false(grepl(",", out[4], fixed = TRUE))
  # Never "-0".
  expect_equal(label_vellum(0)(-0.2), "0")
})

test_that("the group separator follows the SI four-digit rule", {
  # The SI Brochure groups digits only above four, which is why a year is 2024 and
  # not 2 024. Grouping at four digits is the commonest way a correct-looking axis
  # reads wrong, and it showed up on a real figure's year axis.
  years <- label_vellum(0)(c(2000, 2010, 2020))
  expect_equal(years, c("2000", "2010", "2020"))
  expect_false(any(grepl("\u2009", years)))

  # Above four digits the thin space appears.
  big <- label_vellum(0)(c(100000, 250000))
  expect_true(all(grepl("\u2009", big)))
  expect_false(any(grepl(",", big, fixed = TRUE)))
})

test_that("automatic digits keep breaks distinct without over-reporting", {
  expect_equal(label_vellum()(c(1, 2, 3)), c("1", "2", "3"))
  expect_equal(label_vellum()(c(1, 1.5, 2)), c("1.0", "1.5", "2.0"))
})

test_that("automatic digits are faithful, not merely distinct", {
  # -5, -2.5, 0, 2.5, 5 rounds at zero decimals to -5, -2, 0, 2, 5: all distinct,
  # so a duplicate test alone accepts it and the axis then labels the 2.5 tick "2".
  expect_equal(vellum:::auto_digits(c(-5, -2.5, 0, 2.5, 5)), 1L)
  out <- label_vellum()(c(-5, -2.5, 0, 2.5, 5))
  expect_equal(out[2], "−2.5")
  expect_equal(out[4], "2.5")

  # And it must not inflate decimals when the breaks really are integers.
  expect_equal(vellum:::auto_digits(c(-4, -2, 0, 2, 4)), 0L)
  expect_equal(vellum:::auto_digits(c(0, 0.25, 0.5, 0.75, 1)), 2L)
})

test_that("SI labels place a no-break thin space before the unit", {
  out <- label_si("g")(c(0.002, 1, 1500))
  expect_true(all(grepl(" ", out)))        # U+202F NARROW NO-BREAK SPACE
  expect_true(grepl("mg$", out[1]))
  expect_true(grepl("kg$", out[3]))
  expect_equal(label_si("")(0), "0")
})

test_that("P values are exact and never zero", {
  expect_equal(label_pval(0.032), "P = 0.032")
  expect_equal(label_pval(1e-9), "P < 0.001")
  expect_true(is.expression(label_pval(0.032, italic_p = TRUE)))
})

test_that("estimates read correctly with negative bounds", {
  # An en dash between negative bounds is unreadable, so the separator is a word.
  out <- fmt_estimate(-0.4, -0.72, -0.08)
  expect_true(grepl(" to ", out, fixed = TRUE))
  expect_equal(as.character(out), "\u22120.40 (\u22120.72 to \u22120.08)")
})

test_that("label helpers return the right shapes", {
  expect_true(is.expression(label_log()(c(1, 10, 100))))
  expect_true(is.expression(italicise("Escherichia coli")))
  expect_true(grepl("\n", label_wrap(10)("a rather long axis label here")))
})

test_that("every recipe builds", {
  d <- data.frame(
    study = c("Adeyemi 2019", "Baptiste 2020", "Chen 2021", "Pooled"),
    rr = c(0.72, 0.88, 0.61, 0.79), lo = c(0.55, 0.71, 0.44, 0.70),
    hi = c(0.94, 1.09, 0.85, 0.89), w = c(1, 1.6, 0.8, 4)
  )
  m <- matrix(seq(-3, 3, length.out = 60), nrow = 10,
              dimnames = list(paste0("g", 1:10), paste0("s", 1:6)))
  suppressMessages({
    plots <- list(
      fig_dotstrip(iris, "Species", "Sepal.Length",
                   comparisons = list(c("setosa", "virginica")), p_values = 1.2e-8),
      fig_forest(d, "rr", "lo", "hi", "study", weight = "w", null = 1),
      fig_raincloud(iris, "Species", "Sepal.Length"),
      fig_waffle(c(Complete = 62, Partial = 24, None = 14)),
      fig_donut(c(Complete = 62, Partial = 24, None = 14)),
      fig_heatmap(m, diverging = TRUE, midpoint = 0),
      fig_heatmap(m, cluster = TRUE, label_values = TRUE)
    )
  })
  for (i in seq_along(plots)) {
    expect_no_error(ggplot_gtable(ggplot_build(plots[[i]])))
  }
  expect_no_error(patchwork::patchworkGrob(superiority_panel()))

  # Customized parameters
  p_forest_custom <- fig_forest(d, "rr", "lo", "hi", "study", point_shape = 16, point_alpha = 0.8)
  expect_no_error(ggplot_gtable(ggplot_build(p_forest_custom)))
  p_rc_custom <- fig_raincloud(iris, "Species", "Sepal.Length", point_alpha = 0.25)
  expect_no_error(ggplot_gtable(ggplot_build(p_rc_custom)))
})

test_that("fig_dotstrip refuses mismatched comparisons", {
  expect_error(fig_dotstrip(iris, "Species", "Sepal.Length",
                            comparisons = list(c("setosa", "virginica"))),
               "one value per entry")
  expect_error(fig_dotstrip(iris, "Species", "Sepal.Length",
                            comparisons = list(c("setosa", "nonexistent")),
                            p_values = 0.01),
               "not present")
})

test_that("fig_donut argues its case once, then stays quiet", {
  vellum_reset_notices()
  withr::local_options(vellum.quiet_chart_advice = FALSE)
  expect_message(
    suppressWarnings(fig_donut(c(a = 3, b = 2))),
    "least accurate perceptual channels"
  )
  expect_message(suppressWarnings(fig_donut(c(a = 3, b = 2))), NA)
  vellum_reset_notices()
})

test_that("fig_donut collapses excess slices rather than drawing them", {
  suppressWarnings(suppressMessages({
    p <- fig_donut(c(a = 40, b = 22, c = 14, d = 9, e = 6, f = 5, g = 4), max_slices = 5)
  }))
  built <- ggplot_build(p)
  # Five wedges: four named plus an explicit "Other".
  expect_equal(nrow(built$data[[1]]), 5L)
  expect_true(any(grepl("^Other", levels(p$data$cat))))
})

test_that("fig_waffle squares sum to the data", {
  p <- fig_waffle(c(A = 60, B = 40), unit = 1)
  expect_equal(nrow(ggplot_build(p)$data[[1]]), 100L)
})

test_that("annotate_signif and annotate_scalebar return layer lists", {
  expect_length(annotate_signif(c(1, 2), y = 8, p = 0.01), 2L)
  expect_length(annotate_scalebar(0, 0, length = 10, unit = "µm"), 2L)
  expect_error(annotate_signif(1, y = 8, p = 0.01), "exactly two positions")
  expect_error(annotate_signif(c(1, 2), y = 8), "Supply either")
})

test_that("geom_pointinterval validates its widths", {
  expect_error(geom_pointinterval(.width = 0.95), "two widths")
  expect_length(geom_pointinterval(), 3L)
})
