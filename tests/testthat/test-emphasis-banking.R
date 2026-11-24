library(ggplot2)

test_that("emphasise gives the accent to the named levels and mutes the rest", {
  out <- emphasise(c("A", "B", "C", "D"), highlight = "C")
  expect_equal(unname(out[["C"]]), okabe_ito(1))
  expect_true(all(out[c("A", "B", "D")] == vellum:::.mute_grey))
  expect_named(out, c("A", "B", "C", "D"))

  two <- emphasise(month.abb, highlight = c("Jan", "Jul"))
  expect_length(unique(two[c("Jan", "Jul")]), 2L)
  expect_equal(sum(two == vellum:::.mute_grey), 10L)

  expect_error(emphasise(c("A", "B"), highlight = "Z"), "not in")
})

test_that("the mute colour clears WCAG 3:1 for a graphical object", {
  # The default grey has to stay visible as a line or a point, not just as a wash,
  # and the documented claim is 3:1 exactly. An earlier default was documented as
  # meeting it and measured 2.68:1, so this asserts the number rather than the
  # intention.
  expect_gte(contrast_ratio(vellum:::.mute_grey, "#FFFFFF"), 3)
  expect_lt(contrast_ratio(vellum:::.mute_grey, "#FFFFFF"), 4.5)  # muted, not black
})

test_that("emphasis scales need no level list", {
  # The mechanism: name only the highlights, send the rest to na.value. Requiring the
  # full level list would make the scale useless in a pipeline.
  p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
    geom_point() + scale_colour_emphasis("virginica")
  cols <- unique(ggplot_build(p)$data[[1]]$colour)
  expect_length(cols, 2L)
  expect_true(vellum:::.mute_grey %in% cols)

  pf <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
    geom_boxplot() + scale_fill_emphasis(c("setosa", "virginica"))
  expect_length(unique(ggplot_build(pf)$data[[1]]$fill), 3L)

  expect_error(scale_colour_emphasis(character(0)), "at least one level")
})

test_that("label_series places one label per group and spreads them", {
  d <- data.frame(
    t = rep(1:10, 3),
    v = c(1:10, (1:10) + 0.15, (1:10) - 0.15),   # deliberately converging
    g = rep(c("a", "b", "c"), each = 10)
  )
  layers <- label_series(d, "t", "v", "g", min_gap = 0.12)
  expect_length(layers, 2L)
  ends <- layers[[1]]$data
  expect_equal(nrow(ends), 3L)
  # Labels must be at least the requested gap apart, even though the data are not.
  gap <- min(diff(sort(ends$.lab_y)))
  expect_gt(gap, 0.12 * diff(range(d$v)) * 0.99)

  p <- ggplot(d, aes(t, v, colour = g)) + geom_line() + layers +
    scale_colour_vellum(guide = "none") + theme_vellum()
  expect_no_error(ggplot_gtable(ggplot_build(p)))
})

test_that("label_series can label the left end instead", {
  d <- data.frame(t = rep(1:10, 2), v = c(1:10, 10:1), g = rep(c("a", "b"), each = 10))
  right <- label_series(d, "t", "v", "g")[[1]]$data
  left <- label_series(d, "t", "v", "g", side = "left")[[1]]$data
  expect_true(all(right$.x == 10))
  expect_true(all(left$.x == 1))
})

test_that("bank_aspect banks the typical slope to 45 degrees", {
  # A perfectly linear series has median slope equal to y_span/x_span, so the banked
  # aspect ratio is exactly 1 whatever the units.
  for (m in c(0.02, 1, 50)) {
    d <- data.frame(x = 1:40, y = (1:40) * m)
    expect_equal(bank_aspect(d, "x", "y"), 1, tolerance = 1e-9)
  }

  # Doubling the y range for the same x range halves the required aspect ratio.
  d1 <- data.frame(x = 1:40, y = seq(0, 10, length.out = 40))
  d2 <- data.frame(x = 1:40, y = seq(0, 20, length.out = 40))
  expect_equal(bank_aspect(d1, "x", "y"), bank_aspect(d2, "x", "y"), tolerance = 1e-9)

  expect_error(bank_aspect(data.frame(x = 1, y = 1), "x", "y"), "at least two")
  expect_error(bank_aspect(data.frame(x = rep(1, 5), y = 1:5), "x", "y"), "no range")
})

test_that("bank_height composes with fit_width and actually lands on 45 degrees", {
  set.seed(11)
  d <- data.frame(x = 1:40, y = cumsum(rnorm(40, 0.4)))
  p <- ggplot(d, aes(x, y)) + geom_line() + theme_venue("nature")
  h <- NULL
  expect_warning(
    h <- bank_height(p, d, "x", "y", venue = "nature", column = "single"),
    "outside"
  )

  f <- fit_width(p, "nature", "single", height = h, quiet = TRUE)
  dm <- figure_dim(f)

  # The exact-width guarantee must survive banking.
  expect_equal(dm$total[["width"]], 89, tolerance = 0.3)
  # And the panel must have the banked shape.
  expect_equal(dm$panel[["height"]] / dm$panel[["width"]], attr(h, "aspect"),
               tolerance = 0.02)

  # The point of the exercise: the median segment renders at 45 degrees.
  s <- stats::median(abs(diff(d$y)) / abs(diff(d$x)))
  rendered <- s * (dm$panel[["height"]] / diff(range(d$y))) /
    (dm$panel[["width"]] / diff(range(d$x)))
  expect_equal(atan(rendered) * 180 / pi, 45, tolerance = 1.5)
})

test_that("a series with no typical slope falls back rather than dividing by zero", {
  # Flat except for one jump: the median segment slope is exactly 0, so there is no
  # angle to bank to. Returning 1 is the honest answer, not an infinite aspect ratio.
  flat <- data.frame(x = 1:100, y = c(rep(0, 99), 40))
  expect_equal(bank_aspect(flat, "x", "y"), 1)
})

test_that("bank_height refuses to make an absurd figure, and says why", {
  # Tiny wiggles plus two huge excursions: the median segment is nearly flat while the
  # y range is enormous, so banking would demand a panel hundreds of times taller
  # than it is wide.
  set.seed(5)
  wild <- data.frame(x = 1:40, y = c(cumsum(stats::rnorm(38, 0, 0.01)), 50, -50))
  expect_gt(bank_aspect(wild, "x", "y"), 50)

  p <- ggplot(wild, aes(x, y)) + geom_line() + theme_venue("nature")
  expect_warning(h <- bank_height(p, wild, "x", "y", limits = c(1 / 3, 3)), "clamped")
  expect_true(attr(h, "clamped"))
  expect_equal(attr(h, "aspect"), 3)
})

test_that("theme_coverage measures rather than asserts", {
  cov <- theme_coverage()
  expect_s3_class(cov, "data.frame")
  expect_true(all(c("element", "set", "resolved", "family") %in% names(cov)))

  # The root elements are the ones a complete theme genuinely owes ggplot2.
  roots <- c("line", "rect", "text", "title", "point", "polygon",
             "spacing", "margins", "geom")
  expect_true(all(cov$set[match(roots, cov$element)]),
              info = "theme_vellum() must set every root element")

  # And it must cover at least as much as ggplot2's own default theme.
  expect_gte(sum(cov$set), sum(theme_coverage(theme_grey())$set))
  expect_true(nrow(theme_coverage(set_only = TRUE)) < nrow(cov))
})
