library(ggplot2)

test_that("a default ggplot2 figure fails Nature, and the report says why", {
  # base_size 11 against a 7 pt ceiling. This is the gap the package closes, so it
  # is worth asserting rather than assuming.
  r <- venue_check(ggplot(mtcars, aes(wt, mpg)) + geom_point(), "nature", quiet = TRUE)
  expect_equal(r$verdict, "fail")
  expect_equal(r$checks$text_max$verdict, "fail")
  expect_match(r$checks$text_max$detail, "above the 7 pt maximum")
  expect_match(r$checks$text_max$detail, "axis.title.x 11.00 pt")
})

test_that("a figure built for the venue passes cleanly", {
  p <- fit_width(ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature"),
                 "nature", "single", quiet = TRUE)
  r <- venue_check(p, "nature", quiet = TRUE)
  expect_equal(r$verdict, "pass")
  expect_equal(r$checks$text_min$verdict, "pass")
  expect_equal(r$checks$text_max$verdict, "pass")
  expect_equal(r$checks$family$verdict, "pass")
  expect_equal(r$checks$rules$verdict, "pass")
  expect_equal(r$checks$size_width$verdict, "pass")
  expect_equal(r$checks$tag$verdict, "pass")
})

test_that("pre-flight catches oversized figures and off-grid widths", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature")
  expect_equal(venue_check(p, "nature", width = 250, quiet = TRUE)$checks$size_width$verdict, "fail")
  expect_equal(venue_check(p, "nature", height = 400, quiet = TRUE)$checks$size_height$verdict, "fail")
  expect_equal(venue_check(p, "nature", width = 110, quiet = TRUE)$checks$size_width$verdict, "warn")
})

test_that("pre-flight flags an on-plot title for print but not for slides", {
  titled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(title = "Fuel economy falls with weight") + theme_venue("nature")
  expect_equal(venue_check(titled, "nature", quiet = TRUE)$checks$title$verdict, "warn")
  slide <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(title = "Fuel economy falls with weight") + suppressWarnings(theme_vellum_slide())
  expect_equal(suppressWarnings(venue_check(slide, "slide", quiet = TRUE))$checks$title$verdict, "pass")
})

test_that("pre-flight detects raster layers, which carry a dpi requirement", {
  raster <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() + theme_venue("nature")
  expect_equal(venue_check(raster, "nature", quiet = TRUE)$checks$raster$verdict, "warn")
  vector_only <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature")
  expect_equal(venue_check(vector_only, "nature", quiet = TRUE)$checks$raster$verdict, "pass")
})

test_that("pre-flight reports specification provenance honestly", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  expect_equal(venue_check(p, "nature", quiet = TRUE)$checks$provenance$verdict, "pass")
  r <- suppressWarnings(venue_check(p, "cell", quiet = TRUE))
  expect_equal(r$checks$provenance$verdict, "warn")
  expect_match(r$checks$provenance$detail, "unverified")
})

test_that("pre-flight reports and summarises", {
  r <- venue_check(ggplot(mtcars, aes(wt, mpg)) + geom_point(), "nature", quiet = TRUE)
  expect_match(paste(capture.output(print(r), type = "message"), collapse = "\n"),
               "Pre-flight")
  s <- summary(r)
  expect_s3_class(s, "data.frame")
  expect_true(all(c("check", "verdict", "detail") %in% names(s)))
  expect_true(all(s$verdict %in% c("pass", "warn", "fail", "skip")))
})

test_that("save_figure writes every format at the requested size", {

  dir <- withr::local_tempdir()
  p <- fit_width(ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature"),
                 "nature", "single", quiet = TRUE)
  m <- save_figure(p, file.path(dir, "fig1"), venue = "nature",
                   formats = c("pdf", "svg", "png"), quiet = TRUE, check = FALSE)
  expect_equal(nrow(m), 3L)
  expect_true(all(file.exists(m$file)))
  expect_true(all(m$size_kb > 0))
  expect_equal(unique(m$width_mm), 89)

  # Nature does not accept PNG for final artwork, and the manifest must say so
  # rather than implying every file is submittable.
  expect_true(all(m$submission_eligible[m$format %in% c("pdf", "svg")]))
  expect_false(m$submission_eligible[m$format == "png"])
})

test_that("the written size is reported, not just the requested one", {
  # Cairo quantises the PDF page box to whole points, so 89 mm becomes 88.90 mm.
  # The deviation is tiny but it should be reported rather than concealed.

  dir <- withr::local_tempdir()
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature")
  m <- save_figure(p, file.path(dir, "f"), venue = "nature", formats = "pdf",
                   quiet = TRUE, check = FALSE)
  expect_equal(m$written_width_mm, round(round(89 / 25.4 * 72) / 72 * 25.4, 3))
  expect_lt(abs(m$written_width_mm - 89), 0.18)   # under half a point
})

test_that("save_figure rejects unknown formats and uses fitted geometry", {
  dir <- withr::local_tempdir()
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  expect_error(save_figure(p, file.path(dir, "x"), formats = "psd"), "Unsupported format")

  f <- fit_width(p, "nature", "double", height = 90, quiet = TRUE)
  m <- save_figure(f, file.path(dir, "y"), formats = "pdf", quiet = TRUE, check = FALSE)
  expect_equal(m$width_mm, 183)
  expect_equal(m$height_mm, 90)
})
