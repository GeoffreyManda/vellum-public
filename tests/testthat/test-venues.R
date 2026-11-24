test_that("Nature's specification matches its published artwork guide", {
  # These numbers are quoted from Nature Portfolio's "Guide to Preparing Final
  # Artwork". If this test fails, either the registry drifted or the guide changed;
  # in both cases a human must check the source before the test is amended.
  n <- venue_spec("nature")
  expect_equal(unname(n$widths[["single"]]), 89)
  expect_equal(unname(n$widths[["double"]]), 183)
  expect_equal(unname(n$widths[["onehalf"]]), 120)
  expect_equal(unname(n$widths[["onehalf_wide"]]), 136)
  expect_equal(n$height_max, 247)
  expect_equal(n$text_min, 5)
  expect_equal(n$text_max, 7)
  expect_equal(n$tag_size, 8)
  expect_equal(n$tag_case, "lower")
  expect_equal(n$tag_face, "bold")
  expect_true(all(c("Helvetica", "Arial") %in% n$families))
  expect_true(all(c("jpeg", "tiff", "png") %in% n$formats_rejected))
  expect_true(n$verified)
})

test_that("only sourced venue specifications ship verified", {
  v <- venues()
  expect_true(v$verified[v$venue == "nature"])
  expect_true(v$verified[v$venue == "plos"])
  expect_true(v$verified[v$venue == "portfolio"])
  expect_equal(sum(v$verified), 3L)
  for (nm in setdiff(v$venue, c("nature", "plos", "portfolio"))) {
    expect_false(venue_spec(nm)$verified, info = nm)
  }
})

test_that("portfolio plates have an explicit internal specification", {
  p <- venue_spec("portfolio")
  expect_true(p$verified)
  expect_equal(unname(p$widths[["full"]]), 250)
  expect_equal(p$height_max, 160)
  expect_equal(c(p$text_min, p$text_max), c(8, 11))
  expect_match(p$source, "internal consultancy")
})

test_that("PLOS dimensions match its current publisher guide", {
  p <- venue_spec("plos")
  expect_true(p$verified)
  expect_equal(unname(p$widths[c("min", "single", "max")]),
               c(66.8, 132, 190.5))
  expect_equal(p$height_max, 222.3)
  expect_equal(c(p$text_min, p$text_max), c(8, 12))
  expect_equal(p$formats_preferred, c("tiff", "eps"))
  expect_equal(unname(p$dpi[c("line", "combination", "halftone")]),
               c(600, 600, 300))
  expect_match(p$source, "journals[.]plos[.]org")
  expect_equal(p$retrieved, "2026-07-30")
})

test_that("unverified venues warn once per session", {
  vellum_reset_notices()
  collect_warnings <- function(expr) {
    messages <- character()
    withCallingHandlers(
      force(expr),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    messages
  }

  first <- collect_warnings(theme_venue("cell"))
  second <- collect_warnings(theme_venue("cell"))
  expect_true(any(grepl("unverified", first, fixed = TRUE)))
  # Device/font warnings can depend on the graphics backend; the venue notice
  # itself must still be emitted only once.
  expect_false(any(grepl("unverified", second, fixed = TRUE)))
  vellum_reset_notices()
})

test_that("aliases resolve to the same object, not a copy", {
  expect_identical(venue_spec("nature_portfolio"), venue_spec("nature"))
  expect_identical(venue_spec("NPG"), venue_spec("nature"))
  expect_identical(venue_spec("cell_press"), venue_spec("cell"))
})

test_that("venue_width accepts a name or a number", {
  expect_equal(venue_width("nature", "single"), 89)
  expect_equal(venue_width("nature", 123), 123)
  expect_error(venue_width("nature", "quadruple"), "not a defined width")
  expect_error(venue_spec("Journal of Nowhere"), "Unknown venue")
})

test_that("venue_register overrides and venue_reset restores", {
  on.exit(venue_reset(), add = TRUE)
  original <- venue_width("nature", "single")
  suppressMessages(venue_register("nature", widths = c(single = 90, double = 184),
                                  height_max = 250, verified = TRUE))
  expect_equal(venue_width("nature", "single"), 90)
  venue_reset()
  expect_equal(venue_width("nature", "single"), original)
})

test_that("venue_register rejects widths without a single column", {
  expect_error(venue_register("bad", widths = c(double = 180), height_max = 200),
               "including")
})

test_that("venue_summary filters correctly and returns summary data frame", {
  s_all <- venue_summary()
  expect_s3_class(s_all, "vellum_venue_summary")
  expect_true(nrow(s_all) >= 10)

  s_sub <- venue_summary(c("nature", "plos"))
  expect_equal(nrow(s_sub), 2)
  expect_equal(s_sub$venue, c("nature", "plos"))
})
