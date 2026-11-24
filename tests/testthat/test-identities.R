test_that("identity registry distinguishes official, observed and editorial systems", {
  ids <- vellum_identities()
  expect_gte(nrow(ids), 19)
  expect_true(all(c("institution", "journal") %in% unique(ids$type)))
  expect_equal(ids$status[ids$identity == "who"], "official-data-system")
  expect_equal(ids$status[ids$identity == "unicef"], "official")
  expect_equal(ids$status[ids$identity == "bdo"], "observed")
  expect_equal(ids$status[ids$identity == "nature"], "editorial-overlay")
  expect_true(all(nzchar(ids$source)))
  expect_true(all(nzchar(ids$retrieved)))
})

test_that("official identity tokens retain exact source colours", {
  expect_equal(unname(identity_palette("who", "categorical", n = 2)),
               c("#F4A81D", "#F26829"))
  expect_equal(unname(identity_palette("unicef", "core")["primary"]), "#1CABE2")
  expect_equal(unname(identity_palette("usaid", "core")["primary"]), "#002F6C")
  expect_equal(unname(identity_palette("undp", "categorical", n = 3)),
               c("#006EB5", "#5DD4F0", "#02A38A"))
  expect_equal(unname(identity_palette("kpmg", "core")["primary"]), "#00338D")
})

test_that("identity palettes never recycle and continuous ramps interpolate", {
  expect_error(identity_palette("who", "categorical", n = 7),
               "does not silently recycle")
  expect_length(identity_palette("who", "sequential", n = 31), 31)
  expect_length(identity_palette("world_bank", "diverging", n = 17), 17)
  expect_equal(identity_palette("who", "categorical", n = 3, rev = TRUE),
               rev(identity_palette("who", "categorical", n = 3)))
})

test_that("identity fonts retain requested and resolved families", {
  requested <- identity_font("unicef", "body", resolve = FALSE)
  expect_equal(requested[1], "Univers LT Pro")
  resolved <- suppressMessages(identity_font("unicef", "body"))
  expect_length(resolved, 1)
  expect_true(nzchar(resolved))
  expect_equal(attr(resolved, "requested")[1], "Univers LT Pro")
  expect_true(is.logical(attr(resolved, "substituted")))
})

test_that("identity tokens and themes carry provenance independently of venue", {
  tk <- suppressMessages(identity_tokens("who", venue = "portfolio"))
  expect_s3_class(tk, "vellum_tokens")
  expect_equal(tk$meta$identity, "who")
  expect_equal(tk$meta$identity_status, "official-data-system")
  expect_equal(tk$ink$accent, "#008DC9")
  expect_equal(tk$meta$venue, "portfolio")

  th <- suppressMessages(theme_institution("who", venue = "portfolio"))
  expect_s3_class(th, "theme")
  expect_equal(attr(th, "vellum_identity")$identity, "who")
  expect_equal(attr(th, "vellum_tokens")$meta$venue, "portfolio")

  jt <- suppressWarnings(suppressMessages(theme_journal("nature")))
  expect_equal(attr(jt, "vellum_identity")$mode, "accessible")
  expect_equal(attr(jt, "vellum_tokens")$meta$venue, "nature")
})

test_that("identity themes and scales build plots", {
  p1 <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() + suppressWarnings(suppressMessages(
      theme_institution("atni", venue = "portfolio")))
  p2 <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() + scale_colour_brand("unicef") +
    suppressWarnings(suppressMessages(theme_journal("plos")))
  p3 <- ggplot(ggplot2::faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() + scale_fill_brand_c("world_bank") +
    suppressWarnings(suppressMessages(
      theme_institution("world_bank", venue = "portfolio")))
  expect_silent(ggplot_build(p1))
  expect_silent(ggplot_build(p2))
  expect_silent(ggplot_build(p3))
})

test_that("identity audit exposes palette, contrast, font and provenance evidence", {
  x <- suppressMessages(identity_audit("who"))
  expect_s3_class(x, "vellum_identity_audit")
  expect_s3_class(x$palette, "vellum_audit")
  expect_gt(x$text_contrast, 4.5)
  expect_match(x$source, "who.int")
  expect_true(nzchar(x$body_resolved))
})
