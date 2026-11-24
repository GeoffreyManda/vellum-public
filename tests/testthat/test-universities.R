test_that("university registry preserves the reproducible 6/24/20 collections", {
  x <- vellum_university_brands()
  expect_equal(nrow(x), 50)
  expect_identical(anyDuplicated(x$id), 0L)
  expect_equal(
    as.integer(table(x$group)[c("malawi_public", "russell_group", "usa_qs2027_top20")]),
    c(6L, 24L, 20L)
  )
  expect_equal(x$id[x$group == "malawi_public"],
               c("kuhes", "luanar", "mubas", "must_mw", "mzuni", "unima"))
  expect_equal(utils::tail(x$id[x$group == "usa_qs2027_top20"], 2),
               c("duke", "ut_austin"))
})

test_that("all university profiles carry complete role, source and legal records", {
  reg <- vellum_university_registry()
  roles <- c("primary", "secondary", "accent", "support_1", "support_2",
             "ink", "muted", "rule", "surface", "background")
  expect_true(all(vapply(reg$profiles, function(x) all(roles %in% names(x$colours)),
                         logical(1))))
  colours <- unlist(lapply(reg$profiles, `[[`, "colours"), use.names = FALSE)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colours)))
  expect_true(all(vapply(reg$profiles, function(x) length(x$sources) >= 1L, logical(1))))
  expect_false(any(vapply(reg$profiles, function(x) isTRUE(x$legal$bundle_logos), logical(1))))
  expect_false(any(vapply(reg$profiles, function(x) isTRUE(x$legal$autoload_marks), logical(1))))
})

test_that("source provenance maps into BrandStack without losing detail", {
  kuhes <- vellum_university_brand("kuhes")
  cambridge <- vellum_university_brand("cambridge")
  expect_equal(kuhes$source_provenance, "website_inferred")
  expect_equal(kuhes$brand_provenance, "official_observed")
  expect_equal(cambridge$source_provenance, "official_public")
  expect_equal(cambridge$brand_provenance, "official_public")
  listed <- vellum_university_brands(provenance = "website_inferred")
  expect_true(nrow(listed) > 0)
  expect_true(all(listed$brand_provenance == "official_observed"))
})

test_that("university palettes retain requested roles and never recycle", {
  expect_equal(
    vellum_university_palette("oxford", c("primary", "accent")),
    c(primary = "#002147", accent = "#BEA55D")
  )
  expect_error(vellum_university_palette("oxford", "harm"), "Unknown palette roles")
  scale <- scale_colour_university_d("oxford")
  expect_equal(length(scale$palette(5)), 5)
  expect_error(scale$palette(6), "provides 5")
  expect_s3_class(scale_fill_university_d("stanford"), "Scale")
  expect_identical(scale_color_university_d, scale_colour_university_d)
})

test_that("font APIs preserve reference faces and auditable open substitutes", {
  open <- vellum_university_fonts("cambridge")
  reference <- vellum_university_fonts("cambridge", prefer_reference = TRUE)
  expect_equal(unname(open[c("display", "body")]), c("Source Serif 4", "Open Sans"))
  expect_false(identical(open[["display"]], reference[["display"]]))
  resolved <- vellum_university_font_resolve("cambridge", "display")
  expect_length(resolved, 1)
  expect_true(!is.null(attr(resolved, "requested")))
  expect_true("Source Serif 4" %in% attr(resolved, "candidates"))
})

test_that("all university profiles compile through Vellum output modes", {
  ids <- vellum_university_brands()$id
  themes <- suppressWarnings(lapply(ids, function(id) theme_vellum_university(id, "report")))
  expect_true(all(vapply(themes, inherits, logical(1), "theme")))
  expect_true(all(vapply(themes, function(x) {
    identical(attr(x, "vellum_university")$mode, "technical_report")
  }, logical(1))))
  expect_equal(attr(suppressWarnings(theme_vellum_university("ucla", "slide")),
                    "vellum_university")$mode, "executive_slide")
  expect_equal(attr(suppressWarnings(theme_vellum_university("ucla", "dashboard")),
                    "vellum_university")$colour, "accessible")
})

test_that("university role collections integrate with the Vellum registry", {
  x <- vellum_palette_collections()
  university <- x[grepl("^university_", x$collection), , drop = FALSE]
  expect_equal(nrow(university), 50)
  expect_equal(
    vellum_role_palette("university_cambridge", c("primary", "secondary", "accent")),
    vellum_university_palette("cambridge", c("primary", "secondary", "accent"))
  )
  expect_equal(vellum_pal("university_cambridge", 5),
               unname(vellum_university_palette(
                 "cambridge", c("primary", "secondary", "accent", "support_1", "support_2")
               )))
})

test_that("university audits expose contrast, CVD and external-review safeguards", {
  ids <- vellum_university_brands()$id
  audits <- do.call(rbind, lapply(ids, vellum_university_audit))
  expect_equal(nrow(audits), 50)
  expect_true(all(audits$text_contrast_pass))
  expect_true(all(is.finite(audits$min_deuteranopia)))
  expect_false(any(audits$logos_bundled))
  expect_true(all(audits$external_review_recommended[
    audits$provenance %in% c("private_inferred", "website_inferred")
  ]))
})
