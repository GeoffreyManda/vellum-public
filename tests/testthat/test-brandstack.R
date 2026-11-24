test_that("BrandStack schema exposes five evidence classes and rich provenance", {
  ids <- vellum_identities()
  expect_gte(nrow(ids), 28)
  expect_true(all(ids$provenance %in% c(
    "official_public", "official_legacy", "official_observed",
    "private_inferred", "journal_inspired"
  )))
  expect_true(all(ids$confidence >= 0 & ids$confidence <= 1))
  who <- brand_get("who")
  expect_equal(who$source_version, "WHO Data Design Language 0.9.2")
  expect_true(all(c("layout", "chart_rules", "table_rules", "legal", "sources") %in% names(who)))
  expect_false(who$legal$logos_included)
})

test_that("World Bank corporate identity and analytical data colours remain separate", {
  expect_equal(unname(identity_palette("world_bank", "identity")),
               c("#002244", "#009FDA", "#000000", "#FFFFFF"))
  expect_equal(unname(identity_palette("world_bank", "corporate", n = 4)),
               c("#002244", "#009FDA", "#F05023", "#FDB714"))
  expect_equal(unname(identity_palette("world_bank", "categorical", n = 2)),
               c("#34A7F2", "#FF9800"))
  expect_equal(identity_font("world_bank", "display", resolve = FALSE)[1], "Andes")
  expect_match(brand_get("world_bank")$source_version, "2016")
})

test_that("semantic roles compile explicit analytical states", {
  cols <- brand_semantic("who", c(improved = "positive", unchanged = "neutral",
                                   worsened = "negative"))
  expect_named(cols, c("improved", "unchanged", "worsened"))
  expect_equal(unname(cols["improved"]), "#40BF73")
  expect_error(brand_semantic("who", c(x = "celebratory")), "Unknown semantic role")
  expect_s3_class(scale_colour_semantic(
    "who", c(improved = "positive", worsened = "negative")
  ), "Scale")
})

test_that("brand modes compile one identity for distinct output surfaces", {
  expect_equal(nrow(brand_modes()), 6)
  report <- suppressWarnings(suppressMessages(theme_brand("unicef", "technical_report")))
  slide <- suppressWarnings(suppressMessages(theme_brand("unicef", "executive_slide")))
  expect_s3_class(report, "theme")
  expect_equal(attr(report, "vellum_brand_mode")$venue, "portfolio")
  expect_equal(attr(slide, "vellum_brand_mode")$venue, "slide")
  expect_equal(attr(report, "vellum_identity")$identity, "unicef")
})

test_that("font substitution metadata never distributes a proprietary font", {
  subs <- brand_font_substitutions()
  expect_true(all(c("reference_font", "open_default", "secondary_option") %in% names(subs)))
  kpmg <- brand_font_resolve("KPMG Font")
  expect_length(kpmg, 1)
  expect_equal(attr(kpmg, "requested"), "KPMG Font")
  expect_true("Inter Tight" %in% attr(kpmg, "candidates"))
})

test_that("user profiles register, inherit and reset without mutating parents", {
  on.exit(brand_reset(), add = TRUE)
  spec <- list(
    id = "example_org", name = "Example Organisation",
    provenance = "official_observed", confidence = .81,
    source = "internal evidence ledger", verified_on = as.Date("2026-07-30"),
    fonts = list(body = c("Arial", "sans"), display = c("Arial", "sans"),
                 mono = c("Courier", "mono")),
    colours = c(primary = "#123456", secondary = "#457B9D", ink = "#111111",
                weak = "#666666", paper = "#FFFFFF", panel = "#F7F7F7",
                grid = "#D9D9D9", na = "#B8B8B8"),
    palettes = list(
      categorical = c("#123456", "#E76F51", "#2A9D8F"),
      sequential = c("#EDF4F7", "#6C9BB5", "#123456"),
      diverging = c("#E76F51", "#F7F7F7", "#123456")
    )
  )
  expect_message(brand_register(spec), "Registered brand")
  expect_equal(brand_get("example_org")$confidence, .81)
  child <- brand_extend("example_org", "example_org_country",
                        list(label = "Example Organisation Country"))
  expect_equal(child$label, "Example Organisation Country")
  expect_equal(brand_get("example_org")$name, "example_org")
  brand_reset()
  expect_error(brand_get("example_org"), "Unknown Vellum identity")
})

test_that("brand previews, comparisons and checks are buildable and auditable", {
  p <- suppressWarnings(suppressMessages(brand_preview("world_bank", "identity")))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot_build(p))
  cmp <- brand_compare(c("who", "unicef", "world_bank"))
  expect_equal(nrow(cmp), 3)
  expect_true(all(c("verdict", "min_deutan", "confidence") %in% names(cmp)))
  chk <- suppressMessages(brand_check("bdo"))
  expect_s3_class(chk, "vellum_brand_check")
  expect_equal(chk$severity[chk$id == "provenance"], "warn")
  expect_equal(chk$severity[chk$id == "trademark"], "pass")
})

test_that("global catalogue is broad without inventing unresearched palettes", {
  x <- brand_catalogue()
  expect_gte(nrow(x), 85)
  expect_true(all(x$status %in% c("implemented", "research_pending")))
  expect_equal(x$status[x$identity == "who"], "implemented")
  expect_equal(x$status[x$identity == "imf"], "research_pending")
})

test_that("journal and scientific-domain modes retain physical contracts", {
  dims <- journal_dimensions("nature", "single_column")
  expect_equal(dims$width_mm, 89)
  expect_true(dims$verified)
  th <- suppressWarnings(suppressMessages(journal_theme("nature", "double_column")))
  expect_s3_class(th, "theme")
  expect_equal(attr(th, "vellum_journal_mode")$mode, "double_column")

  domains <- list(
    theme_epidemiology(), theme_clinical_trial(), theme_systematic_review(),
    theme_genomics(), theme_spatial_health(), theme_health_economics(),
    theme_causal_dag(), theme_survival_analysis(), theme_evidence_grade(),
    theme_mechanistic_pathway()
  )
  expect_true(all(vapply(domains, inherits, logical(1), "theme")))
})
