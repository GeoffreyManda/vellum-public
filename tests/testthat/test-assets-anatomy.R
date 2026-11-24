make_corpus <- function(licence = "CC0-1.0", embedded = NULL) {
  dir <- tempfile("corpus")
  dir.create(file.path(dir, "icons"), recursive = TRUE)
  meta <- if (is.null(embedded)) "" else
    sprintf("<metadata><license>%s</license><creator>A Person</creator></metadata>", embedded)
  writeLines(sprintf('<svg viewBox="0 0 8 8">%s<circle cx="4" cy="4" r="3" fill="#333333"/></svg>', meta),
             file.path(dir, "icons", "dot.svg"))
  writeLines(sprintf(paste0(
    '{"source":"Demo","sourceUrl":"https://example.org","assets":[',
    '{"id":"demo:dot","name":"Round Cell","category":"Cells","tags":["cell","round"],',
    '"license":"%s","author":"A Person","authorUrl":"","source":"Demo",',
    '"sourceUrl":"https://example.org/dot","svgPath":"icons/dot.svg"}]}'), licence),
    file.path(dir, "manifest.json"))
  dir
}

withr_corpus <- function(code, ...) {
  asset_corpus_remove()
  on.exit(asset_corpus_remove(), add = TRUE)
  force(code)
}

test_that("a corpus registers and its assets are searchable", {
  withr_corpus({
    suppressMessages(asset_corpus_register("demo", make_corpus()))
    expect_equal(nrow(asset_corpora()), 1L)
    expect_equal(asset_corpora()$assets, 1L)
    expect_equal(nrow(asset_search("cell")), 1L)
    expect_equal(nrow(asset_search("round cell")), 1L)
    expect_equal(nrow(asset_search("nonexistent")), 0L)
  })
})

test_that("registration fails loudly rather than silently producing nothing", {
  asset_corpus_remove()
  expect_error(asset_corpus_register("x", tempfile()), "No manifest")
  expect_error(asset_search("anything"), "No asset corpus is registered")
})

test_that("a manifest missing required fields is refused", {
  dir <- tempfile(); dir.create(dir)
  writeLines('{"source":"X","assets":[{"id":"a","svgPath":"a.svg"}]}',
             file.path(dir, "manifest.json"))
  expect_error(asset_corpus_register("x", dir), "missing required field")
})

test_that("a loaded asset carries its provenance", {
  withr_corpus({
    suppressMessages(asset_corpus_register("demo", make_corpus()))
    a <- vellum_asset("demo:dot")
    expect_s3_class(a, "vellum_svg")
    expect_equal(a$provenance$licence_recorded, "CC0-1.0")
    expect_false(a$provenance$licence_disputed)
  })
})

test_that("a licence the file disputes is reported rather than resolved", {
  withr_corpus({
    suppressMessages(asset_corpus_register(
      "demo", make_corpus(licence = "CC-BY-4.0", embedded = "Public Domain")))
    a <- vellum_asset("demo:dot")
    expect_true(a$provenance$licence_disputed)
    expect_equal(a$provenance$licence_stated_in_file, "Public Domain")
    led <- asset_licence(a)
    expect_equal(attr(led, "verdict"), "review")
  })
})

test_that("licences are compared by meaning, not by spelling", {
  expect_equal(asset_licence_key("CC0-1.0"), asset_licence_key("CC0 1.0"))
  expect_equal(asset_licence_key("CC-BY-4.0"), asset_licence_key("CC BY 4.0"))
  expect_false(identical(asset_licence_key("Public Domain"), asset_licence_key("CC-BY-4.0")))
})

test_that("share-alike is flagged separately from ordinary attribution", {
  expect_true(asset_is_share_alike("CC-BY-SA-4.0"))
  expect_false(asset_is_share_alike("CC-BY-4.0"))
  withr_corpus({
    suppressMessages(asset_corpus_register("demo", make_corpus(licence = "CC-BY-SA-4.0")))
    led <- asset_licence(vellum_asset("demo:dot"))
    expect_true(led$share_alike)
    expect_equal(led$verdict, "review")
  })
})

test_that("the ledger separates clear from attribution-bearing assets", {
  withr_corpus({
    suppressMessages(asset_corpus_register("cc0", make_corpus(licence = "CC0-1.0")))
    expect_equal(asset_licence(vellum_asset("demo:dot"))$verdict, "clear")
    expect_length(asset_attribution(vellum_asset("demo:dot")), 0L)
  })
  withr_corpus({
    suppressMessages(asset_corpus_register("by", make_corpus(licence = "CC-BY-4.0")))
    a <- vellum_asset("demo:dot")
    expect_equal(asset_licence(a)$verdict, "attribution")
    credit <- asset_attribution(a)
    expect_match(credit, "Round Cell")
    expect_match(credit, "CC-BY-4.0")
  })
})

test_that("asking for an asset that is not registered says so usefully", {
  withr_corpus({
    suppressMessages(asset_corpus_register("demo", make_corpus()))
    expect_error(vellum_asset("demo:nothing"), "No registered asset")
  })
})

test_that("an illustration without provenance cannot produce a licence ledger", {
  svg <- svg_read('<svg viewBox="0 0 4 4"><rect width="4" height="4" fill="#000"/></svg>')
  expect_error(asset_licence(svg), "no provenance")
})

test_that("anatomical conventions are stated as conventions", {
  tbl <- anatomy_palette()
  expect_true(all(tbl$basis == "convention"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", tbl$colour)))
  expect_equal(unname(anatomy_palette("arterial")), tbl$colour[tbl$structure == "arterial"])
  expect_error(anatomy_palette("spleenish"), "Unknown structure")
})

test_that("arterial and venous survive greyscale printing", {
  # The pair most likely to be printed in black and white is the pair a reader
  # most needs to tell apart.
  sep <- attr(anatomy_audit(c("arterial", "venous")), "arteriovenous")
  expect_gt(sep[["greyscale"]], 15)
  expect_gt(sep[["colour"]], 30)
})

test_that("colours that cannot carry their own boundary say so", {
  tbl <- anatomy_palette()
  # A pale tissue fill is correct for a large area and useless for a thin one, so
  # the set records which fills need an outline rather than darkening them into
  # colours a reader would not recognise.
  expect_true(all(contrast_ratio(tbl$colour[!tbl$needs_outline], "#FFFFFF") >= 3))
  expect_true(all(contrast_ratio(tbl$colour[tbl$needs_outline], "#FFFFFF") < 3))
  expect_true(tbl$needs_outline[tbl$structure == "bone"])
  expect_false(tbl$needs_outline[tbl$structure == "venous"])
})

test_that("anatomy_recolour maps stated colours and records what it did", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><rect width="4" height="10" fill="#CC0000"/>',
    '<rect x="5" width="4" height="10" fill="#0000CC"/></svg>'))
  out <- anatomy_recolour(svg, c("#CC0000" = "arterial", "#0000CC" = "venous"))
  cols <- vapply(out$shapes, `[[`, character(1), "fill")
  expect_equal(cols[1], unname(anatomy_palette("arterial")))
  expect_equal(cols[2], unname(anatomy_palette("venous")))
  expect_equal(nrow(out$anatomy), 2L)
})

test_that("mapping a colour the drawing does not contain warns rather than silently doing nothing", {
  svg <- svg_read('<svg viewBox="0 0 10 10"><rect width="4" height="10" fill="#CC0000"/></svg>')
  expect_warning(anatomy_recolour(svg, c("#00FF00" = "nerve")), "not in the drawing")
  expect_error(anatomy_recolour(svg, c("#CC0000" = "elbow")), "Unknown structure")
})

test_that("skin tones progress evenly in lightness and are never a scale", {
  s <- anatomy_skin(6)
  expect_length(s, 6L)
  L <- lightness(s)
  expect_true(all(diff(L) < 0))
  expect_error(anatomy_skin(1), "at least")
  expect_error(anatomy_skin(20), "at most")
})

make_raster_corpus <- function() {
  dir <- tempfile("rcorpus")
  dir.create(file.path(dir, "images"), recursive = TRUE)
  png::writePNG(array(runif(60 * 40 * 3), dim = c(40, 60, 3)),
                file.path(dir, "images", "plate.png"))
  writeLines(paste0(
    '{"source":"Image Bank","sourceUrl":"https://example.org","assets":[',
    '{"id":"bank:plate","name":"Feeding plate","category":"Nutrition",',
    '"tags":["feeding"],"license":"Bank terms; credit required",',
    '"author":"A Photographer","source":"Image Bank",',
    '"sourceUrl":"https://example.org/plate","pngPath":"images/plate.png",',
    '"assetFormat":"raster"}]}'), file.path(dir, "manifest.json"))
  dir
}

test_that("a raster corpus registers and loads as raster", {
  skip_if_not_installed("png")
  withr_corpus({
    suppressMessages(asset_corpus_register("bank", make_raster_corpus()))
    expect_equal(asset_corpora()$assets, 1L)
    a <- vellum_asset("bank:plate")
    # Loading a raster through the SVG reader would fail with a parse error that
    # says nothing about why.
    expect_s3_class(a, "vellum_raster")
    expect_equal(a$width_px, 60)
    expect_equal(a$provenance$name, "Feeding plate")
    expect_equal(a$provenance$corpus, "bank")
  })
})

test_that("a raster asset can produce a licence ledger and a credit line", {
  skip_if_not_installed("png")
  withr_corpus({
    suppressMessages(asset_corpus_register("bank", make_raster_corpus()))
    a <- vellum_asset("bank:plate")
    led <- asset_licence(a)
    expect_equal(nrow(led), 1L)
    # Unrecognised terms are for review, not quietly waved through as clear.
    expect_equal(led$verdict, "review")
    expect_match(asset_attribution(a), "A Photographer")
  })
})

test_that("asset_search finds raster assets alongside vector ones", {
  skip_if_not_installed("png")
  withr_corpus({
    suppressMessages(asset_corpus_register("bank", make_raster_corpus()))
    suppressMessages(asset_corpus_register("demo", make_corpus()))
    expect_equal(nrow(asset_search("feeding")), 1L)
    expect_equal(nrow(asset_search("cell")), 1L)
  })
})
