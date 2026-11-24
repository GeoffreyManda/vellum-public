test_that("every ltc palette resolves and is listed", {
  listed <- vellum_palettes(family = "ltc")
  expect_equal(nrow(listed), 32L)
  expect_true(all(listed$source == "Theodosiou, ltc (MIT)"))
  expect_true(all(nzchar(listed$rationale)))

  for (nm in listed$palette) {
    cols <- vellum_pal(nm)
    expect_length(cols, length(vellum:::.ltc[[nm]]$cols))
    expect_false(inherits(try(grDevices::col2rgb(cols), silent = TRUE), "try-error"))
  }
})

test_that("ltc names do not collide with any other palette registry", {
  others <- c("okabe_ito", "okabeito", "oi",
              names(vellum:::.tol), names(vellum:::.vellum_specs),
              names(vellum:::.vellum_role_collections))
  expect_length(intersect(names(vellum:::.ltc), others), 0L)
  expect_length(vellum_palettes()$palette, length(unique(vellum_palettes()$palette)))
})

test_that("colour values are transcribed exactly from ltc", {
  # Spot-checks against the upstream source, one per thematic group. If these
  # drift, the provenance claim in LICENSE.note is no longer true.
  expect_equal(vellum_pal("maya"),
               c("#3D5A80", "#98C1D9", "#E0FBFC", "#EE6C4D", "#293241"))
  expect_equal(vellum_pal("minou"),
               c("#00798C", "#D1495B", "#EDAE49", "#66A182", "#2E4057", "#8D96A3"))
  expect_equal(vellum_pal("shuggie"),
               c("#5B5F8D", "#9BB29E", "#DA6B51", "#F1DCBA", "#484149"))
  # Diverging palettes are interpolated on the way out, as Tol's are, so compare
  # the values without the ramp's spec attribute.
  expect_equal(as.character(vellum_pal("heatmap3")),
               c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"))
  # Declared provenance corrections.
  expect_equal(vellum_pal("trio3"), okabe_ito(3))
})

test_that("the heatmap ramps are typed as diverging and the rest as qualitative", {
  listed <- vellum_palettes(family = "ltc")
  div <- listed$palette[listed$kind == "diverging"]
  expect_setequal(div, c("heatmap0", "heatmap1", "heatmap2", "heatmap3"))
  expect_true(all(listed$kind[!listed$palette %in% div] == "qualitative"))

  # Diverging ltc palettes interpolate by default, exactly as Tol's do;
  # qualitative ones refuse, because an invented intermediate is not a category.
  expect_length(vellum_pal("heatmap0", 40), 40L)
  expect_error(vellum_pal("shuggie", 40), "defines 5 colours")
})

test_that("the recorded accessibility figures are measurements, not assertions", {
  # Vellum's whole claim about colour is that the numbers are reproducible. If
  # a palette's recorded worst case drifts from what the audit measures, the
  # catalogue is lying to the reader, so recompute all 32 here.
  worst_qual <- function(cols) {
    min(vapply(c("deutan", "protan", "tritan"), function(cd) {
      m <- vellum:::delta_e(vellum:::cvd(cols, cd, severity = 1))
      diag(m) <- Inf
      min(m)
    }, numeric(1)))
  }
  worst_div <- function(cols) {
    min(vapply(c("deutan", "protan", "tritan"), function(cd) {
      vellum:::arm_separation(vellum:::cvd(cols, cd, severity = 1))
    }, numeric(1)))
  }
  for (nm in names(vellum:::.ltc)) {
    x <- vellum:::.ltc[[nm]]
    measured <- if (x$type == "div") worst_div(x$cols) else worst_qual(x$cols)
    expect_equal(round(measured, 1), x$cvd, tolerance = 0.05,
                 label = paste0("recorded cvd for '", nm, "'"))
    if (x$type != "div") {
      expect_equal(as.integer(palette_max_safe(x$cols)), x$safe_n,
                   label = paste0("recorded safe_n for '", nm, "'"))
    }
  }
})

test_that("the catalogue states the accessibility cost in the rationale", {
  listed <- vellum_palettes(family = "ltc")
  qual <- vellum:::.ltc[vapply(vellum:::.ltc, function(x) x$type != "div", logical(1))]
  below <- vapply(qual, function(x) x$cvd < vellum:::.audit_thresholds$qual_cvd_pass,
                  logical(1))
  # The file comment claims eleven of twenty-eight. Hold it to that.
  expect_equal(length(qual), 28L)
  expect_equal(sum(below), 11L)

  for (nm in names(qual)[below]) {
    expect_match(listed$rationale[listed$palette == nm], "BELOW vellum's floor",
                 fixed = TRUE)
  }
  expect_match(listed$rationale[listed$palette == "sylvie"], "2.3", fixed = TRUE)
  expect_match(listed$rationale[listed$palette == "heatmap2"], "ColorBrewer RdBu",
               fixed = TRUE)
})

test_that("ltc palettes pass through the rest of the colour machinery", {
  expect_equal(pal_na("shuggie"), "#BDBDBD")
  expect_s3_class(palette_audit("alger"), "vellum_audit")
  expect_true("dora" %in% vellum_palettes_supported("qualitative"))
  expect_true("heatmap2" %in% vellum_palettes_supported("diverging", "ltc"))
  expect_false("heatmap2" %in% vellum_palettes_supported("diverging", "tol"))
  expect_no_error(ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = 1:3, y = 1:3, g = letters[1:3]),
                    ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point() +
      scale_colour_vellum("trio1")
  ))
})
