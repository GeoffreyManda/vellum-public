test_that("skill-derived palette collections retain roles and provenance", {
  x <- vellum_palette_collections()
  expect_true(all(c(
    "flow_semantic", "flow_greyscale", "flow_okabe_ito",
    "flow_jama", "flow_lancet", "protein_plus",
    "structured_domains", "structured_severity", "atni",
    "cameo_monochrome"
  ) %in% x$collection))
  expect_true(all(nzchar(x$source)))
  expect_true(all(nzchar(x$intended_use)))
  expect_true(all(nzchar(x$accessibility)))
})

test_that("semantic role palettes return exact named colours", {
  expect_equal(
    vellum_role_palette(
      "flow_semantic", c("cohort", "loss", "death")
    ),
    c(cohort = "#2F5D7C", loss = "#A66A00", death = "#8A2D3B")
  )
  expect_equal(
    vellum_role_palette(
      "protein_plus", c("unadjusted", "adjustment_1", "adjustment_2")
    ),
    c(
      unadjusted = "#2166AC",
      adjustment_1 = "#E08214",
      adjustment_2 = "#969696"
    )
  )
  expect_error(
    vellum_role_palette("atni", "not_a_role"),
    "Unknown role"
  )
})

test_that("collection display palettes never recycle colours", {
  expect_equal(vellum_pal("atni", 4), unname(
    vellum_role_palette("atni")[c("purple", "blue", "green", "yellow")]
  ))
  expect_error(vellum_pal("protein_plus", 4), "provides 3")
  listed <- vellum_palettes("qualitative")
  expect_true(all(c("flow_jama", "atni", "structured_domains") %in%
                    listed$palette))
  expect_true(all(vapply(
    names(vellum:::.vellum_role_collections),
    function(nm) {
      cols <- vellum_role_palette(nm)
      !inherits(try(grDevices::col2rgb(cols), silent = TRUE), "try-error")
    },
    logical(1)
  )))
})
