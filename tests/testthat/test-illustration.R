demo_svg <- function(stroke = "0.5") {
  svg_read(sprintf(paste0(
    '<svg viewBox="0 0 100 50"><rect width="100" height="50" fill="#B15928"/>',
    '<path d="M0,0 L100,50" stroke="#333333" stroke-width="%s" fill="none"/></svg>'), stroke))
}

test_that("an illustration draws as vector paths, not as a raster", {
  g <- asset_grob(demo_svg())
  expect_s3_class(g, "vellum_asset")
  drawn <- grid::grid.force(g)
  kids <- grid::childNames(drawn)
  expect_true(length(kids) > 0)
  expect_false(any(grepl("raster", kids, ignore.case = TRUE)))
})

test_that("stroke weight is resolved from the size actually drawn", {
  svg <- demo_svg("0.5")
  # 0.5 user units on a 100-unit canvas drawn 100 mm wide is 0.5 mm.
  expect_equal(asset_measure(svg, width = 100)$min_linewidth_pt, mm_pt(0.5),
               tolerance = 1e-6)
  # Halve the drawn width and the weight halves with it.
  expect_equal(asset_measure(svg, width = 50)$min_linewidth_pt, mm_pt(0.25),
               tolerance = 1e-6)
  expect_equal(asset_measure(svg, width = 100)$height_mm, 50)
})

test_that("the drawing keeps its proportions", {
  svg <- demo_svg()
  expect_equal(asset_measure(svg, width = 60)$height_mm, 30)
})

test_that("ink recolouring collapses to one hue and keeps lightness order", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><rect width="1" height="1" fill="#111111"/>',
    '<rect width="1" height="1" fill="#DDDDDD"/></svg>'))
  out <- asset_recolour(svg, ink = "#1B3A5C", preserve = character())
  cols <- vapply(out$shapes, `[[`, character(1), "fill")
  expect_length(unique(cols), 2L)
  # The dark shape stays the darker of the two.
  expect_lt(lightness(cols[1]), lightness(cols[2]))
})

test_that("recolouring preserves white by default and can be told not to", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><rect width="1" height="1" fill="#FFFFFF"/>',
    '<rect width="1" height="1" fill="#000000"/></svg>'))
  kept <- asset_recolour(svg, ink = "#AA0000")
  expect_equal(kept$shapes[[1]]$fill, "#FFFFFF")
  changed <- asset_recolour(svg, ink = "#AA0000", preserve = character())
  expect_false(identical(changed$shapes[[1]]$fill, "#FFFFFF"))
})

test_that("an explicit colour map renames only what it names", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><rect width="1" height="1" fill="#CC0000"/>',
    '<rect width="1" height="1" fill="#00CC00"/></svg>'))
  out <- asset_recolour(svg, map = c("#CC0000" = "#123456"))
  expect_equal(out$shapes[[1]]$fill, "#123456")
  expect_equal(out$shapes[[2]]$fill, "#00CC00")
})

test_that("the three recolouring modes are mutually exclusive", {
  expect_error(asset_recolour(demo_svg(), ink = "#000000", palette = "okabe_ito"),
               "only one")
})

test_that("SVG line joins are translated to grid's spelling", {
  # SVG says "miter", grid says "mitre", and passing the SVG spelling straight
  # through is an error at draw time.
  expect_equal(asset_linejoin("miter"), "mitre")
  expect_equal(asset_linejoin("round"), "round")
  expect_equal(asset_linejoin(NULL), "mitre")
  expect_equal(asset_linecap("square"), "square")
})

test_that("annotation_asset needs a position and accepts either kind", {
  svg <- demo_svg()
  expect_error(annotation_asset(svg), "position")
  expect_s3_class(annotation_asset(svg, x_npc = 0.5, y_npc = 0.5, width = 10), "Layer")
  expect_s3_class(annotation_asset(svg, xmin = 1, xmax = 2, ymin = 1, ymax = 2), "Layer")
})

test_that("illustrations placed in a plot are found and measured by pre-flight", {
  svg <- demo_svg("0.4")
  thin <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotation_asset(svg, x_npc = 0.8, y_npc = 0.8, width = 8) +
    theme_vellum()
  thick <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotation_asset(svg, x_npc = 0.8, y_npc = 0.8, width = 60) +
    theme_vellum()

  expect_length(illustration_layers(thin), 1L)
  expect_equal(illustration_layers(thin)[[1]]$width_mm, 8)

  # The identical drawing fails at 8 mm and passes at 60 mm, which is the whole
  # point of keeping it as measurable geometry.
  expect_equal(venue_check(thin, "nature", quiet = TRUE)$checks$art_linewidth$verdict, "fail")
  expect_equal(venue_check(thick, "nature", quiet = TRUE)$checks$art_linewidth$verdict, "pass")
})

test_that("pre-flight says so when artwork carries no provenance", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotation_asset(demo_svg(), x_npc = 0.5, y_npc = 0.5, width = 20) +
    theme_vellum()
  expect_equal(venue_check(p, "nature", quiet = TRUE)$checks$art_licence$verdict, "warn")
})

test_that("a plot with no illustration gains no illustration checks", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() + theme_vellum()
  expect_length(illustration_layers(p), 0L)
  expect_null(venue_check(p, "nature", quiet = TRUE)$checks$art_linewidth)
})

demo_png <- function(w = 40, h = 20) {
  f <- tempfile(fileext = ".png")
  png::writePNG(array(runif(w * h * 3), dim = c(h, w, 3)), f)
  f
}

test_that("raster artwork loads and reports itself as raster", {
  skip_if_not_installed("png")
  r <- raster_read(demo_png(), name = "Test plate", source = "Bank")
  expect_s3_class(r, "vellum_raster")
  expect_equal(r$width_px, 40)
  expect_equal(r$height_px, 20)
  expect_equal(r$provenance$name, "Test plate")
  expect_error(raster_read(tempfile(fileext = ".jpg")), "must be a PNG")
})

test_that("effective resolution falls as a raster is enlarged", {
  skip_if_not_installed("png")
  r <- raster_read(demo_png(w = 600))
  # 600 px across 25.4 mm is 600 dpi; across 50.8 mm it is 300.
  expect_equal(raster_dpi(r, 25.4), 600)
  expect_equal(raster_dpi(r, 50.8), 300)
})

test_that("raster artwork is held to resolution, not to rule weight", {
  skip_if_not_installed("png")
  r <- raster_read(demo_png(w = 300))
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotation_asset(r, x_npc = 0.5, y_npc = 0.5, width = 60) +
    theme_vellum()
  rep <- venue_check(p, "nature", content = "combination", quiet = TRUE)
  # 300 px across 60 mm is about 127 dpi, well under Nature's 600.
  expect_equal(rep$checks$art_raster$verdict, "fail")
  # It must not be judged by the vector rules.
  expect_null(rep$checks$art_linewidth)
})

test_that("illustration_report names the binding illustration", {
  skip_if_not_installed("png")
  small <- raster_read(demo_png(w = 100), name = "Low")
  big <- raster_read(demo_png(w = 2000), name = "High")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    annotation_asset(small, x_npc = 0.3, y_npc = 0.5, width = 20) +
    annotation_asset(big, x_npc = 0.7, y_npc = 0.5, width = 20) +
    theme_vellum()
  tbl <- illustration_report(p)
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$illustration[1], "Low")   # worst first
  expect_true(all(tbl$kind == "raster"))
})

test_that("a diagram node accepts raster artwork", {
  skip_if_not_installed("png")
  r <- raster_read(demo_png(), name = "Plate")
  d <- diagram(
    list(dg_node("a", "State", x = 0, y = 0, width = 30, height = 14),
         dg_node("art", "", asset = r, x = -10, y = 0, width = 8)),
    layout = "manual")
  expect_s3_class(d, "ggplot")
  expect_length(illustration_layers(d), 1L)
  expect_error(dg_node("x", "", asset = 42), "must be an illustration")
})
