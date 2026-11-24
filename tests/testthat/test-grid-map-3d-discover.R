library(ggplot2)

test_that("the grid flag string controls each gridline independently", {
  on_off <- function(th, nm) !inherits(calc_element(nm, th), "element_blank")
  cases <- list(
    "none"  = c(FALSE, FALSE, FALSE, FALSE),
    "x"     = c(TRUE,  FALSE, FALSE, FALSE),
    "y"     = c(FALSE, TRUE,  FALSE, FALSE),
    "both"  = c(TRUE,  TRUE,  FALSE, FALSE),
    "Yy"    = c(FALSE, TRUE,  FALSE, TRUE),
    "XYxy"  = c(TRUE,  TRUE,  TRUE,  TRUE),
    "Xy"    = c(TRUE,  FALSE, FALSE, TRUE)
  )
  els <- c("panel.grid.major.x", "panel.grid.major.y",
           "panel.grid.minor.x", "panel.grid.minor.y")
  for (g in names(cases)) {
    th <- suppressWarnings(theme_vellum(grid = g))
    expect_equal(vapply(els, on_off, logical(1), th = th), cases[[g]],
                 ignore_attr = TRUE, info = g)
  }
  expect_error(theme_vellum(grid = "Q"), "grid flag")
})

test_that("a blank grid parent does not silently blank its children", {
  # theme_vellum() declares complete = TRUE, which forces inherit.blank = TRUE onto
  # every element. Setting panel.grid = element_blank() therefore blanked all four
  # children however they were specified. This is that regression.
  th <- suppressWarnings(theme_vellum(grid = "both"))
  expect_false(inherits(calc_element("panel.grid.major.y", th), "element_blank"))
  expect_false(inherits(th$panel.grid, "element_blank"))

  gt <- ggplot_gtable(ggplot_build(
    ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum(grid = "Yy")))
  panel <- gt$grobs[[which(gt$layout$name == "panel")]]
  expect_true(any(grepl("grill", names(panel$children))))
})

test_that("secondary axes get margins pointing the right way", {
  # A sec_axis on the right must not inherit the left axis's right-hand margin, which
  # would push its labels toward the panel instead of away from it.
  th <- complete_theme(theme_venue("nature"))
  right <- calc_element("axis.text.y.right", th)
  left <- calc_element("axis.text.y", th)
  expect_gt(as.numeric(right$margin)[4], 0)   # left margin on a right-hand axis
  expect_gt(as.numeric(left$margin)[2], 0)    # right margin on a left-hand axis
  expect_equal(right$hjust, 0)
  expect_equal(left$hjust, 1)

  top <- calc_element("axis.text.x.top", th)
  expect_gt(as.numeric(top$margin)[3], 0)     # bottom margin on a top axis
})

test_that("theme_vellum_map strips axes and keeps a graticule", {
  th <- theme_vellum_map()
  expect_s3_class(calc_element("axis.text.x", th), "element_blank")
  expect_s3_class(calc_element("axis.title.x", th), "element_blank")
  expect_false(inherits(calc_element("panel.grid.major", th), "element_blank"))
  expect_false(inherits(calc_element("panel.border", th), "element_blank"))

  bare <- theme_vellum_map(graticule = FALSE)
  expect_s3_class(calc_element("panel.grid.major", bare), "element_blank")

  expect_no_error(ggplot_gtable(ggplot_build(
    ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point() + theme_vellum_map())))
})

test_that("proj3d is an orthographic projection with usable depth", {
  # Overhead view collapses z into nothing on the page: the surface becomes a heatmap.
  # theta must be 0 as well, or the x-y plane is still rotated on the page.
  flat <- proj3d(c(0, 1, 0, 1), c(0, 0, 1, 1), c(0, 5, 10, 20), theta = 0, phi = 90)
  expect_equal(flat$X, c(0, 1, 0, 1), tolerance = 1e-9)
  expect_equal(flat$Y, c(0, 0, 1, 1), tolerance = 1e-9)   # z contributes nothing

  # Edge-on view puts z straight onto the page's vertical axis.
  edge <- proj3d(0, 0, c(0, 1), phi = 0)
  expect_equal(edge$Y, c(0, 1), tolerance = 1e-9)

  # Depth must order points: further back is larger.
  d <- proj3d(0, c(0, 1), 0, theta = 0, phi = 25)
  expect_gt(d$depth[2], d$depth[1])

  expect_error(proj3d(0, 0, 0, phi = 120), "at most 90")
})

test_that("fig_surface3d builds from a matrix and from long data", {
  g <- expand.grid(x = seq(-2, 2, length.out = 12), y = seq(-2, 2, length.out = 12))
  g$z <- with(g, sin(x) * cos(y))
  m <- matrix(g$z, 12, 12,
              dimnames = list(seq(-2, 2, length.out = 12), seq(-2, 2, length.out = 12)))

  expect_no_error(ggplot_gtable(ggplot_build(fig_surface3d(g, "x", "y", "z"))))
  expect_no_error(ggplot_gtable(ggplot_build(fig_surface3d(m, midpoint = 0))))
  expect_no_error(ggplot_gtable(ggplot_build(
    fig_surface3d(m, wireframe = FALSE, box = FALSE))))

  # One facet per grid cell, four vertices each.
  built <- ggplot_build(fig_surface3d(m))
  expect_equal(nrow(built$data[[1]]), (12 - 1) * (12 - 1) * 4)

  expect_error(fig_surface3d(g), "name the")
  expect_error(fig_surface3d(matrix(1:2, 1, 2)), "2 x 2")
  expect_error(fig_surface3d(m, aspect = c(1, 1)), "three values")
})

test_that("fig_surface3d sorts facets back to front", {
  # The painter's algorithm is the whole mechanism: without a correct order, near
  # facets are overdrawn by far ones and the surface turns inside out.
  m <- matrix(seq(0, 1, length.out = 25), 5, 5)
  built <- ggplot_build(fig_surface3d(m, box = FALSE))
  d <- built$data[[1]]
  first_of_group <- d[!duplicated(d$group), ]
  # Groups appear in draw order, and that order must be non-increasing in depth.
  expect_equal(first_of_group$group, sort(first_of_group$group))
})

test_that("fig_surface3d drops facets with a missing corner rather than failing", {
  m <- matrix(seq(0, 1, length.out = 36), 6, 6)
  m[3, 3] <- NA
  built <- ggplot_build(fig_surface3d(m, box = FALSE))
  # The four facets touching the hole are gone; 25 - 4 remain.
  expect_equal(nrow(built$data[[1]]) / 4, 21)
  expect_error(
    expect_warning(fig_surface3d(matrix(NA_real_, 4, 4)), NA),
    "missing corner"
  )
})

test_that("fig_surface3d_ci aligns estimate and interval blankets", {
  g <- expand.grid(x = seq(-1, 1, length.out = 12),
                   y = seq(-1, 1, length.out = 12))
  g$p <- with(g, plogis(x - y + x * y))
  g$lo <- pmax(0, g$p - 0.08)
  g$hi <- pmin(1, g$p + 0.08)

  p <- suppressWarnings(
    fig_surface3d_ci(g, "x", "y", "p", "lo", "hi", zlim = c(0, 1))
  )
  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_equal(nrow(built$data[[2]]), (12 - 1)^2 * 4)
  expect_true(all(c("#0072B2", "#D55E00") %in%
                    unique(unlist(lapply(built$data, `[[`, "colour")))))

  bad <- g
  bad$lo[1] <- bad$p[1] + 0.1
  expect_error(fig_surface3d_ci(bad, "x", "y", "p", "lo", "hi"), "lower.*upper")
  expect_error(fig_surface3d_ci(g[-1, ], "x", "y", "p", "lo", "hi"), "complete")
})

test_that("supported-value helpers list what the arguments accept", {
  expect_true(all(c("nature", "cell", "science", "slide") %in% vellum_venues_supported()))
  expect_true("okabe_ito" %in% vellum_palettes_supported("qualitative"))
  expect_true("strata" %in% vellum_palettes_supported("diverging"))
  expect_false("strata" %in% vellum_palettes_supported("qualitative"))
  expect_true(all(c("pdf", "png", "svg") %in% vellum_formats_supported()))
  expect_true(all(vellum_themes_supported() %in% getNamespaceExports("vellum")))
})

test_that("the cheatsheet prints every section without error", {
  for (s in c("workflow", "themes", "colour", "scales", "geoms", "recipes",
              "geometry", "typography", "values")) {
    expect_no_error(suppressMessages(vellum_cheatsheet(s)))
  }
  expect_no_error(suppressMessages(vellum_cheatsheet()))
  expect_error(vellum_cheatsheet("nonsense"), "should be one of")
})

test_that("the snippet file is valid JSON with the documented prefixes", {
  json <- vellum_snippets(dir = NULL)
  # Balanced braces and brackets, and no stray unescaped quote inside a body.
  count <- function(p) lengths(regmatches(json, gregexpr(p, json)))
  expect_equal(count("[{]"), count("[}]"))
  expect_equal(count("\\["), count("\\]"))
  for (p in c("vfig", "vtheme", "vaudit", "vforest", "vdotstrip", "vemph", "vbank")) {
    expect_match(json, sprintf('"prefix": "%s"', p), fixed = TRUE)
  }

  dir <- withr::local_tempdir()
  path <- suppressMessages(vellum_snippets(dir = dir))
  expect_true(file.exists(path))
  expect_error(suppressMessages(vellum_snippets(dir = dir)), "already exists")
  expect_no_error(suppressMessages(vellum_snippets(dir = dir, overwrite = TRUE)))

  # A quote inside a snippet body must reach the file escaped, or the editor rejects it.
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, '\\\\"nature', perl = TRUE)
})
