library(ggplot2)

test_that("tokens clamp text into the venue's permitted range", {
  # This clamp is what makes venue_check() pass by construction rather than by luck.
  tk <- vellum_tokens("nature")
  sizes <- unlist(tk$type$size[c("axis_title", "axis_text", "strip",
                                 "legend_title", "legend_text", "annot", "small")])
  expect_true(all(sizes >= 5))
  expect_true(all(sizes <= 7))

  # Even an absurd request is clamped.
  tk2 <- vellum_tokens("nature", base_size = 40)
  sizes2 <- unlist(tk2$type$size[c("axis_title", "axis_text")])
  expect_true(all(sizes2 <= 7))
})

test_that("tokens scale physical marks with type size", {
  small <- vellum_tokens("nature")
  big <- vellum_tokens("poster")
  expect_gt(big$geom$point, small$geom$point)
  expect_gt(big$rule$pt$axis, small$rule$pt$axis)
  expect_gt(as_mm(big$space$tick_len), as_mm(small$space$tick_len))
  # The hairline never goes below the print floor, however small the type.
  expect_gte(vellum_tokens("nature", base_size = 5)$rule$pt$hairline, 0.25)
})

test_that("the theme carries the palette, so no scale call is needed", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() + theme_vellum()
  cols <- unique(ggplot_build(p)$data[[1]]$colour)
  expect_true(all(cols %in% okabe_ito(8)))

  p2 <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() +
    theme_vellum(palette = "bright")
  expect_true(all(unique(ggplot_build(p2)$data[[1]]$colour) %in% vellum_pal("bright")))
})

test_that("an ordered palette is not used for unordered groups", {
  # A sequential ramp on a discrete aesthetic would imply a ranking that is not
  # there, so the theme falls back to Okabe-Ito.
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() +
    theme_vellum(palette = "abyss")
  expect_true(all(unique(ggplot_build(p)$data[[1]]$colour) %in% okabe_ito(8)))
})

test_that("a screen venue is not a dark venue", {
  # Tying the background to the venue produced dark ink on dark paper for
  # theme_vellum_slide(): text at 1.03:1 contrast, i.e. invisible.
  for (v in c("nature", "slide", "poster")) {
    tk <- suppressWarnings(vellum_tokens(v))
    expect_equal(tk$ink$paper, "#FFFFFF", info = v)
    expect_gt(contrast_ratio(tk$ink$text, tk$ink$paper), 15)
  }
  tkd <- suppressWarnings(vellum_tokens("slide", dark = TRUE))
  expect_equal(tkd$ink$paper, "#101012")
  expect_gt(contrast_ratio(tkd$ink$text, tkd$ink$paper), 15)
})

test_that("every theme variant builds", {
  base <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  variants <- list(
    theme_vellum(), theme_vellum_grid(), theme_vellum_frame(), theme_vellum_prism(),
    theme_vellum_void(), theme_vellum_dark(), theme_vellum_slide(), theme_vellum_poster()
  )
  for (i in seq_along(variants)) {
    expect_no_error(ggplot_gtable(ggplot_build(base + variants[[i]])))
  }
  suppressWarnings(expect_no_error(ggplot_gtable(ggplot_build(base + theme_venue("cell")))))
})

test_that("theme_venue applies the venue's house axis idiom", {
  nature <- calc_element("panel.border", complete_theme(theme_venue("nature")))
  expect_s3_class(nature, "element_blank")
  cell <- suppressWarnings(calc_element("panel.border", complete_theme(theme_venue("cell"))))
  expect_false(inherits(cell, "element_blank"))
})

test_that("shapes and linetypes refuse to exceed their usable range", {
  expect_length(vellum_shapes(6), 6L)
  expect_error(vellum_shapes(20), "distinguishable shapes")
  expect_error(vellum_linetypes(20), "distinguishable linetypes")
})

test_that("figure_dim reports NA rather than zero for a free panel", {
  # A null unit converts to zero length, so reporting it as a measurement would be
  # a wrong number rather than a missing one.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  d <- figure_dim(p)
  expect_false(d$panel_fixed[["width"]])
  expect_true(is.na(d$panel[["width"]]))
  expect_true(is.na(d$total[["width"]]))
  expect_gt(d$decoration[["width"]], 0)   # decoration is intrinsic and real
})

test_that("fit_width hits the target size exactly", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  for (col in c("single", "double")) {
    f <- fit_width(p, "nature", col, quiet = TRUE)
    got <- figure_dim(f)$total
    expect_equal(got[["width"]], venue_width("nature", col), tolerance = 0.3)
    expect_true(all(figure_dim(f)$panel_fixed))
  }
})

test_that("fit_width copes with long labels and with facets", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    labs(y = "Adjusted mean fuel economy (miles per US gallon)") +
    geom_point() + theme_vellum()
  f <- fit_width(p, "nature", "single", quiet = TRUE)
  expect_equal(figure_dim(f)$total[["width"]], 89, tolerance = 0.3)

  pf <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~cyl) + theme_vellum()
  ff <- fit_width(pf, "nature", "double", height = 70, quiet = TRUE)
  expect_equal(figure_dim(ff)$total[["height"]], 70, tolerance = 0.3)
})

test_that("fit_width explains itself when the target is impossible", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum_poster()
  expect_error(fit_width(p, "nature", column = 12), "decoration alone")
  expect_error(fit_width(patchwork::wrap_plots(p, p), "nature"), "single .*ggplot")
})

test_that("set_panel_area fixes the panel absolutely", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  d <- figure_dim(set_panel_area(p, width = 50, height = 35))
  expect_equal(d$panel[["width"]], 50, tolerance = 0.01)
  expect_equal(d$panel[["height"]], 35, tolerance = 0.01)
})

test_that("compose_figure tags panels in the venue's own convention", {
  a <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  b <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot() + theme_vellum()
  cf <- compose_figure(a, b, venue = "nature", ncol = 2)
  expect_s3_class(cf, "patchwork")
  expect_no_error(patchwork::patchworkGrob(cf))
  expect_error(compose_figure(a, b, tags = c("i")), "entries for 2 panels")
})
