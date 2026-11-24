test_that("a minimal document parses to the geometry it describes", {
  svg <- svg_read('<svg viewBox="0 0 10 20"><rect width="4" height="8" fill="#AA3377"/></svg>')
  expect_s3_class(svg, "vellum_svg")
  expect_equal(unname(svg$viewbox[["width"]]), 10)
  expect_equal(unname(svg$viewbox[["height"]]), 20)
  expect_length(svg$shapes, 1L)
  expect_equal(svg$shapes[[1]]$fill, "#AA3377")
})

test_that("the viewBox is split on whitespace, not on the letter s", {
  # A POSIX character class reads "[,\\s]+" as comma-backslash-s, so a viewBox
  # with no commas silently failed to split and fell back to a default canvas.
  svg <- svg_read('<svg viewBox="0 0 400.69 232.4"><rect width="1" height="1"/></svg>')
  expect_equal(unname(svg$viewbox[["width"]]), 400.69)
  expect_equal(unname(svg$viewbox[["height"]]), 232.4)

  comma <- svg_read('<svg viewBox="0,0,50,25"><rect width="1" height="1"/></svg>')
  expect_equal(unname(comma$viewbox[["width"]]), 50)
})

test_that("path data tokenises the way SVG actually writes it", {
  expect_equal(svg_tokenise_path("M1.5.5L2-3"), c("M", "1.5", ".5", "L", "2", "-3"))
  expect_equal(svg_tokenise_path("M0 0L1e2 5"), c("M", "0", "0", "L", "1e2", "5"))
})

test_that("arc flags written without separators are read as flags", {
  # "a1 1 0 0110 10" packs large-arc, sweep and x into "0110": a plain number
  # scan reads 110 and the arc lands somewhere else entirely.
  p <- svg_parse_path("M0,0 a5 5 0 0110 0", tol = 0.1)
  expect_length(p$subpaths, 1L)
  last <- p$subpaths[[1]][nrow(p$subpaths[[1]]), ]
  expect_equal(unname(last[1]), 10, tolerance = 0.05)
  expect_equal(unname(last[2]), 0, tolerance = 0.05)
})

test_that("relative and absolute commands agree", {
  a <- svg_parse_path("M10,10 L20,10 L20,20 Z", tol = 0.1)
  r <- svg_parse_path("m10,10 l10,0 l0,10 z", tol = 0.1)
  expect_equal(a$subpaths[[1]], r$subpaths[[1]])
})

test_that("a repeated moveto continues as a lineto", {
  p <- svg_parse_path("M0,0 5,0 10,0", tol = 0.1)
  expect_equal(nrow(p$subpaths[[1]]), 3L)
})

test_that("the CSS cascade resolves in specification order", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><defs><style>.a{fill:#111111;}.b{fill:#222222;}</style></defs>',
    '<rect class="a" width="1" height="1" fill="#999999"/>',
    '<rect class="a b" width="1" height="1"/>',
    '<rect class="a" width="1" height="1" style="fill:#333333"/></svg>'))
  fills <- vapply(svg$shapes, `[[`, character(1), "fill")
  # A stylesheet rule beats a presentation attribute; a later class beats an
  # earlier one; an inline style beats both.
  expect_equal(fills, c("#111111", "#222222", "#333333"))
})

test_that("namespaced documents are matched on local name", {
  svg <- svg_read(paste0(
    '<ns0:svg xmlns:ns0="http://www.w3.org/2000/svg" viewBox="0 0 10 10">',
    '<ns0:circle cx="5" cy="5" r="4" fill="#123456"/></ns0:svg>'))
  expect_length(svg$shapes, 1L)
  expect_equal(svg$shapes[[1]]$fill, "#123456")
})

test_that("currentColor defers to the caller and the root's style is inherited", {
  src <- '<svg viewBox="0 0 10 10" fill="currentColor"><path d="M0,0 L9,9 L9,0 Z"/></svg>'
  expect_equal(svg_read(src, colour = "#FF0000")$shapes[[1]]$fill, "#FF0000")
  expect_equal(svg_read(src, colour = "#00FF00")$shapes[[1]]$fill, "#00FF00")
})

test_that("transforms compose and scale the stroke width with the geometry", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 100 100"><g transform="translate(10,20) scale(2)">',
    '<path d="M0,0 L5,0" stroke="#000000" stroke-width="1" fill="none"/></g></svg>'))
  pts <- svg$shapes[[1]]$subpaths[[1]]
  expect_equal(unname(pts[1, ]), c(10, 20))
  expect_equal(unname(pts[2, ]), c(20, 20))
  expect_equal(svg$shapes[[1]]$stroke_width_user, 2)
})

test_that("an element with neither fill nor stroke contributes nothing", {
  svg <- svg_read('<svg viewBox="0 0 10 10"><rect width="5" height="5" fill="none"/></svg>')
  expect_length(svg$shapes, 0L)
})

test_that("embedded rasters are recorded rather than silently dropped", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><image href="x.png" width="10" height="10"/>',
    '<rect width="2" height="2" fill="#000000"/></svg>'))
  expect_equal(svg$rasters, 1L)
  expect_match(svg$notes, "raster", all = FALSE)
})

test_that("the flattening tolerance follows the canvas rather than the unit", {
  small <- svg_read('<svg viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="#000"/></svg>')
  big <- svg_read('<svg viewBox="0 0 1000 1000"><circle cx="500" cy="500" r="400" fill="#000"/></svg>')
  # The same drawing at a thousand times the scale should not cost a thousand
  # times the vertices.
  expect_lt(svg_vertex_count(big), svg_vertex_count(small) * 3)
})

test_that("gradients and clipping paths are collected", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><defs>',
    '<linearGradient id="g"><stop offset="0" stop-color="#FFFFFF"/>',
    '<stop offset="1" stop-color="#000000"/></linearGradient>',
    '<clipPath id="c"><rect width="5" height="5"/></clipPath></defs>',
    '<rect width="10" height="10" fill="url(#g)" clip-path="url(#c)"/></svg>'))
  expect_length(svg$gradients, 1L)
  expect_equal(svg$gradients[["g"]]$colours, c("#FFFFFF", "#000000"))
  expect_length(svg$clips, 1L)
})

test_that("the drawing's own metadata is kept", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><metadata><title>Neutrophil</title>',
    '<license>Public Domain</license><creator>A Person</creator></metadata>',
    '<rect width="1" height="1" fill="#000"/></svg>'))
  expect_equal(svg$meta$title, "Neutrophil")
  expect_equal(svg$meta$license, "Public Domain")
  expect_equal(svg$meta$creator, "A Person")
})

test_that("svg_palette counts fills and strokes separately", {
  svg <- svg_read(paste0(
    '<svg viewBox="0 0 10 10"><rect width="1" height="1" fill="#AA0000"/>',
    '<rect width="1" height="1" fill="#AA0000"/>',
    '<path d="M0,0 L1,1" stroke="#0000AA" fill="none"/></svg>'))
  p <- svg_palette(svg)
  expect_equal(p$n[p$colour == "#AA0000" & p$role == "fill"], 2L)
  expect_equal(p$n[p$colour == "#0000AA" & p$role == "stroke"], 1L)
})

test_that("a malformed document fails with a usable message", {
  expect_error(svg_read("<notsvg/>"), "not")
  expect_error(svg_read("/no/such/file.svg"), "No SVG file")
})
