hierarchy_demo <- data.frame(
  domain = c("Health", "Health", "Health", "Climate", "Climate", "Operations"),
  service = c("Care", "Care", "Prevention", "Heat", "Flood", "Delivery"),
  item = c("Acute", "Chronic", "Screening", "Alerts", "Response", "Logistics"),
  value = c(24, 16, 20, 15, 13, 12)
)

test_that("hierarchical composition recipes preserve the full root total", {
  nodes <- vellum:::hierarchy_nodes(
    hierarchy_demo, c("domain", "service", "item"), "value"
  )
  expect_equal(nodes$value[nodes$id == "__root__"], sum(hierarchy_demo$value))
  expect_equal(sum(nodes$value[nodes$depth == 1]), sum(hierarchy_demo$value))
  expect_true(all(nodes$value >= 0))

  treemap_layout <- vellum:::layout_hierarchy_rect(nodes, "treemap")
  expect_equal(sum(
    with(treemap_layout[treemap_layout$depth == 1, ],
         (xmax - xmin) * (ymax - ymin))
  ), 1)
  expect_true(all(treemap_layout$xmin >= 0 & treemap_layout$xmax <= 1))

  partition <- vellum:::layout_hierarchy_rect(nodes, "partition")
  expect_equal(range(partition$xmin, partition$xmax), c(0, 1))
})

test_that("treemap, icicle and sunburst build as static ggplots", {
  invisible(suppressWarnings(vellum_tokens("nature")))
  plots <- list(
    suppressWarnings(fig_treemap(
      hierarchy_demo, c("domain", "service", "item"), "value"
    )),
    suppressWarnings(fig_icicle(
      hierarchy_demo, c("domain", "service", "item"), "value"
    )),
    suppressWarnings(fig_sunburst(
      hierarchy_demo, c("domain", "service", "item"), "value"
    ))
  )
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
  expect_silent(lapply(plots, ggplot2::ggplot_build))
})

test_that("dendrogram exposes merge structure and redundant clusters", {
  hc <- stats::hclust(stats::dist(USArrests[1:12, ]))
  p <- fig_dendrogram(hc, k = 3)
  expect_s3_class(p, "ggplot")
  expect_gte(length(p$layers), 3)
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("chord ribbons build from a named adjacency matrix", {
  m <- matrix(
    c(0, 9, 3, 2, 4, 0, 7, 1, 5, 2, 0, 6, 3, 4, 2, 0),
    4, byrow = TRUE,
    dimnames = list(c("A", "B", "C", "D"), c("A", "B", "C", "D"))
  )
  p <- fig_chord(m, palette = "flow_jama")
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 4)
  built <- ggplot2::ggplot_build(p)
  expect_gt(nrow(built$data[[1]]), sum(m > 0))
  expect_identical(p$coordinates$clip, "on")
})

test_that("chord labels stay separated and inside their solved bounds", {
  labs <- c(
    "Clinical services", "Community prevention", "Systems and data",
    "Research translation", "Patient partnership", "Quality improvement"
  )
  m <- matrix(3, length(labs), length(labs), dimnames = list(labs, labs))
  diag(m) <- 0
  p <- fig_chord(m, palette = "atni", label_width = 14, label_gap = 1)
  d <- attr(p, "vellum_chord_labels")
  bounds <- attr(p, "vellum_chord_bounds")
  expect_true(all(d$lower >= bounds$y[1] - 1e-10))
  expect_true(all(d$upper <= bounds$y[2] + 1e-10))
  expect_true(all(abs(d$x) < max(abs(bounds$x))))
  for (side in c(-1, 1)) {
    z <- d[d$side == side, , drop = FALSE]
    z <- z[order(z$y), , drop = FALSE]
    if (nrow(z) > 1L) {
      expect_true(all(z$lower[-1] > z$upper[-nrow(z)]))
    }
  }
})

test_that("chord labels fail visibly rather than clipping", {
  labs <- c("pneumonoultramicroscopicsilicovolcanoconiosis", "Care")
  m <- matrix(c(0, 2, 2, 0), 2, dimnames = list(labs, labs))
  expect_error(
    fig_chord(m, label_width = 6),
    "wider than"
  )
})

test_that("hierarchy recipes validate malformed inputs", {
  expect_error(
    fig_treemap(hierarchy_demo, c("missing"), "value"),
    "path"
  )
  bad <- hierarchy_demo
  bad$value[1] <- -1
  expect_error(fig_icicle(bad, c("domain", "service"), "value"),
               "non-negative")
  expect_error(fig_dendrogram(data.frame(x = 1)), "hclust")
  expect_error(fig_chord(matrix(1:6, 2, 3)), "square")
})

test_that("catalogue routes hierarchy and chord forms to recipes", {
  x <- chart_catalogue(c("Hierarchy", "Network"))
  expect_true(all(c(
    "fig_treemap", "fig_icicle", "fig_sunburst",
    "fig_dendrogram", "fig_chord"
  ) %in% x$vellum_recipe))
})
