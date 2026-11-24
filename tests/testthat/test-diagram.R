test_that("a diagram is a ggplot carrying its own natural size in millimetres", {
  d <- diagram(list(dg_node("a", "Start"), dg_node("b", "End")),
               list(dg_edge("a", "b")))
  expect_s3_class(d, "ggplot")
  sz <- diagram_size(d)
  expect_named(sz, c("width", "height"))
  expect_gt(sz[["height"]], 0)
  # Two 9 mm nodes 16 mm apart cannot occupy less than that.
  expect_gte(sz[["height"]], 16)
})

test_that("diagram_size rescales while preserving proportions", {
  d <- diagram(list(dg_node("a", "A"), dg_node("b", "B")), list(dg_edge("a", "b")))
  nat <- diagram_size(d)
  at89 <- diagram_size(d, width = 89)
  expect_equal(at89[["width"]], 89)
  # Both are reported rounded to 0.01 mm, so the ratios agree to that and no
  # further.
  expect_equal(at89[["height"]] / at89[["width"]],
               nat[["height"]] / nat[["width"]], tolerance = 1e-3)
})

test_that("layered ranking puts a node downstream of everything feeding it", {
  nodes <- data.frame(id = c("a", "b", "c"), label = c("A", "B", "C"),
                      stringsAsFactors = FALSE)
  edges <- data.frame(from = c("a", "b"), to = c("b", "c"), stringsAsFactors = FALSE)
  r <- dg_ranks(nodes, edges)
  expect_equal(unname(r[c("a", "b", "c")]), c(0L, 1L, 2L))
})

test_that("a feedback loop is drawn without the layout running away", {
  # Ranking by longest path has no fixed point on a cycle: every pass pushes the
  # nodes one rank further apart, and the canvas grows without bound.
  nodes <- lapply(c("a", "b", "c"), function(i) dg_node(i, toupper(i)))
  edges <- list(dg_edge("a", "b"), dg_edge("b", "c"), dg_edge("c", "a", "inhibition"))
  d <- diagram(nodes, edges)
  expect_lt(diagram_size(d)[["height"]], 200)
})

test_that("an edge hidden behind an unrelated node is routed around it", {
  # Collinear nodes make a skip edge invisible, and an invisible edge is a
  # missing claim rather than an untidy one.
  nodes <- lapply(c("a", "b", "c"), function(i) dg_node(i, toupper(i)))
  edges <- list(dg_edge("a", "b"), dg_edge("b", "c"), dg_edge("a", "c"))
  d <- diagram(nodes, edges, direction = "right")
  drawn <- attr(d, "vellum_diagram")$edges
  skip <- drawn[drawn$from == "a" & drawn$to == "c", ]
  expect_gt(abs(skip$curvature), 0)
  direct <- drawn[drawn$from == "a" & drawn$to == "b", ]
  expect_equal(direct$curvature, 0)
})

test_that("edge types are validated and unknown ones refused", {
  expect_error(dg_edge("a", "b", "vibes"), "must be one of")
  expect_silent(dg_edge("a", "b", "inhibition"))
})

test_that("an edge to a node that does not exist is an error", {
  expect_error(
    diagram(list(dg_node("a", "A")), list(dg_edge("a", "ghost"))),
    "do not exist")
})

test_that("duplicate node identifiers are refused", {
  expect_error(diagram(list(dg_node("a", "A"), dg_node("a", "B"))), "Duplicated")
})

test_that("acyclic = TRUE names the cycle it found", {
  nodes <- lapply(c("a", "b"), function(i) dg_node(i, toupper(i)))
  edges <- list(dg_edge("a", "b"), dg_edge("b", "a"))
  expect_error(diagram(nodes, edges, acyclic = TRUE), "cycle")
  expect_s3_class(diagram(nodes, edges, acyclic = FALSE), "ggplot")
})

test_that("labels wrap to the measured node width and keep authored line breaks", {
  # strwrap treats a newline as ordinary whitespace, so wrapping a whole label at
  # once runs deliberately separated lines together.
  out <- dg_wrap("First line\nSecond line", width_mm = 60, size_pt = 7)
  expect_equal(length(strsplit(out, "\n")[[1]]), 2L)
  narrow <- dg_wrap("a fairly long label that must wrap somewhere", width_mm = 12, size_pt = 7)
  expect_gt(length(strsplit(narrow, "\n")[[1]]), 1L)
})

test_that("tints are opaque, so a fill has a measurable colour", {
  t <- dg_tint("#0072B2", 0.2)
  expect_match(t, "^#[0-9A-Fa-f]{6}$")
  expect_gt(lightness(t), lightness("#0072B2"))
})

test_that("node text ink is chosen by measured contrast", {
  expect_equal(dg_readable_ink("#FFFFFF", "#000000"), "#000000")
  expect_equal(dg_readable_ink("#101010", "#000000"), "#FFFFFF")
})

test_that("an edge meets a node's outline rather than its centre", {
  p <- dg_boundary("rect", cx = 0, cy = 0, w = 10, h = 4, tx = 100, ty = 0)
  expect_equal(unname(p[1]), 5, tolerance = 1e-6)
  e <- dg_boundary("circle", cx = 0, cy = 0, w = 10, h = 10, tx = 100, ty = 0)
  expect_equal(unname(e[1]), 5, tolerance = 1e-6)
})

test_that("a manual layout demands positions for every node", {
  expect_error(
    diagram(list(dg_node("a", "A"), dg_node("b", "B", x = 1, y = 1)), layout = "manual"),
    "no position")
})

test_that("an explicitly positioned node keeps its position under an automatic layout", {
  d <- diagram(list(dg_node("a", "A"), dg_node("b", "B", x = 40, y = -40)),
               list(dg_edge("a", "b")))
  nodes <- attr(d, "vellum_diagram")$nodes
  expect_equal(nodes$x[nodes$id == "b"], 40)
  expect_equal(nodes$y[nodes$id == "b"], -40)
})

test_that("compartments enclose their contents", {
  d <- diagram(
    list(dg_node("a", "A"), dg_node("b", "B"), dg_node("c", "C")),
    list(dg_edge("a", "b"), dg_edge("b", "c")),
    list(dg_compartment("box", "Inside", contains = c("b", "c"))))
  nodes <- attr(d, "vellum_diagram")$nodes
  boxes <- dg_compartment_boxes(attr(d, "vellum_diagram")$compartments, nodes)
  inside <- nodes[nodes$id %in% c("b", "c"), ]
  expect_true(all(inside$x - inside$width / 2 >= boxes$xmin))
  expect_true(all(inside$y + inside$height / 2 <= boxes$ymax))
})
