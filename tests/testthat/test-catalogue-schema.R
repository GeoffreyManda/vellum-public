test_that("every catalogue row is classified by structure and encoding", {
  x <- chart_catalogue()
  expect_true(all(c("structure", "encoding") %in% names(x)))
  expect_false(anyNA(x$structure))
  expect_false(anyNA(x$encoding))
  expect_equal(nrow(x), length(vellum:::.chart_schema))
  expect_setequal(names(vellum:::.chart_schema), x$chart)
})

test_that("the vocabularies are closed and fully used", {
  structures <- chart_vocabulary("structure")
  encodings <- chart_vocabulary("encoding")
  x <- chart_catalogue()

  expect_true(all(x$structure %in% structures$term))
  expect_true(all(x$encoding %in% encodings$term))
  # A term nobody uses is an invitation to drift, so hold the vocabulary to its
  # own members rather than letting it accumulate aspirations.
  expect_setequal(unique(x$structure), structures$term)
  expect_setequal(unique(x$encoding), encodings$term)

  expect_equal(encodings$accuracy_rank, seq_len(nrow(encodings)))
  expect_equal(encodings$term[1:2], c("count", "position_aligned"))
  expect_equal(utils::tail(encodings$term, 1), "density")
})

test_that("the schema filters and agrees with what the recipes draw", {
  expect_setequal(chart_catalogue(structure = "hierarchy")$chart,
                  c("treemap", "icicle", "sunburst", "dendrogram"))
  # Membership rather than an exhaustive list: the catalogue is expected to
  # grow, and a test that has to be edited every time one is added stops being
  # read and starts being rubber-stamped.
  expect_true(all(c("flow diagram", "CONSORT flow", "causal DAG", "PRISMA flow") %in%
                    chart_catalogue(structure = "process")$chart))
  expect_false("scatter plot" %in% chart_catalogue(structure = "process")$chart)
  expect_true("donut" %in% chart_catalogue(encoding = "angle")$chart)
  # A waffle is counted, not estimated. Classifying it as area would put it
  # BELOW the donut on Cleveland and McGill's ordering, which is the opposite of
  # what fig_donut() tells the user, and the opposite of the truth.
  expect_equal(chart_catalogue(encoding = "count")$chart, "waffle")

  # The claim fig_donut() makes in its own note: the alternative it recommends
  # sits higher on the accuracy ordering than the form it replaces.
  rank_of <- function(chart) {
    e <- chart_catalogue()$encoding[chart_catalogue()$chart == chart]
    v <- chart_vocabulary("encoding")
    v$accuracy_rank[v$term == e]
  }
  expect_lt(rank_of("waffle"), rank_of("donut"))
  expect_lt(rank_of("dot strip"), rank_of("donut"))

  expect_error(chart_catalogue(structure = "banana"), "should be one of")
  expect_error(chart_catalogue(encoding = "vibes"), "should be one of")
})

test_that("filters compose across all three axes", {
  x <- chart_catalogue(family = "Hierarchy", structure = "hierarchy",
                       status = "recipe")
  expect_equal(nrow(x), 4L)
  # Alluvial, Sankey and chord: all three read a magnitude off ribbon width.
  expect_setequal(chart_catalogue(structure = "network", encoding = "length")$chart,
                  c("alluvial", "Sankey diagram", "chord diagram"))
})
