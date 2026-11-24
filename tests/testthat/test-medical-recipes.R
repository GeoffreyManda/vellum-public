consort_stages <- function(n = c(430, 318, 302)) {
  data.frame(id = c("assessed", "randomised", "analysed"),
             label = c("Assessed for eligibility", "Randomised", "Analysed"),
             n = n, stringsAsFactors = FALSE)
}

consort_exclusions <- function() {
  data.frame(after = c("assessed", "assessed", "randomised"),
             label = c("Did not meet criteria", "Declined", "Lost to follow-up"),
             n = c(84, 28, 16), stringsAsFactors = FALSE)
}

test_that("a participant flow that reconciles is drawn", {
  p <- fig_consort(consort_stages(), consort_exclusions())
  expect_s3_class(p, "ggplot")
  recon <- attr(p, "vellum_reconciliation")
  expect_true(all(recon$reconciles))
  expect_equal(recon$difference, c(0, 0))
})

test_that("a participant flow that does not reconcile is refused", {
  # 430 assessed minus 112 excluded is 318, not 320.
  expect_error(fig_consort(consort_stages(c(430, 320, 302)), consort_exclusions()),
               "do not reconcile")
  expect_error(fig_consort(consort_stages(c(430, 320, 302)), consort_exclusions()),
               "unaccounted for")
})

test_that("the reconciliation check can be waived deliberately", {
  expect_s3_class(
    fig_consort(consort_stages(c(430, 320, 302)), consort_exclusions(), check = FALSE),
    "ggplot")
})

test_that("each step reconciles on its own, not merely the total", {
  # These two errors cancel in the end-to-end total but are both wrong.
  stages <- data.frame(id = c("a", "b", "c"), label = c("A", "B", "C"),
                       n = c(100, 60, 50), stringsAsFactors = FALSE)
  excl <- data.frame(after = c("a", "b"), label = c("x", "y"), n = c(30, 20),
                     stringsAsFactors = FALSE)
  recon <- consort_reconcile(stages, excl)
  expect_equal(recon$difference, c(10, -10))
  expect_false(all(recon$reconciles))
})

test_that("an exclusion attached to a stage that does not exist is an error", {
  bad <- consort_exclusions()
  bad$after[1] <- "nowhere"
  expect_error(fig_consort(consort_stages(), bad), "do not exist")
})

test_that("exclusion boxes clear the main flow", {
  p <- fig_consort(consort_stages(), consort_exclusions())
  nodes <- attr(p, "vellum_diagram")$nodes
  spine <- nodes[!grepl("^excl_", nodes$id), ]
  side <- nodes[grepl("^excl_", nodes$id), ]
  expect_true(all(side$x - side$width / 2 > max(spine$x + spine$width / 2)))
})

test_that("a causal diagram refuses a cycle", {
  variables <- data.frame(id = c("e", "m", "o"), label = c("E", "M", "O"),
                          role = c("exposure", "mediator", "outcome"),
                          stringsAsFactors = FALSE)
  ok <- data.frame(from = c("e", "m"), to = c("m", "o"), stringsAsFactors = FALSE)
  expect_s3_class(fig_dag(variables, ok), "ggplot")
  cyclic <- rbind(ok, data.frame(from = "o", to = "e"))
  expect_error(fig_dag(variables, cyclic), "cycle")
})

test_that("adjusted variables are boxed and unadjusted ones are not", {
  variables <- data.frame(id = c("e", "c", "o"), label = c("E", "C", "O"),
                          role = c("exposure", "confounder", "outcome"),
                          stringsAsFactors = FALSE)
  arcs <- data.frame(from = c("e", "c", "c"), to = c("o", "e", "o"),
                     stringsAsFactors = FALSE)
  p <- fig_dag(variables, arcs, adjusted = "c")
  nodes <- attr(p, "vellum_diagram")$nodes
  expect_equal(nodes$shape[nodes$id == "c"], "rect")
  expect_equal(nodes$shape[nodes$id == "e"], "ellipse")
})

test_that("adjusting for a variable that does not exist is an error", {
  variables <- data.frame(id = "e", label = "E", role = "exposure",
                          stringsAsFactors = FALSE)
  expect_error(fig_dag(variables, data.frame(from = character(), to = character()),
                       adjusted = "ghost"),
               "do not exist")
})

test_that("a compartmental model must name every transition rate", {
  states <- c(S = "Susceptible", I = "Infectious")
  expect_error(fig_compartmental(states, data.frame(from = "S", to = "I")),
               "must name its rate")
  expect_s3_class(
    fig_compartmental(states, data.frame(from = "S", to = "I", rate = "beta")),
    "ggplot")
})

test_that("a compartmental model draws one node per state", {
  p <- fig_compartmental(
    c(S = "Susceptible", E = "Exposed", I = "Infectious", R = "Recovered"),
    data.frame(from = c("S", "E", "I"), to = c("E", "I", "R"),
               rate = c("beta", "sigma", "gamma"), stringsAsFactors = FALSE))
  expect_equal(nrow(attr(p, "vellum_diagram")$nodes), 4L)
})

test_that("a pathway requires an explicit relation type", {
  entities <- data.frame(id = c("a", "b"), label = c("A", "B"), stringsAsFactors = FALSE)
  expect_error(fig_pathway(entities, data.frame(from = "a", to = "b")), "type")
  expect_s3_class(
    fig_pathway(entities, data.frame(from = "a", to = "b", type = "activation")),
    "ggplot")
})

test_that("pathway entity kinds map to distinguishable shapes", {
  entities <- data.frame(
    id = c("l", "r", "g"), label = c("L", "R", "G"),
    kind = c("ligand", "receptor", "gene"), stringsAsFactors = FALSE)
  relations <- data.frame(from = c("l", "r"), to = c("r", "g"),
                          type = c("binding", "activation"), stringsAsFactors = FALSE)
  nodes <- attr(fig_pathway(entities, relations), "vellum_diagram")$nodes
  expect_equal(length(unique(nodes$shape)), 3L)
})

test_that("a pathway puts its compartments round the right entities", {
  entities <- data.frame(
    id = c("a", "b"), label = c("A", "B"),
    compartment = c(NA, "nucleus"), stringsAsFactors = FALSE)
  relations <- data.frame(from = "a", to = "b", type = "translocation",
                          stringsAsFactors = FALSE)
  p <- fig_pathway(entities, relations, compartments = c(nucleus = "Nucleus"))
  comps <- attr(p, "vellum_diagram")$compartments
  expect_equal(nrow(comps), 1L)
  expect_equal(comps$contains[[1]], "b")
})

test_that("a pipeline groups its steps into phases", {
  steps <- data.frame(
    id = c("a", "b", "c"), label = c("A", "B", "C"),
    phase = c("Wet lab", "Wet lab", "Analysis"),
    detail = c("p1", NA, "p3"), stringsAsFactors = FALSE)
  p <- fig_pipeline(steps)
  expect_equal(nrow(attr(p, "vellum_diagram")$compartments), 2L)
  nodes <- attr(p, "vellum_diagram")$nodes
  expect_match(nodes$label[nodes$id == "a"], "p1")
  expect_false(grepl("NA", nodes$label[nodes$id == "b"]))
})

test_that("a timeline places events on a real numeric axis", {
  events <- data.frame(time = c(-2, 0, 24), label = c("Screen", "Randomise", "Endpoint"),
                       stringsAsFactors = FALSE)
  p <- fig_timeline(events, time_label = "Weeks")
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  rng <- built$layout$panel_params[[1]]$x.range
  # The gap from week 0 to week 24 must not be drawn the same as the gap from
  # week -2 to week 0.
  expect_gte(diff(rng), 26)
})

test_that("recipes reject malformed input rather than drawing something wrong", {
  expect_error(fig_consort(data.frame(id = "a", label = "A", n = 1)), "at least two")
  expect_error(fig_consort(data.frame(x = 1)), "data frame with")
  expect_error(fig_dag(data.frame(id = "a"), data.frame(from = "a", to = "a")), "label")
  expect_error(fig_timeline(data.frame(label = "x")), "time")
})
