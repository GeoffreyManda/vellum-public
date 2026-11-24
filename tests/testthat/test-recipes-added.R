builds <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

flows <- data.frame(
  from  = c("Screened", "Screened", "Enrolled", "Enrolled", "Enrolled"),
  to    = c("Enrolled", "Ineligible", "Completed", "Withdrew", "Lost"),
  value = c(180, 120, 140, 22, 18),
  stringsAsFactors = FALSE
)

test_that("fig_sankey draws conserved flows across any number of stages", {
  builds(fig_sankey(flows))
  three <- data.frame(
    from = c("Coal", "Gas", "Wind", "Electricity", "Electricity"),
    to = c("Electricity", "Electricity", "Electricity", "Industry", "Homes"),
    value = c(40, 35, 25, 45, 55), stringsAsFactors = FALSE
  )
  builds(fig_sankey(three))
  builds(fig_sankey(three, colour_by = "target"))
})

test_that("fig_sankey refuses a figure whose widths would be wrong", {
  broken <- flows
  broken$value[1] <- 200
  expect_error(fig_sankey(broken), "not conserved")
  expect_no_error(fig_sankey(broken, check = FALSE))

  expect_error(
    fig_sankey(data.frame(from = c("a", "b"), to = c("b", "a"), value = c(1, 1))),
    "cyclic"
  )
  expect_error(
    fig_sankey(data.frame(from = "a", to = "b", value = -1)),
    "zero or negative"
  )
  expect_error(
    fig_sankey(data.frame(from = "a", to = "a", value = 1)),
    "leave and enter the same node"
  )
})

test_that("fig_sankey conserves the drawn widths, not just the input", {
  # The ribbon leaving a node must be exactly as thick as the value it carries,
  # or the diagram is decorative. Measure the polygons rather than trusting them.
  p <- fig_sankey(flows)
  poly <- ggplot2::ggplot_build(p)$data[[1]]
  widths <- vapply(split(poly, poly$group), function(g) {
    at <- g$x[which.min(abs(g$x - min(g$x)))]
    diff(range(g$y[abs(g$x - at) < 1e-9]))
  }, numeric(1))
  expect_equal(sort(unname(round(widths, 6))), sort(flows$value))
})

marimekko_data <- local({
  d <- expand.grid(region = c("North", "Central", "South"),
                   outcome = c("Recovered", "Relapsed", "Died"))
  d$n <- c(410, 260, 120, 90, 70, 45, 30, 24, 18)
  d
})

test_that("fig_marimekko gives every column a width proportional to its total", {
  p <- fig_marimekko(marimekko_data, x = "region", fill = "outcome", value = "n")
  builds(p)
  rects <- ggplot2::ggplot_build(p)$data[[1]]
  widths <- vapply(split(rects, rects$xmin), function(g) g$xmax[1] - g$xmin[1], numeric(1))
  totals <- vapply(split(marimekko_data$n, marimekko_data$region), sum, numeric(1))
  # Both are in column order: `split()` on xmin sorts ascending, and the region
  # factor keeps its level order. A column's share of the drawn width must equal
  # its share of the count, which is the whole claim a Marimekko makes.
  expect_equal(unname(widths / sum(widths)), unname(totals / sum(totals)),
               tolerance = 1e-6)
  expect_error(
    fig_marimekko(transform(marimekko_data, n = -n), "region", "outcome", "n"),
    "non-negative"
  )
})

area_data <- local({
  d <- expand.grid(year = 2015:2024, source = c("Grant", "Contract", "Donation"))
  d$amount <- c(seq(40, 76, length.out = 10), seq(30, 12, length.out = 10),
                seq(8, 30, length.out = 10))
  d
})

test_that("fig_stacked_area refuses gaps and negatives rather than interpolating", {
  builds(fig_stacked_area(area_data, "year", "amount", "source"))
  builds(fig_stacked_area(area_data, "year", "amount", "source", proportional = TRUE))
  expect_error(fig_stacked_area(area_data[-3, ], "year", "amount", "source"),
               "missing at some time points")
  expect_error(
    fig_stacked_area(transform(area_data, amount = amount - 50), "year", "amount", "source"),
    "non-negative"
  )
})

test_that("fig_radar and fig_parallel handle axes and refuse what has none", {
  d <- data.frame(arm = c("Control", "Intervention"),
                  adherence = c(0.62, 0.81), retention = c(0.70, 0.88),
                  satisfaction = c(0.55, 0.79), cost = c(0.80, 0.44))
  builds(fig_radar(d, group = "arm"))
  expect_error(fig_radar(d[, 1:3], group = "arm"), "at least three axes")

  builds(fig_parallel(iris, columns = names(iris)[1:4], group = "Species"))
  builds(fig_parallel(iris, columns = names(iris)[1:4]))
  expect_error(fig_parallel(iris, columns = c("Sepal.Length", "Species")), "not numeric")
})

test_that("fig_ridgeline smooths every group identically and refuses thin ones", {
  set.seed(1)
  d <- data.frame(
    value = c(stats::rnorm(200, 5), stats::rnorm(200, 6, 1.4), stats::rnorm(200, 8)),
    dose = rep(c("0 mg", "10 mg", "20 mg"), each = 200)
  )
  builds(fig_ridgeline(d, x = "value", group = "dose"))
  thin <- rbind(d, data.frame(value = stats::rnorm(4), dose = "40 mg"))
  expect_error(fig_ridgeline(thin, x = "value", group = "dose"), "fewer than 10")
  expect_no_error(fig_ridgeline(thin, x = "value", group = "dose", min_n = 4))
})

test_that("fig_horizon folds both directions onto one baseline", {
  set.seed(2)
  d <- expand.grid(t = 1:60, series = paste("Site", 1:4))
  d$value <- as.numeric(stats::filter(stats::rnorm(nrow(d)), 0.85, method = "recursive"))
  p <- fig_horizon(d, x = "t", y = "value", group = "series")
  builds(p)
  drawn <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(drawn$y >= 0))
  expect_error(
    fig_horizon(data.frame(t = 1:5, value = 0), x = "t", y = "value"),
    "nothing to band"
  )
})

test_that("fig_manhattan orders chromosomes genomically and refuses impossible P values", {
  set.seed(3)
  d <- data.frame(
    chrom = rep(c(1, 2, 9, 10, "X"), each = 100),
    pos = rep(sort(sample.int(2.4e8, 100)), 5),
    p = stats::runif(500)^3
  )
  p <- fig_manhattan(d)
  builds(p)
  breaks <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_equal(breaks[!is.na(breaks)], c("1", "2", "9", "10", "X"))

  expect_error(fig_manhattan(transform(d, p = p * 0)), "outside \\(0, 1\\]")
  expect_error(fig_manhattan(transform(d, chrom = "banana")), "Unrecognised chromosome")
})

wtp_grid <- seq(0, 50000, by = 2000)

test_that("decision-analytic recipes draw prepared model output", {
  set.seed(4)
  draws <- data.frame(effect = stats::rnorm(400, 0.08, 0.05),
                      cost = stats::rnorm(400, 1400, 900))
  builds(fig_ce_plane(draws, wtp = 20000))
  builds(fig_ce_plane(rbind(cbind(draws, s = "A"), cbind(draws, s = "B")),
                      group = "s", wtp = 20000))

  pa <- stats::plogis((wtp_grid - 20000) / 6000)
  ceac <- data.frame(
    wtp = rep(wtp_grid, 2),
    strategy = rep(c("Usual care", "New programme"), each = length(wtp_grid)),
    prob = c(1 - pa, pa)
  )
  builds(fig_ceac(ceac, "wtp", "prob", "strategy"))

  evpi <- data.frame(wtp = wtp_grid, v = 400 * exp(-((wtp_grid - 20000) / 9000)^2))
  builds(fig_evpi(evpi, "wtp", "v", population = 30000))

  th <- seq(0.01, 0.5, by = 0.01)
  dc <- data.frame(threshold = th, nb = 0.14 - 0.28 * th)
  builds(fig_decision_curve(dc, "threshold", "nb", prevalence = 0.16))
})

test_that("decision-analytic recipes refuse incoherent model output", {
  pa <- stats::plogis((wtp_grid - 20000) / 6000)
  ceac <- data.frame(
    wtp = rep(wtp_grid, 2),
    strategy = rep(c("Usual care", "New programme"), each = length(wtp_grid)),
    prob = c(1 - pa, pa)
  )
  # Curves that do not partition are the commonest CEAC error: they come from
  # pairwise comparisons against one comparator rather than from a joint analysis.
  expect_error(fig_ceac(transform(ceac, prob = prob * 0.7), "wtp", "prob", "strategy"),
               "do not sum to 1")
  expect_no_error(fig_ceac(transform(ceac, prob = prob * 0.7), "wtp", "prob", "strategy",
                           check = FALSE))
  expect_error(fig_ceac(transform(ceac, prob = prob * 2), "wtp", "prob", "strategy"),
               "outside \\[0, 1\\]")

  evpi <- data.frame(wtp = wtp_grid, v = 400 * exp(-((wtp_grid - 20000) / 9000)^2))
  evpi$v[3] <- -1
  expect_error(fig_evpi(evpi, "wtp", "v"), "negative")

  expect_error(
    fig_decision_curve(data.frame(threshold = c(0, 0.2), nb = c(0.1, 0.05)),
                       "threshold", "nb"),
    "must lie in \\(0, 1\\)"
  )
})

test_that("fig_gosh and fig_rob_matrix show the structure of the evidence", {
  set.seed(5)
  n <- 200
  outlier <- stats::rbinom(n, 1, 0.4) == 1
  g <- data.frame(
    estimate = ifelse(outlier, stats::rnorm(n, 0.82, 0.05), stats::rnorm(n, 0.62, 0.04)),
    i2 = ifelse(outlier, stats::runif(n, 55, 88), stats::runif(n, 2, 30)),
    trial7 = ifelse(outlier, "Included", "Excluded")
  )
  builds(fig_gosh(g, colour = "trial7", reference = 0.7))
  builds(fig_gosh(g, log_scale = TRUE))
  expect_error(fig_gosh(transform(g, i2 = i2 - 100)), "negative")

  d <- expand.grid(study = paste("Trial", 1:5),
                   domain = c("Randomisation", "Deviations", "Reporting"))
  set.seed(6)
  d$judgement <- sample(c("low", "some", "high"), nrow(d), TRUE)
  builds(fig_rob_matrix(d))
  builds(fig_rob_matrix(d, summary = TRUE))
  expect_error(fig_rob_matrix(transform(d, judgement = "maybe")), "Unrecognised judgement")
  expect_error(fig_rob_matrix(rbind(d, d[1, ])), "judged more than once")
})

test_that("a risk-of-bias cell never relies on colour alone", {
  d <- expand.grid(study = paste("Trial", 1:3), domain = c("A", "B"))
  d$judgement <- c("low", "some", "high", "low", "high", "some")
  drawn <- ggplot2::ggplot_build(fig_rob_matrix(d))$data
  symbols <- drawn[[2]]$label
  expect_length(symbols, nrow(d))
  expect_true(all(nzchar(symbols)))
  expect_setequal(unique(symbols), unname(judgement_scale()$symbols[unique(d$judgement)]))
})

test_that("the catalogue names a function that exists for every implemented row", {
  cat_rows <- chart_catalogue()
  named <- setdiff(unique(cat_rows$vellum_recipe), NA)
  expect_true(all(vapply(named, exists, logical(1), where = asNamespace("vellum"))))

  # The rows this work was meant to close.
  added <- c("Sankey diagram", "Marimekko", "stacked area", "radar chart",
             "parallel coordinates", "ridgeline", "horizon graph",
             "Manhattan plot", "GOSH plot", "risk-of-bias matrix",
             "decision curve", "cost-effectiveness plane", "CEAC",
             "value-of-information curve")
  expect_true(all(!is.na(cat_rows$vellum_recipe[cat_rows$chart %in% added])))

  # And the rows that were stale: these recipes existed before this change.
  stale <- c("CONSORT flow", "causal DAG", "flow diagram", "node-link graph")
  expect_true(all(!is.na(cat_rows$vellum_recipe[cat_rows$chart %in% stale])))
})
