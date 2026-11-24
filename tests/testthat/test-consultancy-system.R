test_that("figure contracts travel with plots and generate prose", {
  con <- figure_contract(
    claim = "Risk rises with exposure.",
    claim_type = "descriptive",
    evidence = "Synthetic cohort",
    support = "n = 120",
    uncertainty = "95% confidence interval",
    provenance = "Generated in the test"
  )
  expect_s3_class(con, "vellum_contract")
  expect_match(contract_caption(con), "Risk rises")
  expect_match(contract_alt_text(con), "n = 120")

  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(x = "Weight", y = "Mileage") + theme_vellum()
  p <- attach_contract(p, con)
  expect_identical(plot_contract(p), con)
})

test_that("interval grammar is named and Wilson limits behave at boundaries", {
  spec <- interval_spec("confidence", 0.95, "pointwise", "Wilson score")
  expect_s3_class(spec, "vellum_interval")
  expect_match(interval_label(spec), "95% pointwise confidence")

  w <- wilson_interval(c(0, 5, 10), 10)
  expect_equal(w$estimate, c(0, 0.5, 1))
  expect_true(all(w$lower >= 0))
  expect_true(all(w$upper <= 1))
  expect_lt(w$upper[1], 0.5)
  expect_gt(w$lower[3], 0.5)
})

test_that("design profiles stay independent of venue", {
  p <- vellum_profiles()
  expect_true(all(c("restrained", "editorial", "clinical",
                    "technical", "presentation") %in% p$profile))
  th <- suppressWarnings(theme_vellum_profile("clinical", venue = "plos"))
  expect_s3_class(th, "theme")
  expect_identical(attr(th, "vellum_profile"), "clinical")
})

test_that("review finds missing contract, title and colour-only encoding", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() + labs(title = "A title", x = "Weight", y = "Mileage") +
    theme_vellum()
  r <- suppressWarnings(vellum_review(p, require_contract = TRUE))
  expect_s3_class(r, "vellum_review")
  expect_equal(r$severity[r$id == "contract"], "fail")
  expect_equal(r$severity[r$id == "journal-title"], "warn")
  expect_equal(r$severity[r$id == "redundant-encoding"], "warn")

  refined <- vellum_refine(p, "editorial")
  expect_null(refined$labels$title)
  expect_equal(attr(refined, "vellum_moved_title"), "A title")
})

test_that("review verifies promised uncertainty is visible", {
  con <- figure_contract(
    "Estimates differ.", evidence = "Prepared model estimates",
    uncertainty = "95% confidence interval", provenance = "Test fixture"
  )
  d <- data.frame(term = c("A", "B"), estimate = c(.2, .4),
                  lower = c(.1, .25), upper = c(.3, .55))
  p <- fig_interval(d, "estimate", "lower", "upper", "term") |>
    attach_contract(con)
  r <- suppressWarnings(vellum_review(p))
  expect_equal(r$severity[r$id == "uncertainty"], "pass")
})

test_that("composition records layout choices and contracts", {
  skip_if_not_installed("patchwork")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
  con <- figure_contract("Two views agree.", evidence = "mtcars",
                         provenance = "R datasets")
  x <- compose_figure(p, p, ncol = 2, axes = "collect_y",
                      axis_titles = "collect_y", contract = con)
  expect_s3_class(x, "patchwork")
  expect_equal(attr(x, "vellum_composition")$panels, 2)
  expect_identical(plot_contract(x), con)
})

test_that("consultancy recipes return buildable ggplots", {
  set.seed(42)
  surv <- expand.grid(time = 0:5, group = c("A", "B"))
  surv$estimate <- exp(-surv$time * ifelse(surv$group == "A", .16, .24))
  surv$lower <- pmax(0, surv$estimate - .05)
  surv$upper <- pmin(1, surv$estimate + .05)

  roc <- data.frame(
    fpr = rep(seq(0, 1, length.out = 20), 2),
    model = rep(c("A", "B"), each = 20)
  )
  roc$tpr <- pmin(1, sqrt(roc$fpr) + ifelse(roc$model == "B", .08, 0))

  cal <- data.frame(pred = seq(.05, .95, length.out = 10))
  cal$obs <- pmin(1, pmax(0, cal$pred^0.9))
  cal$lower <- pmax(0, cal$obs - .06)
  cal$upper <- pmin(1, cal$obs + .06)

  ba <- data.frame(avg = rnorm(30, 10, 2), diff = rnorm(30, .2, .7))
  fun <- data.frame(est = rnorm(20, .2, .3), se = runif(20, .05, .25))
  volc <- data.frame(effect = rnorm(60), p = runif(60), gene = paste0("G", 1:60))
  traj <- expand.grid(time = 1:4, id = factor(1:8))
  traj$value <- rnorm(nrow(traj), traj$time)
  slope <- data.frame(label = LETTERS[1:6], start = 1:6, end = c(2, 4, 3, 7, 4, 8))
  rank <- data.frame(name = LETTERS[1:8], value = c(3, 7, 2, 8, 5, 4, 6, 1))
  tor <- data.frame(parameter = LETTERS[1:5], low = 1:5, high = 8:4)
  scores <- data.frame(PC1 = rnorm(20), PC2 = rnorm(20), group = rep(c("A", "B"), 10))
  loads <- data.frame(PC1 = c(.4, -.3), PC2 = c(.2, .5),
                      variable = c("Age", "Marker"))
  risk <- data.frame(likelihood = c(1, 3, 5), impact = c(5, 3, 4),
                     label = c("A", "B", "C"))

  plots <- list(
    fig_survival(surv, "time", "estimate", "group", "lower", "upper"),
    fig_roc(roc, "fpr", "tpr", "model"),
    fig_calibration(cal, "pred", "obs", "lower", "upper"),
    fig_bland_altman(ba, "avg", "diff"),
    fig_funnel(fun, "est", "se"),
    fig_volcano(volc, "effect", "p", "gene", max_labels = 3),
    fig_trajectory(traj, "time", "value", "id"),
    fig_slope(slope, "start", "end", "label", highlight = "D"),
    fig_lollipop(rank, "name", "value", highlight = "D"),
    fig_tornado(tor, "parameter", "low", "high", baseline = 4.5),
    fig_correlation(mtcars, c("mpg", "wt", "hp", "qsec")),
    fig_pca_biplot(scores, "PC1", "PC2", "group", loadings = loads,
                   loading_label = "variable"),
    fig_risk_matrix(risk, "likelihood", "impact", "label")
  )
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
  expect_silent(lapply(plots, ggplot_build))
})

test_that("chart catalogue is broad, searchable and honest about coverage", {
  x <- chart_catalogue()
  expect_gte(nrow(x), 65)
  expect_gte(length(unique(x$family)), 10)
  expect_true(all(x$status %in% c("recipe", "example", "mapped")))
  expect_true(any(x$vellum_recipe == "fig_surface3d_ci", na.rm = TRUE))

  r <- recommend_plot("compare diagnostic discrimination", "false positive true positive")
  expect_equal(r$chart[1], "ROC curve")
  expect_s3_class(fig_chart_taxonomy(), "ggplot")
})

test_that("visual QC produces one auditable row per plot", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    labs(x = "Weight", y = "Mileage") + theme_vellum()
  q <- suppressWarnings(visual_qc_manifest(list(scatter = p, second = p)))
  expect_equal(nrow(q), 2)
  expect_true(all(c("score", "pass", "warn", "fail", "contract") %in% names(q)))
})

test_that("physical label wrapping responds to measured width", {
  wrap <- label_wrap_mm(12, size = 7, family = "Arial")
  out <- wrap(c("Short", "A substantially longer scientific category"))
  expect_equal(out[1], "Short")
  expect_match(out[2], "\n")
})
