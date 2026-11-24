builds <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
}

PRISMA <- list(
  identified = 2841, duplicates = 612, screened = 2229, screen_excluded = 2012,
  sought = 217, not_retrieved = 9, assessed = 208, included = 24,
  excluded = c("Wrong comparator" = 96, "Wrong outcome" = 61, "Not a trial" = 27)
)

test_that("fig_prisma reconciles the record counts", {
  builds(do.call(fig_prisma, PRISMA))

  broken <- PRISMA
  broken$screened <- 2000
  expect_error(do.call(fig_prisma, broken), "do not reconcile")
  expect_no_error(do.call(fig_prisma, c(broken, list(check = FALSE))))

  no_reasons <- PRISMA
  no_reasons$excluded <- 184
  expect_error(do.call(fig_prisma, no_reasons), "named numeric vector of reasons")
})

test_that("diagnostic-accuracy and meta-analysis diagnostics draw and refuse", {
  set.seed(8)
  d <- data.frame(sens = stats::runif(14, 0.62, 0.95),
                  spec = stats::runif(14, 0.55, 0.93),
                  n = round(stats::runif(14, 60, 900)))
  builds(fig_sroc(d, "sens", "spec", n = "n", summary_point = c(0.82, 0.78)))
  expect_error(fig_sroc(transform(d, sens = sens * 100), "sens", "spec"),
               "proportions in \\[0, 1\\]")

  b <- data.frame(study = paste("Trial", 1:18),
                  q = c(stats::runif(15, 0, 4), 9.1, 12.4, 7.2),
                  infl = c(stats::runif(15, 0, 0.25), 0.62, 0.41, 0.7))
  builds(fig_baujat(b, "q", "infl", label = "study"))
  expect_error(fig_baujat(transform(b, q = -q), "q", "infl"), "cannot be negative")

  l <- data.frame(control = stats::runif(16, 0.08, 0.62))
  l$treatment <- pmin(0.98, l$control * stats::runif(16, 0.45, 0.95))
  builds(fig_labbe(l, rr = 0.7))
  expect_error(fig_labbe(transform(l, control = control * 3)), "proportions in \\[0, 1\\]")
})

test_that("fig_cumulative_ma catches a reversed interval", {
  d <- data.frame(study = paste0("+ Trial ", 1:8),
                  est = c(0.55, 0.62, 0.68, 0.71, 0.72, 0.73, 0.74, 0.74),
                  lo = c(0.24, 0.36, 0.48, 0.56, 0.6, 0.64, 0.67, 0.69),
                  hi = c(1.26, 1.07, 0.96, 0.9, 0.86, 0.83, 0.81, 0.79))
  builds(fig_cumulative_ma(d, "est", "lo", "hi", "study", log_scale = TRUE))
  flipped <- d
  names(flipped)[names(flipped) == "lo"] <- "tmp"
  names(flipped)[names(flipped) == "hi"] <- "lo"
  names(flipped)[names(flipped) == "tmp"] <- "hi"
  expect_error(fig_cumulative_ma(flipped, "est", "lo", "hi", "study"),
               "not contain")
})

test_that("network geometry reports whether consistency can be assessed at all", {
  star <- data.frame(from = "Placebo", to = c("A", "B", "C"), trials = c(9, 6, 4))
  p <- fig_nma_network(star, weight = "trials")
  builds(p)
  expect_match(ggplot2::ggplot_build(p)$plot$labels$caption, "no closed loop")

  loop <- rbind(star, data.frame(from = "A", to = "B", trials = 3))
  expect_no_match(ggplot2::ggplot_build(fig_nma_network(loop, "trials"))$plot$labels$caption,
                  "no closed loop")
  expect_error(fig_nma_network(data.frame(from = "A", to = "B", weight = 1)),
               "at least three treatments")
})

test_that("fig_rankogram requires the ranks to partition", {
  d <- expand.grid(treatment = c("Drug A", "Drug B", "Drug C", "Placebo"), rank = 1:4)
  d$p <- c(0.55, 0.30, 0.13, 0.02, 0.28, 0.40, 0.26, 0.06,
           0.15, 0.24, 0.45, 0.16, 0.02, 0.06, 0.16, 0.76)
  builds(fig_rankogram(d, "treatment", "rank", "p"))
  builds(fig_rankogram(d, "treatment", "rank", "p", cumulative = TRUE))
  expect_error(fig_rankogram(transform(d, p = p * 0.6), "treatment", "rank", "p"),
               "do not sum to 1")
})

test_that("causal recipes hold the design to its own claims", {
  cov <- c("Age", "Sex", "Deprivation", "Comorbidity", "Smoking", "BMI")
  b <- data.frame(covariate = rep(cov, 2),
                  stage = rep(c("Unadjusted", "Weighted"), each = length(cov)),
                  smd = c(stats::runif(6, 0.08, 0.42), stats::runif(6, 0.005, 0.09)))
  builds(fig_balance(b, "covariate", "smd", stage = "stage"))
  expect_error(fig_balance(b[-3, ], "covariate", "smd", stage = "stage"),
               "same covariates")

  es <- data.frame(k = -4:5,
                   est = c(0.01, -0.02, 0.03, 0, 0.12, 0.21, 0.28, 0.31, 0.3, 0.33),
                   lo = c(-0.09, -0.12, -0.07, 0, 0.02, 0.11, 0.17, 0.19, 0.17, 0.18),
                   hi = c(0.11, 0.08, 0.13, 0, 0.22, 0.31, 0.39, 0.43, 0.43, 0.48))
  builds(fig_event_study(es, "k", "est", "lo", "hi", reference = -1))
  # The reference period is the whole interpretation, so it has no default.
  expect_error(fig_event_study(es, "k", "est", "lo", "hi"), "is required")
  expect_error(fig_event_study(es[es$k >= 0, ], "k", "est", "lo", "hi", reference = 0),
               "no pre-treatment periods")

  set.seed(12)
  r <- data.frame(score = stats::runif(400, 0, 100))
  r$outcome <- 20 + 0.35 * r$score + 12 * (r$score >= 60) + stats::rnorm(400, 0, 7)
  builds(fig_rdd(r, "score", "outcome", cutoff = 60))
  expect_error(fig_rdd(r, "score", "outcome", cutoff = 900), "one side of the cutoff")
})

test_that("a Lexis lifeline must advance in age and time together", {
  set.seed(13)
  d <- data.frame(entry_time = stats::runif(20, 2000, 2016))
  d$entry_age <- stats::runif(20, 20, 60)
  d$span <- stats::runif(20, 1, 9)
  d$exit_time <- d$entry_time + d$span
  d$exit_age <- d$entry_age + d$span
  builds(fig_lexis(d, period_range = c(2000, 2026), age_range = c(15, 75),
                   cohorts = seq(1940, 2000, by = 20)))
  bad <- d
  bad$exit_age[1] <- bad$exit_age[1] + 3
  expect_error(fig_lexis(bad, period_range = c(2000, 2026), age_range = c(15, 75)),
               "advance in age and time together")
})

test_that("fig_epicurve refuses negative case counts", {
  d <- expand.grid(day = 1:40, status = c("Confirmed", "Probable"))
  d$cases <- round(c(stats::dnorm(1:40, 22, 7) * 320, stats::dnorm(1:40, 19, 8) * 130))
  builds(fig_epicurve(d, "day", "cases", group = "status"))
  expect_error(fig_epicurve(transform(d, cases = -cases), "day", "cases"),
               "cannot be negative")
})

test_that("fig_bar draws from zero and fig_splom refuses to be unreadable", {
  p <- fig_bar(c(Nairobi = 412, Kampala = 366, Lusaka = 291))
  builds(p)
  # A bar encodes value as length, so the baseline is not negotiable.
  rng <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  expect_lte(rng[1], 0)

  builds(fig_splom(iris, columns = names(iris)[1:4], group = "Species"))
  wide <- as.data.frame(matrix(stats::rnorm(300), ncol = 10))
  expect_error(fig_splom(wide, columns = names(wide)), "too small to read")
})

test_that("fig_upset holds intersections to being exclusive", {
  x <- c("PubMed" = 412, "Embase" = 233, "CENTRAL" = 96, "PubMed&Embase" = 318,
         "PubMed&CENTRAL" = 54, "Embase&CENTRAL" = 41, "PubMed&Embase&CENTRAL" = 129)
  builds(fig_upset(x))
  builds(fig_upset(x, total = sum(x)))
  expect_error(fig_upset(x, total = 2000), "EXCLUSIVE intersection")
  expect_error(fig_upset(c(a = -1)), "cannot be negative")
})

test_that("fig_waterfall reconciles its contributions", {
  d <- data.frame(step = c("Baseline", "Coverage", "Price", "Adherence", "Net"),
                  value = c(0, 42, -18, 26, 50),
                  total = c(TRUE, FALSE, FALSE, FALSE, TRUE))
  builds(fig_waterfall(d, "step", "value", is_total = "total"))
  expect_error(fig_waterfall(transform(d, value = replace(value, 5, 99)),
                             "step", "value", is_total = "total"),
               "do not reconcile")
})

test_that("stream, bump, pyramid and swimmer draw and hold their invariants", {
  set.seed(15)
  st <- expand.grid(t = 1:20, series = LETTERS[1:5])
  st$v <- abs(stats::rnorm(100, 6, 2)) + rep(seq(1, 5), each = 20)
  builds(fig_stream(st, "t", "v", "series"))
  expect_error(fig_stream(st[-4, ], "t", "v", "series"), "missing at some time points")

  bp <- expand.grid(year = 2019:2024, team = c("A", "B", "C", "D"))
  bp$points <- stats::runif(24, 20, 90)
  builds(fig_bump(bp, x = "year", group = "team", value = "points"))
  expect_error(fig_bump(bp[-2, ], x = "year", group = "team", value = "points"),
               "missing or duplicated")

  ages <- c("0-4", "5-14", "15-24", "25-34")
  py <- data.frame(age = rep(ages, 2), sex = rep(c("Female", "Male"), each = 4),
                   n = c(890, 1520, 1320, 1080, 920, 1580, 1360, 1100))
  builds(fig_pyramid(py, "age", "n", "sex"))
  builds(fig_pyramid(py, "age", "n", "sex", proportional = TRUE))
  three <- rbind(py, data.frame(age = ages, sex = "Other", n = 10))
  expect_error(fig_pyramid(three, "age", "n", "sex"), "exactly two levels")

  set.seed(17)
  sw <- data.frame(id = sprintf("P%02d", 1:12), start = 0,
                   end = stats::runif(12, 2, 26))
  sw$ongoing <- sw$end > 18
  builds(fig_swimmer(sw, ongoing = "ongoing"))
  expect_error(fig_swimmer(rbind(sw, sw[1, ])), "must be unique")
})

test_that("fig_markov_trace requires a closed cohort", {
  cycles <- 0:30
  well <- 0.98^cycles
  dead <- 1 - 0.995^cycles
  sick <- pmax(0, 1 - well - dead)
  d <- data.frame(cycle = rep(cycles, 3),
                  state = rep(c("Well", "Sick", "Dead"), each = length(cycles)),
                  p = c(well, sick, dead))
  builds(fig_markov_trace(d, "cycle", "state", "p", absorbing = "Dead"))
  leak <- d
  leak$p[2] <- leak$p[2] * 0.5
  expect_error(fig_markov_trace(leak, "cycle", "state", "p"), "does not sum to 1")
  expect_no_error(fig_markov_trace(leak, "cycle", "state", "p", check = FALSE))
  expect_error(fig_markov_trace(d, "cycle", "state", "p", absorbing = "Cured"),
               "not present")
})

test_that("fig_frontier classifies dominance rather than trusting the ICERs", {
  d <- data.frame(
    strategy = c("Usual care", "Screening", "Screen + treat", "Intensive", "Add-on"),
    cost = c(1200, 3400, 6100, 9800, 5200),
    effect = c(4.1, 4.9, 5.6, 5.9, 4.8)
  )
  p <- fig_frontier(d, "cost", "effect", "strategy", wtp = 20000)
  builds(p)
  cls <- attr(p, "vellum_frontier")
  # Add-on costs more than Screening for less effect: simple dominance.
  expect_equal(as.character(cls$status[cls$strategy == "Add-on"]), "Dominated")
  expect_equal(sum(cls$status == "On frontier"), 4L)

  # Extended dominance: a strategy beaten by a mixture of two others, which is
  # the case a bare ICER table does not reveal.
  ed <- data.frame(strategy = c("A", "B", "C"), cost = c(0, 5000, 6000),
                   effect = c(1, 1.2, 2))
  expect_equal(as.character(attr(fig_frontier(ed, "cost", "effect", "strategy"),
                               "vellum_frontier")$status[2]), "Extendedly dominated")
})

test_that("fig_evppi cannot exceed the information it is part of", {
  d <- data.frame(parameter = c("Effect", "Risk", "Utility", "Cost"),
                  evppi = c(310, 184, 96, 41))
  builds(fig_evppi(d, "parameter", "evppi", evpi = 402, population = 30000))
  expect_error(fig_evppi(transform(d, evppi = replace(evppi, 1, 900)),
                         "parameter", "evppi", evpi = 402),
               "above the overall EVPI")
  expect_error(fig_evppi(transform(d, evppi = -evppi), "parameter", "evppi"),
               "negative")
})

test_that("every new form is registered in the taxonomy", {
  added <- c("PRISMA flow", "summary ROC", "Baujat plot", "L'Abbe plot",
             "cumulative meta-analysis", "network geometry", "rankogram",
             "covariate balance", "event study", "regression discontinuity",
             "Lexis diagram", "epidemic curve", "bar chart", "scatterplot matrix",
             "UpSet plot", "waterfall", "streamgraph", "bump chart",
             "population pyramid", "swimmer plot", "Markov trace",
             "efficiency frontier", "EVPPI")
  cat_rows <- chart_catalogue()
  expect_true(all(added %in% cat_rows$chart))
  expect_true(all(cat_rows$status[cat_rows$chart %in% added] == "recipe"))

  # The six that shipped without a row.
  orphans <- c("study timeline", "analysis pipeline", "compartmental model",
               "pathway diagram", "projected surface", "albatross plot")
  expect_true(all(orphans %in% cat_rows$chart))

  # And nothing exported is missing from the taxonomy any more.
  exported <- grep("^fig_", getNamespaceExports("vellum"), value = TRUE)
  expect_setequal(setdiff(exported, c(cat_rows$vellum_recipe, "fig_chart_taxonomy")),
                  character(0))
})
