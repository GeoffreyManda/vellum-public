# Chart taxonomy and recommendation -----------------------------------------

catalogue_rows <- function() {
  rows <- list(
    c("Distribution", "dot strip", "show every observation", "continuous by group", "interval optional", "fig_dotstrip", "recipe"),
    c("Distribution", "raincloud", "compare shape, centre and observations", "continuous by group", "IQR", "fig_raincloud", "recipe"),
    c("Distribution", "box plot", "compare robust summaries", "continuous by group", "IQR", "fig_raincloud", "mapped"),
    c("Distribution", "violin plot", "compare distribution shape", "continuous by group", "distribution", "fig_raincloud", "mapped"),
    c("Distribution", "histogram", "show frequency distribution", "continuous", "sampling variability optional", NA, "example"),
    c("Distribution", "density plot", "show smooth distribution shape", "continuous", "bandwidth sensitivity", NA, "example"),
    c("Distribution", "ridgeline", "compare many distributions", "continuous by many groups", "distribution", "fig_ridgeline", "recipe"),
    c("Distribution", "ECDF", "compare full cumulative distributions", "continuous by group", "confidence band optional", NA, "mapped"),
    c("Distribution", "Q-Q plot", "diagnose distributional assumptions", "continuous", "reference envelope", NA, "mapped"),
    c("Comparison", "estimate interval", "compare estimates on an aligned scale", "estimate with limits", "named interval", "fig_interval", "recipe"),
    c("Comparison", "forest plot", "compare effect estimates", "estimate with limits", "confidence or credible interval", "fig_forest", "recipe"),
    c("Comparison", "paired slope", "show within-unit change", "paired continuous", "raw paired support", "fig_slope", "recipe"),
    c("Comparison", "lollipop ranking", "rank categories", "category and value", "interval optional", "fig_lollipop", "recipe"),
    c("Comparison", "beeswarm", "show observations without overlap", "continuous by group", "raw observations", "fig_dotstrip", "mapped"),
    c("Comparison", "Gardner-Altman", "show raw groups and effect size", "two groups", "bootstrap interval", "fig_dotstrip", "mapped"),
    c("Comparison", "tornado", "rank one-way sensitivity", "parameter with low/high", "scenario range", "fig_tornado", "recipe"),
    c("Relationship", "scatter plot", "show association between two variables", "two continuous", "fit band optional", NA, "example"),
    c("Relationship", "bubble plot", "add a third magnitude", "two continuous plus magnitude", "fit band optional", NA, "example"),
    c("Relationship", "hexbin", "show dense bivariate support", "large bivariate sample", "count per bin", NA, "example"),
    c("Relationship", "contour field", "show a continuous two-dimensional field", "regular x-y-z grid", "surface optional", "fig_heatmap", "mapped"),
    c("Relationship", "response surface", "show nonlinear joint effects", "regular x-y-z grid", "interval blankets", "fig_surface3d_ci", "recipe"),
    c("Relationship", "allometry", "show multiplicative scaling", "positive bivariate", "fit interval", NA, "example"),
    c("Time", "line chart", "show change through ordered time", "time series", "band optional", NA, "example"),
    c("Time", "trajectory plot", "show individual longitudinal paths", "repeated measures", "raw paths plus summary", "fig_trajectory", "recipe"),
    c("Time", "step-change plot", "show an intervention or discontinuity", "time series", "model interval", NA, "example"),
    c("Time", "seasonal polar", "show cyclic seasonality", "cyclic time series", "band optional", NA, "example"),
    c("Time", "horizon graph", "compare dense time series", "many time series", "not usually applicable", "fig_horizon", "recipe"),
    c("Survival", "Kaplan-Meier", "compare time-to-event survival", "prepared survival estimates", "confidence band", "fig_survival", "recipe"),
    c("Survival", "cumulative incidence", "show competing event incidence", "prepared incidence estimates", "confidence band", "fig_survival", "mapped"),
    c("Diagnostic", "ROC curve", "compare discrimination", "FPR and TPR", "AUC interval optional", "fig_roc", "recipe"),
    c("Diagnostic", "precision-recall curve", "evaluate rare-event classification", "precision and recall", "bootstrap interval optional", "fig_roc", "mapped"),
    c("Diagnostic", "calibration curve", "compare predicted and observed risk", "binned or smooth calibration", "confidence interval", "fig_calibration", "recipe"),
    c("Diagnostic", "Bland-Altman", "assess measurement agreement", "paired method values", "limits of agreement", "fig_bland_altman", "recipe"),
    c("Diagnostic", "decision curve", "compare net benefit by threshold", "threshold and net benefit", "bootstrap interval", "fig_decision_curve", "recipe"),
    c("Evidence synthesis", "funnel plot", "inspect small-study patterns", "estimate and standard error", "pseudo-confidence limits", "fig_funnel", "recipe"),
    c("Evidence synthesis", "risk-of-bias matrix", "show structured judgements", "study by domain matrix", "not applicable", "fig_rob_matrix", "recipe"),
    c("Evidence synthesis", "orchard plot", "show meta-analytic distribution", "effects with hierarchy", "prediction interval", "fig_forest", "mapped"),
    c("Evidence synthesis", "GOSH plot", "diagnose meta-analysis heterogeneity", "subset estimates", "sampling distribution", "fig_gosh", "recipe"),
    c("Multivariate", "correlation matrix", "show pairwise association structure", "numeric matrix", "not usually shown", "fig_correlation", "recipe"),
    c("Multivariate", "PCA biplot", "show sample scores and variable loadings", "prepared scores and loadings", "not usually shown", "fig_pca_biplot", "recipe"),
    c("Multivariate", "parallel coordinates", "compare multivariate profiles", "many continuous variables", "raw support", "fig_parallel", "recipe"),
    c("Multivariate", "radar chart", "compare a small normalized profile", "few commensurate dimensions", "not usually shown", "fig_radar", "recipe"),
    c("Multivariate", "clustered heatmap", "show matrix structure and clusters", "numeric matrix", "not usually shown", "fig_heatmap", "mapped"),
    c("Omics", "volcano plot", "screen effect against evidence", "effect and P value", "decision thresholds", "fig_volcano", "recipe"),
    c("Omics", "MA plot", "show effect against abundance", "mean abundance and log ratio", "uncertainty threshold", "fig_volcano", "mapped"),
    c("Omics", "Manhattan plot", "show association across genomic position", "chromosome position and P value", "significance threshold", "fig_manhattan", "recipe"),
    c("Composition", "stacked area", "show composition changing over time", "parts over time", "sampling interval optional", "fig_stacked_area", "recipe"),
    c("Composition", "waffle", "show countable part-to-whole composition", "category and count", "denominator visible", "fig_waffle", "recipe"),
    c("Composition", "donut", "show simple part-to-whole composition", "few categories", "denominator visible", "fig_donut", "recipe"),
    c("Composition", "alluvial", "show flows between categorical states", "weighted categorical transitions", "support counts", "fig_sankey", "mapped"),
    c("Composition", "Marimekko", "show joint categorical composition", "two categorical variables", "support counts", "fig_marimekko", "recipe"),
    c("Matrix and field", "heatmap", "show values over a discrete matrix", "row-column-value", "not usually shown", "fig_heatmap", "recipe"),
    c("Matrix and field", "tile map", "show values over a regular field", "x-y-value grid", "support or interval optional", "fig_heatmap", "mapped"),
    c("Matrix and field", "calendar heatmap", "show daily temporal pattern", "date and value", "not usually shown", "fig_heatmap", "mapped"),
    c("Spatial", "choropleth", "show normalized regional values", "geometries and rate", "interval or reliability map", "fig_choropleth", "recipe"),
    c("Spatial", "proportional symbols", "show regional magnitude", "locations and magnitude", "support visible", "fig_spatial_symbols", "recipe"),
    c("Spatial", "dot-density map", "show spatial distribution of counts", "geometries and counts", "dot value stated", NA, "mapped"),
    c("Spatial", "flow map", "show movement between locations", "origin-destination flows", "support counts", "fig_flow_map", "recipe"),
    c("Spatial", "cartogram", "resize regions by magnitude", "geometries and magnitude", "support visible", NA, "mapped"),
    c("Spatiotemporal", "small-multiple map", "compare geographic change over ordered time", "repeated geometries by time", "common scale", "fig_spatiotemporal_map", "recipe"),
    c("Spatiotemporal", "change choropleth", "map within-region change", "geometries and paired values", "difference interval", "fig_choropleth", "mapped"),
    c("Spatiotemporal", "space-time cube", "show spatial events through continuous time", "coordinates, time and magnitude", "support density", NA, "mapped"),
    c("Spatiotemporal", "animated map", "communicate geographic evolution in digital media", "repeated geometries by time", "common scale and static companion", "fig_spatiotemporal_map", "mapped"),
    c("Decision", "risk matrix", "prioritize likelihood and impact", "ordinal likelihood-impact", "not applicable", "fig_risk_matrix", "recipe"),
    c("Decision", "cost-effectiveness plane", "show incremental cost and effect", "paired simulation draws", "uncertainty cloud", "fig_ce_plane", "recipe"),
    c("Decision", "CEAC", "show probability cost-effective by threshold", "threshold and probability", "Monte Carlo uncertainty", "fig_ceac", "recipe"),
    c("Decision", "value-of-information curve", "show value of reducing uncertainty", "threshold and expected value", "Monte Carlo uncertainty", "fig_evpi", "recipe"),
    c("Network", "node-link graph", "show topology and relationships", "nodes and edges", "support or edge weight", "diagram", "mapped"),
    c("Network", "Sankey diagram", "show conserved flows", "weighted directed transitions", "support counts", "fig_sankey", "recipe"),
    c("Network", "chord diagram", "show dense reciprocal relationships", "weighted adjacency matrix", "support counts", "fig_chord", "recipe"),
    c("Hierarchy", "treemap", "show hierarchical part-to-whole", "tree with magnitude", "denominator visible", "fig_treemap", "recipe"),
    c("Hierarchy", "icicle", "compare nested branch magnitude on aligned bands", "tree with magnitude", "denominator visible", "fig_icicle", "recipe"),
    c("Hierarchy", "sunburst", "show nested composition", "tree with magnitude", "denominator visible", "fig_sunburst", "recipe"),
    c("Hierarchy", "dendrogram", "show hierarchical clustering", "distance tree", "stability optional", "fig_dendrogram", "recipe"),
    c("Process", "flow diagram", "explain ordered stages and attrition", "nodes and directed edges", "counts at stages", "diagram", "mapped"),
    c("Process", "CONSORT flow", "report trial participant flow", "stage counts and exclusions", "denominators", "fig_consort", "recipe"),
    c("Process", "causal DAG", "declare assumed causal structure", "variables and directed edges", "conceptual", "fig_dag", "recipe"),
    c("Time", "study timeline", "place study events on a real time axis", "events with dates or offsets", "windows optional", "fig_timeline", "recipe"),
    c("Process", "analysis pipeline", "let a reader reproduce a method", "ordered steps with parameters", "not applicable", "fig_pipeline", "recipe"),
    c("Process", "compartmental model", "state a transmission model, not picture one", "compartments and labelled rates", "not applicable", "fig_compartmental", "recipe"),
    c("Process", "pathway diagram", "show typed molecular relations", "entities and typed relations", "not applicable", "fig_pathway", "recipe"),
    c("Relationship", "projected surface", "show a fitted surface when its shape is the finding", "regular x-y-z grid", "interval blankets optional", "fig_surface3d", "recipe"),
    c("Evidence synthesis", "albatross plot", "recover effect magnitude when pooling is impossible", "P value, sample size and direction", "contours of implied effect", "fig_albatross", "recipe"),
    c("Evidence synthesis", "PRISMA flow", "report record flow through a review", "stage counts and exclusion reasons", "denominators", "fig_prisma", "recipe"),
    c("Diagnostic", "summary ROC", "pool paired sensitivity and specificity", "study-level sensitivity and specificity", "confidence region", "fig_sroc", "recipe"),
    c("Evidence synthesis", "Baujat plot", "name the studies driving heterogeneity", "per-study Q contribution and influence", "not applicable", "fig_baujat", "recipe"),
    c("Evidence synthesis", "L'Abbe plot", "expose baseline risk behind a ratio", "arm-wise event rates", "trial size", "fig_labbe", "recipe"),
    c("Evidence synthesis", "cumulative meta-analysis", "show when the evidence settled", "ordered cumulative estimates", "confidence interval", "fig_cumulative_ma", "recipe"),
    c("Evidence synthesis", "network geometry", "show what a network can identify", "direct comparisons between treatments", "trials per comparison", "fig_nma_network", "recipe"),
    c("Evidence synthesis", "rankogram", "show ranking without false certainty", "rank probabilities by treatment", "posterior probability", "fig_rankogram", "recipe"),
    c("Comparison", "covariate balance", "show a design achieved balance", "standardised differences before and after", "not applicable", "fig_balance", "recipe"),
    c("Time", "event study", "test parallel trends and show the effect", "lead and lag coefficients", "confidence interval", "fig_event_study", "recipe"),
    c("Relationship", "regression discontinuity", "show a jump at a threshold", "running variable and outcome", "separate fits either side", "fig_rdd", "recipe"),
    c("Time", "Lexis diagram", "separate age, period and cohort", "lifelines on an age-time plane", "not applicable", "fig_lexis", "recipe"),
    c("Time", "epidemic curve", "show outbreak onsets over time", "cases by onset interval", "interval stated", "fig_epicurve", "recipe"),
    c("Comparison", "bar chart", "compare category magnitudes", "category and value", "interval optional", "fig_bar", "recipe"),
    c("Multivariate", "scatterplot matrix", "find the pair that matters", "several continuous variables", "raw support", "fig_splom", "recipe"),
    c("Composition", "UpSet plot", "show set intersections beyond three sets", "exclusive intersection sizes", "denominator visible", "fig_upset", "recipe"),
    c("Composition", "waterfall", "decompose a difference into contributions", "ordered signed contributions", "reconciliation", "fig_waterfall", "recipe"),
    c("Composition", "streamgraph", "show composition shape when the total is context", "parts over time", "not usually shown", "fig_stream", "recipe"),
    c("Time", "bump chart", "show changes in ordering", "rank or value by period", "not shown; margin is discarded", "fig_bump", "recipe"),
    c("Distribution", "population pyramid", "show age structure by sex", "counts by age band and sex", "denominator stated", "fig_pyramid", "recipe"),
    c("Time", "swimmer plot", "keep individual trajectories visible", "per-participant intervals and events", "ongoing follow-up marked", "fig_swimmer", "recipe"),
    c("Decision", "Markov trace", "show a model cohort behaving", "state occupancy by cycle", "closure verified", "fig_markov_trace", "recipe"),
    c("Decision", "efficiency frontier", "exclude dominated strategies", "cost and effect per strategy", "dominance classified", "fig_frontier", "recipe"),
    c("Decision", "EVPPI", "target research at the parameters worth measuring", "expected value by parameter", "bounded by overall EVPI", "fig_evppi", "recipe")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("family", "chart", "question", "data_shape",
                  "uncertainty", "vellum_recipe", "status")
  out
}

#' Search Vellum's chart taxonomy
#'
#' @description
#' Every visual grammar the package recognises, classified three ways so that
#' the same table answers three different questions.
#'
#' `family` is the analytical question -- distribution, comparison, relationship
#' -- which is what someone with a finding searches on. `structure` is the shape
#' of the data, for someone who arrives with a hierarchy or a network and needs
#' to know what consumes it. `encoding` is the perceptual channel the form's
#' primary comparison rides on, ordered by how accurately people decode it; it is
#' the axis that explains why one form beats another for the same data.
#'
#' See [chart_vocabulary()] for the two closed vocabularies.
#'
#' @param family Optional family filter.
#' @param status Optional implementation-status filter: `"recipe"`, `"mapped"`,
#'   or `"example"`.
#' @param structure Optional data-shape filter.
#' @param encoding Optional perceptual-channel filter.
#' @return A data frame with one row per visual grammar.
#' @export
#' @examples
#' chart_catalogue(structure = "hierarchy")
#' chart_catalogue(encoding = "angle")
#' table(chart_catalogue()$structure)
chart_catalogue <- function(family = NULL, status = NULL, structure = NULL,
                            encoding = NULL) {
  x <- catalogue_rows()
  x <- cbind(x, chart_schema_columns(x$chart))
  if (!is.null(family)) {
    x <- x[tolower(x$family) %in% tolower(family), , drop = FALSE]
  }
  if (!is.null(status)) x <- x[x$status %in% status, , drop = FALSE]
  if (!is.null(structure)) {
    structure <- match.arg(structure, names(.chart_structures), several.ok = TRUE)
    x <- x[x$structure %in% structure, , drop = FALSE]
  }
  if (!is.null(encoding)) {
    encoding <- match.arg(encoding, names(.chart_encodings), several.ok = TRUE)
    x <- x[x$encoding %in% encoding, , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

#' Recommend visual grammars from a plain-language question
#'
#' @param question Words describing the intended comparison or analytical task.
#' @param data_shape Optional words describing the available data.
#' @param require_uncertainty Prefer forms that explicitly represent uncertainty.
#' @param top Maximum number of recommendations.
#' @return A ranked subset of [chart_catalogue()].
#' @export
recommend_plot <- function(question, data_shape = NULL,
                           require_uncertainty = FALSE, top = 6) {
  check_string(question, "question")
  if (!is.null(data_shape)) check_string(data_shape, "data_shape")
  check_bool(require_uncertainty, "require_uncertainty")
  check_number(top, "top", min = 1)
  x <- catalogue_rows()
  words <- unique(strsplit(tolower(paste(question, data_shape %||% "")),
                           "[^a-z0-9]+")[[1]])
  words <- words[nchar(words) >= 3]
  hay <- tolower(paste(x$family, x$chart, x$question, x$data_shape))
  score <- vapply(hay, function(s) sum(vapply(words, grepl, logical(1), x = s,
                                              fixed = TRUE)), numeric(1))
  score <- score + ifelse(x$status == "recipe", 0.75,
                          ifelse(x$status == "example", 0.25, 0))
  if (require_uncertainty) {
    score <- score + ifelse(grepl("interval|band|distribution|support|uncertainty",
                                  x$uncertainty), 0.75, -0.5)
  }
  x$score <- score
  x <- x[order(-x$score, x$family, x$chart), , drop = FALSE]
  x <- utils::head(x[x$score > 0, , drop = FALSE], as.integer(top))
  rownames(x) <- NULL
  x
}

#' Visual map of the chart taxonomy
#'
#' @param venue Target venue.
#' @return A ggplot overview of families, implementation status and chart forms.
#' @export
fig_chart_taxonomy <- function(venue = "slide") {
  d <- chart_catalogue()
  count <- stats::aggregate(
    list(n = d$chart),
    list(family = d$family, status = d$status),
    length
  )
  totals <- stats::aggregate(n ~ family, count, sum)
  levels <- totals$family[order(totals$n)]
  count$family <- factor(count$family, levels = levels)
  count$status <- factor(count$status, levels = c("mapped", "example", "recipe"))
  count$label <- ifelse(count$n > 1, count$n, "")
  count$ink <- ifelse(count$status == "mapped", "#000000", "#FFFFFF")
  cols <- c(recipe = "#0072B2", example = "#009E73", mapped = "#B6B6B6")
  ggplot2::ggplot(count, ggplot2::aes(.data$n, .data$family,
                                      fill = .data$status)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      position = ggplot2::position_stack(vjust = 0.5),
      colour = count$ink,
      size = fs(vellum_tokens(venue)$type$size$small),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = seq(0, max(totals$n), by = 2),
                                expand = ggplot2::expansion(mult = c(0, .03))) +
    ggplot2::labs(x = "Visual grammars", y = NULL, fill = "Vellum coverage",
                  caption = "Recipe: reusable package function \u00b7 Example: rendered catalogue \u00b7 Mapped: shares an implemented grammar") +
    theme_vellum_profile("presentation", venue = venue) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "bottom")
}
