# Evidence-synthesis recipes, part two: the reporting diagram, diagnostic
# accuracy, and the four diagnostics that interrogate a pooled estimate rather
# than reporting it.

#' PRISMA flow diagram
#'
#' @description
#' The reporting diagram for a systematic review, in the PRISMA 2020 shape. Its
#' job is arithmetic made visible: a reader must be able to follow every record
#' from identification to inclusion and account for each one that left.
#'
#' The counts are therefore reconciled rather than drawn. Records identified,
#' less duplicates removed, must equal records screened; screened less excluded
#' must equal reports sought; and so on to the studies included. A PRISMA
#' diagram whose arithmetic does not close is the single most common defect in
#' published reviews, and it is invisible once drawn because every box looks
#' equally authoritative. Set `check = FALSE` to draw it anyway and the figure
#' will be wrong.
#'
#' Exclusions at full text must carry reasons. PRISMA 2020 requires it, and a
#' count without reasons cannot be appraised: it is the difference between "we
#' excluded 41 reports" and "we excluded 41 reports, 30 for the wrong
#' comparator".
#'
#' @param identified Records identified from databases and registers.
#' @param duplicates Records removed before screening.
#' @param screened Titles and abstracts screened.
#' @param screen_excluded Records excluded at title and abstract.
#' @param sought Reports sought for retrieval.
#' @param not_retrieved Reports not retrieved.
#' @param assessed Reports assessed for eligibility.
#' @param excluded A named numeric vector of full-text exclusions with reasons,
#'   for example `c("Wrong comparator" = 30, "No outcome data" = 11)`.
#' @param included Studies included in the review.
#' @param other_identified Optionally, records identified by other methods
#'   (citation searching, registers, contact with authors).
#' @param check Reconcile the counts.
#' @param venue Target venue.
#' @param ... Passed to [diagram()].
#'
#' @return A `ggplot` object.
#' @references Page MJ et al. (2021) The PRISMA 2020 statement. *BMJ* 372:n71.
#' @export
#' @examples
#' fig_prisma(
#'   identified = 2841, duplicates = 612, screened = 2229,
#'   screen_excluded = 2012, sought = 217, not_retrieved = 9,
#'   assessed = 208, included = 24,
#'   excluded = c("Wrong comparator" = 96, "Wrong outcome" = 61,
#'                "Not a trial" = 27)
#' )
fig_prisma <- function(identified, duplicates, screened, screen_excluded,
                       sought, not_retrieved, assessed, excluded, included,
                       other_identified = 0, check = TRUE,
                       venue = "nature", ...) {
  nums <- list(identified = identified, duplicates = duplicates,
               screened = screened, screen_excluded = screen_excluded,
               sought = sought, not_retrieved = not_retrieved,
               assessed = assessed, included = included,
               other_identified = other_identified)
  for (nm in names(nums)) check_number(nums[[nm]], nm, min = 0)
  check_bool(check, "check")
  if (!is.numeric(excluded) || is.null(names(excluded)) || !length(excluded)) {
    cli::cli_abort(c(
      "{.arg excluded} must be a named numeric vector of reasons.",
      i = "PRISMA 2020 requires reasons for full-text exclusions; a bare count cannot be appraised."
    ))
  }

  if (check) {
    steps <- list(
      c("identified less duplicates", identified + other_identified - duplicates, screened),
      c("screened less excluded", screened - screen_excluded, sought),
      c("sought less not retrieved", sought - not_retrieved, assessed),
      c("assessed less excluded", assessed - sum(excluded), included)
    )
    bad <- Filter(function(x) as.numeric(x[2]) != as.numeric(x[3]), steps)
    if (length(bad)) {
      cli::cli_abort(c(
        "The record counts do not reconcile at {length(bad)} step{?s}.",
        stats::setNames(vapply(bad, function(x)
          sprintf("%s gives %s, but the next box says %s.", x[1], x[2], x[3]),
          character(1)), rep("x", length(bad))),
        i = "Every record leaving a stage must arrive at the next one or be excluded with a count.",
        i = "Set {.code check = FALSE} to draw it anyway, but the diagram will be wrong."
      ))
    }
  }

  fmt <- function(n) format(n, big.mark = " ", trim = TRUE)
  tk <- vellum_tokens(venue)
  spine <- list(
    c("identified", sprintf("Records identified\n(n = %s)", fmt(identified + other_identified))),
    c("screened", sprintf("Records screened\n(n = %s)", fmt(screened))),
    c("sought", sprintf("Reports sought for retrieval\n(n = %s)", fmt(sought))),
    c("assessed", sprintf("Reports assessed for eligibility\n(n = %s)", fmt(assessed))),
    c("included", sprintf("Studies included in review\n(n = %s)", fmt(included)))
  )
  side <- list(
    c("dup", sprintf("Duplicates removed\n(n = %s)", fmt(duplicates)), "identified"),
    c("scr_ex", sprintf("Records excluded\n(n = %s)", fmt(screen_excluded)), "screened"),
    c("nr", sprintf("Reports not retrieved\n(n = %s)", fmt(not_retrieved)), "sought"),
    c("ft_ex", paste0(sprintf("Reports excluded (n = %s)", fmt(sum(excluded))), "\n",
                      paste(sprintf("%s: %s", names(excluded), fmt(excluded)), collapse = "\n")),
      "assessed")
  )

  node_width <- 58
  spacing_y <- 24
  nodes <- lapply(seq_along(spine), function(i) {
    dg_node(spine[[i]][1], spine[[i]][2], shape = "rect", width = node_width,
            height = 14, role = "stage", fill = "#FFFFFF",
            colour = tk$ink$text, text_colour = tk$ink$text)
  })
  edges <- lapply(seq_len(length(spine) - 1L), function(i)
    dg_edge(spine[[i]][1], spine[[i + 1L]][1], "flow"))

  for (i in seq_along(side)) {
    idx <- match(side[[i]][3], vapply(spine, `[`, character(1), 1))
    lines <- length(strsplit(side[[i]][2], "\n")[[1]])
    nodes[[length(nodes) + 1L]] <- dg_node(
      side[[i]][1], side[[i]][2], shape = "rect", width = node_width * 0.94,
      height = 8 + 4.6 * lines, role = "excluded", fill = "#FFFFFF",
      colour = tk$ink$weak, text_colour = tk$ink$text,
      x = node_width / 2 + 10 + node_width * 0.47,
      y = -(idx - 0.5) * spacing_y)
    edges[[length(edges) + 1L]] <- dg_edge(side[[i]][3], side[[i]][1], "flow",
                                           route = "elbow")
  }

  diagram(nodes, edges, layout = "flow", direction = "down", venue = venue,
          node_width = node_width, spacing_y = spacing_y, ...)
}

#' Summary ROC plot for diagnostic test accuracy
#'
#' @description
#' Sensitivity against specificity for every study in a diagnostic accuracy
#' review, in ROC space. Use it where [fig_forest()] cannot: sensitivity and
#' specificity are not independent, because both move with the threshold each
#' study chose, so pooling them separately discards the correlation that
#' explains most of the heterogeneity.
#'
#' The axis is specificity running from 1 at the origin, which is the ROC
#' convention and the opposite of a plain scatter. Getting it backwards produces
#' a figure that looks like a summary ROC and reads inverted, so the direction is
#' stated on the axis rather than assumed.
#'
#' Study points are scaled by sample size where it is given, because a small
#' study sitting in the corner of ROC space carries the visual weight of a large
#' one otherwise.
#'
#' @param data A data frame, one row per study.
#' @param sensitivity,specificity Column names, as proportions in `[0, 1]`.
#' @param n Optional column of sample sizes, mapped to point area.
#' @param summary_point Optional length-two numeric, the pooled sensitivity and
#'   specificity.
#' @param region Optional data frame with `sensitivity` and `specificity`
#'   columns tracing a confidence or prediction region.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(8)
#' d <- data.frame(
#'   sens = stats::runif(14, 0.62, 0.95),
#'   spec = stats::runif(14, 0.55, 0.93),
#'   n = round(stats::runif(14, 60, 900))
#' )
#' fig_sroc(d, "sens", "spec", n = "n", summary_point = c(0.82, 0.78))
fig_sroc <- function(data, sensitivity = "sensitivity", specificity = "specificity",
                     n = NULL, summary_point = NULL, region = NULL,
                     venue = "nature", palette = "okabe_ito") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(sensitivity, specificity, n), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  se <- as.numeric(data[[sensitivity]])
  sp <- as.numeric(data[[specificity]])
  check_numeric(se, sensitivity)
  check_numeric(sp, specificity)
  if (any(se < 0 | se > 1 | sp < 0 | sp > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Sensitivity and specificity must be proportions in [0, 1].",
      i = "Percentages are a common cause; divide by 100."
    ))
  }

  d <- data.frame(x = 1 - sp, y = se, stringsAsFactors = FALSE)
  d$size <- if (is.null(n)) NA_real_ else as.numeric(data[[n]])
  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 2)

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = tk$ink$weak,
                         linewidth = tk$rule$grid, linetype = "22")
  if (!is.null(region)) {
    if (!all(c("sensitivity", "specificity") %in% names(region))) {
      cli::cli_abort("{.arg region} needs {.field sensitivity} and {.field specificity} columns.")
    }
    reg <- data.frame(x = 1 - region$specificity, y = region$sensitivity)
    p <- p + ggplot2::geom_polygon(data = reg, ggplot2::aes(.data$x, .data$y),
                                   fill = cols[1], alpha = 0.12,
                                   colour = cols[1], linewidth = tk$rule$geom)
  }
  p <- p + if (is.null(n)) {
    ggplot2::geom_point(colour = cols[1], size = tk$geom$point, alpha = 0.85)
  } else {
    list(ggplot2::geom_point(ggplot2::aes(size = .data$size), colour = cols[1],
                             alpha = 0.7),
         ggplot2::scale_size_area(max_size = 5, guide = "none"))
  }
  if (!is.null(summary_point)) {
    if (length(summary_point) != 2L) {
      cli::cli_abort("{.arg summary_point} must be {.code c(sensitivity, specificity)}.")
    }
    p <- p + ggplot2::annotate("point", x = 1 - summary_point[2], y = summary_point[1],
                               shape = 23, size = 3.4, fill = cols[2],
                               colour = tk$ink$text, stroke = 0.5)
  }
  p +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                                labels = function(z) sprintf("%.2f", 1 - z)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Specificity (decreasing)", y = "Sensitivity") +
    theme_vellum(venue = venue, grid = "none")
}

#' Baujat plot
#'
#' @description
#' Each study's contribution to heterogeneity against its influence on the
#' pooled estimate. It separates two things a single `I²` cannot: a study can be
#' an outlier without moving the answer, and a study can move the answer without
#' being an outlier. Only the top right corner is both, and those are the studies
#' a sensitivity analysis has to address.
#'
#' It is the natural companion to [fig_gosh()]. GOSH shows *that* the evidence
#' splits; a Baujat plot names the studies responsible.
#'
#' @param data A data frame, one row per study.
#' @param heterogeneity,influence Column names giving each study's contribution
#'   to `Q` and its influence on the pooled estimate.
#' @param label Column of study labels.
#' @param top Number of extreme studies to label, by distance from the origin.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(9)
#' d <- data.frame(
#'   study = paste("Trial", 1:18),
#'   q = c(stats::runif(15, 0, 4), 9.1, 12.4, 7.2),
#'   infl = c(stats::runif(15, 0, 0.25), 0.62, 0.41, 0.7)
#' )
#' fig_baujat(d, "q", "infl", label = "study")
fig_baujat <- function(data, heterogeneity = "heterogeneity", influence = "influence",
                       label = NULL, top = 4, venue = "nature",
                       palette = "okabe_ito") {
  check_number(top, "top", min = 0)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(heterogeneity, influence, label), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(x = as.numeric(data[[heterogeneity]]),
                  y = as.numeric(data[[influence]]), stringsAsFactors = FALSE)
  check_numeric(d$x, heterogeneity)
  check_numeric(d$y, influence)
  if (any(d$x < 0 | d$y < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Both axes are contributions and cannot be negative.",
      i = "A study's contribution to {.field Q} and its influence on the estimate are both squared quantities."
    ))
  }
  d$label <- if (is.null(label)) "" else as.character(data[[label]])

  # Rank by distance from the origin in units of each axis's own spread, so a
  # study is not called extreme merely because one axis happens to be wider.
  sx <- stats::sd(d$x); sy <- stats::sd(d$y)
  d$dist <- sqrt((d$x / ifelse(sx > 0, sx, 1))^2 + (d$y / ifelse(sy > 0, sy, 1))^2)
  d$extreme <- rank(-d$dist) <= top

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$extreme),
                        size = tk$geom$point, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = tk$ink$weak, `TRUE` = cols[2]),
                                 guide = "none") +
    ggplot2::labs(x = "Contribution to overall heterogeneity",
                  y = "Influence on the pooled estimate") +
    theme_vellum(venue = venue, grid = "none")
  if (!is.null(label) && any(d$extreme)) {
    p <- p + ggplot2::geom_text(
      data = d[d$extreme, , drop = FALSE], ggplot2::aes(label = .data$label),
      hjust = -0.14, size = fs(tk$type$size$small), colour = tk$ink$text,
      family = tk$type$family) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.18)))
  }
  p
}

#' L'Abbe plot
#'
#' @description
#' Event rate in the treatment arm against the control arm, one point per trial.
#' It shows what a forest plot of ratios hides: the *baseline risk*. Two trials
#' with the same risk ratio can sit at opposite ends of this plot, and whether an
#' effect is constant on the ratio scale or the difference scale is visible here
#' and nowhere else.
#'
#' The line of no effect is the diagonal, not a vertical. Points below it favour
#' treatment when the outcome is harmful, above it when the outcome is
#' beneficial — which is why the outcome's direction has to be stated rather
#' than inferred.
#'
#' @param data A data frame, one row per trial.
#' @param control,treatment Column names giving the event rate in each arm, as
#'   proportions in `[0, 1]`.
#' @param n Optional column of total trial size, mapped to point area.
#' @param rr Optional risk ratio to draw as a reference ray from the origin.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(10)
#' d <- data.frame(
#'   control = stats::runif(16, 0.08, 0.62),
#'   n = round(stats::runif(16, 80, 1200))
#' )
#' d$treatment <- pmin(0.98, d$control * stats::runif(16, 0.45, 0.95))
#' fig_labbe(d, rr = 0.7)
fig_labbe <- function(data, control = "control", treatment = "treatment",
                      n = NULL, rr = NULL, venue = "nature",
                      palette = "okabe_ito") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(control, treatment, n), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(x = as.numeric(data[[control]]),
                  y = as.numeric(data[[treatment]]), stringsAsFactors = FALSE)
  check_numeric(d$x, control)
  check_numeric(d$y, treatment)
  if (any(d$x < 0 | d$x > 1 | d$y < 0 | d$y > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Event rates must be proportions in [0, 1].",
      i = "A L'Abbe plot compares risks, not counts; divide by the arm's denominator."
    ))
  }
  d$size <- if (is.null(n)) NA_real_ else as.numeric(data[[n]])

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = tk$ink$text,
                         linewidth = tk$rule$geom)
  if (!is.null(rr)) {
    check_number(rr, "rr", min = 0)
    p <- p + ggplot2::geom_abline(slope = rr, intercept = 0, colour = cols[2],
                                  linewidth = tk$rule$geom, linetype = "22")
  }
  p <- p + if (is.null(n)) {
    ggplot2::geom_point(colour = cols[1], size = tk$geom$point, alpha = 0.85)
  } else {
    list(ggplot2::geom_point(ggplot2::aes(size = .data$size), colour = cols[1],
                             alpha = 0.7),
         ggplot2::scale_size_area(max_size = 5, guide = "none"))
  }
  p +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Event rate, control arm", y = "Event rate, treatment arm",
                  caption = if (!is.null(rr))
                    sprintf("Solid line: no effect. Dashed: risk ratio %s.", format(rr))
                  else "Solid line: no effect.") +
    theme_vellum(venue = venue, grid = "none")
}

#' Cumulative meta-analysis
#'
#' @description
#' The pooled estimate recomputed after each study is added, in the order they
#' arrived. It answers a question the final estimate cannot: *when* did the
#' evidence become conclusive, and has anything changed since.
#'
#' The ordering is the analysis. Sorted by year it shows when the answer was
#' first available, which is the form used to argue that later trials were
#' unnecessary. Sorted by precision it shows whether the result rests on one
#' large study. Sort deliberately and say which you used.
#'
#' The interval narrowing while the estimate stays put is the signature of a
#' settled question. An estimate that keeps moving is not settled however narrow
#' the interval has become.
#'
#' @param data A data frame, one row per cumulative step, already in order.
#' @param estimate,lower,upper Column names for the cumulative pooled estimate
#'   and its interval.
#' @param label Column of the study added at that step.
#' @param null Value representing no effect.
#' @param log_scale Draw the estimate axis on a log scale, for ratio measures.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   study = paste0("+ Trial ", 1:8),
#'   est = c(0.55, 0.62, 0.68, 0.71, 0.72, 0.73, 0.74, 0.74),
#'   lo = c(0.24, 0.36, 0.48, 0.56, 0.6, 0.64, 0.67, 0.69),
#'   hi = c(1.26, 1.07, 0.96, 0.9, 0.86, 0.83, 0.81, 0.79)
#' )
#' fig_cumulative_ma(d, "est", "lo", "hi", "study", log_scale = TRUE)
fig_cumulative_ma <- function(data, estimate = "estimate", lower = "lower",
                              upper = "upper", label = "label", null = 1,
                              log_scale = FALSE, venue = "nature",
                              palette = "okabe_ito") {
  check_bool(log_scale, "log_scale")
  check_number(null, "null")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(estimate, lower, upper, label), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(est = as.numeric(data[[estimate]]),
                  lo = as.numeric(data[[lower]]),
                  hi = as.numeric(data[[upper]]),
                  label = as.character(data[[label]]),
                  stringsAsFactors = FALSE)
  if (any(d$lo > d$est | d$hi < d$est, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{sum(d$lo > d$est | d$hi < d$est, na.rm = TRUE)} interval{?s} {?does/do} not contain {?its/their} estimate.",
      i = "Check that {.arg lower} and {.arg upper} are the right way round."
    ))
  }
  if (log_scale && any(d$lo <= 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg log_scale} needs positive limits.")
  }
  d$step <- seq_len(nrow(d))
  d$label <- factor(d$label, levels = rev(d$label))

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 2)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$est, .data$label)) +
    ggplot2::geom_vline(xintercept = null, colour = tk$ink$weak,
                        linewidth = tk$rule$geom, linetype = "22") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
                            colour = cols[1], linewidth = tk$geom$interval) +
    ggplot2::geom_point(colour = cols[1], size = tk$geom$point) +
    ggplot2::labs(x = "Cumulative pooled estimate", y = NULL) +
    theme_vellum(venue = venue, grid = "none")
  if (log_scale) p <- p + ggplot2::scale_x_log10()
  p
}

#' Network meta-analysis geometry
#'
#' @description
#' The comparison network: one node per treatment, sized by how much evidence it
#' carries, one edge per direct comparison, weighted by the number of trials
#' making it. Before any ranking is believable a reader has to see this, because
#' the network's shape decides what the model can identify.
#'
#' A star network gives no closed loop, so nothing is estimated both directly and
#' indirectly and consistency cannot be assessed at all — a fact about the
#' evidence that no amount of modelling repairs. That is reported here rather
#' than left to be noticed.
#'
#' @param edges A data frame of direct comparisons with `from`, `to` and a
#'   weight column.
#' @param weight Column giving the number of trials or participants per
#'   comparison.
#' @param nodes Optional data frame with `treatment` and a size column.
#' @param node_size Column in `nodes` mapped to node area.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' e <- data.frame(
#'   from = c("Placebo", "Placebo", "Placebo", "Drug A", "Drug B"),
#'   to = c("Drug A", "Drug B", "Drug C", "Drug B", "Drug C"),
#'   trials = c(9, 6, 4, 3, 2)
#' )
#' fig_nma_network(e, weight = "trials")
fig_nma_network <- function(edges, weight = "weight", nodes = NULL,
                            node_size = "n", venue = "nature",
                            palette = "okabe_ito") {
  if (!is.data.frame(edges) || !all(c("from", "to") %in% names(edges))) {
    cli::cli_abort("{.arg edges} must be a data frame with {.field from} and {.field to}.")
  }
  if (!weight %in% names(edges)) {
    cli::cli_abort("{.arg edges} has no column {.val {weight}}.")
  }
  w <- as.numeric(edges[[weight]])
  check_numeric(w, weight)

  tx <- unique(c(as.character(edges$from), as.character(edges$to)))
  k <- length(tx)
  if (k < 3L) cli::cli_abort("A network needs at least three treatments; {k} {?was/were} given.")

  size <- if (is.null(nodes)) {
    vapply(tx, function(t) sum(w[edges$from == t | edges$to == t]), numeric(1))
  } else {
    if (!all(c("treatment", node_size) %in% names(nodes))) {
      cli::cli_abort("{.arg nodes} needs {.field treatment} and {.val {node_size}}.")
    }
    stats::setNames(as.numeric(nodes[[node_size]]), as.character(nodes$treatment))[tx]
  }

  # A loop needs at least as many edges as nodes. Fewer, and every comparison is
  # identified one way only.
  closed <- nrow(edges) >= k
  ang <- seq(0, 2 * pi, length.out = k + 1)[seq_len(k)] - pi / 2
  pos <- data.frame(treatment = tx, x = cos(ang), y = sin(ang),
                    size = unname(size), stringsAsFactors = FALSE)
  rownames(pos) <- pos$treatment

  seg <- data.frame(
    x = pos[as.character(edges$from), "x"], y = pos[as.character(edges$from), "y"],
    xend = pos[as.character(edges$to), "x"], yend = pos[as.character(edges$to), "y"],
    w = w, stringsAsFactors = FALSE
  )

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 1)
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = seg,
                          ggplot2::aes(.data$x, .data$y, xend = .data$xend,
                                       yend = .data$yend, linewidth = .data$w),
                          colour = tk$ink$weak, lineend = "round") +
    ggplot2::scale_linewidth(range = c(0.3, 2.4), guide = "none") +
    ggplot2::geom_point(data = pos,
                        ggplot2::aes(.data$x, .data$y, size = .data$size),
                        colour = cols[1]) +
    ggplot2::scale_size_area(max_size = 9, guide = "none") +
    ggplot2::geom_text(data = pos,
                       ggplot2::aes(.data$x * 1.19, .data$y * 1.19,
                                    label = .data$treatment),
                       size = fs(tk$type$size$small), colour = tk$ink$text,
                       family = tk$type$family) +
    ggplot2::coord_equal(xlim = c(-1.42, 1.42), ylim = c(-1.3, 1.3)) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = if (closed)
                    sprintf("%d treatments, %d direct comparisons.", k, nrow(edges))
                  else sprintf(paste("%d treatments, %d direct comparisons: no closed loop,",
                                     "so consistency cannot be assessed."), k, nrow(edges))) +
    theme_vellum_void(venue = venue)
}

#' Rankogram
#'
#' @description
#' The probability that each treatment takes each rank, from a network
#' meta-analysis. It is the honest form of a league table: a single "best"
#' treatment implies a certainty the posterior rarely supports, while a rankogram
#' shows a treatment that is probably first and possibly fourth as exactly that.
#'
#' Rank probabilities partition — every treatment holds some rank — so the
#' probabilities for each rank must sum to one across treatments. That is
#' checked, because a set that does not partition has usually come from separate
#' pairwise comparisons and cannot be read this way.
#'
#' SUCRA summarises a curve as one number and is a convenience, not a result:
#' two treatments with the same SUCRA can have entirely different rank
#' distributions, and only the distribution says which you would bet on.
#'
#' @param data A data frame in long form.
#' @param treatment,rank,probability Column names.
#' @param check Verify that probabilities sum to one within each rank.
#' @param tolerance Absolute tolerance for that check.
#' @param cumulative Draw cumulative rank probabilities, from which SUCRA is the
#'   area beneath.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- expand.grid(treatment = c("Drug A", "Drug B", "Drug C", "Placebo"),
#'                  rank = 1:4)
#' d$p <- c(0.55, 0.28, 0.15, 0.02,
#'          0.30, 0.40, 0.24, 0.06,
#'          0.13, 0.26, 0.45, 0.16,
#'          0.02, 0.06, 0.16, 0.76)
#' fig_rankogram(d, "treatment", "rank", "p")
fig_rankogram <- function(data, treatment = "treatment", rank = "rank",
                          probability = "probability", check = TRUE,
                          tolerance = 0.01, cumulative = FALSE,
                          venue = "nature", palette = "okabe_ito") {
  check_bool(check, "check")
  check_bool(cumulative, "cumulative")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(treatment, rank, probability), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(tx = as.character(data[[treatment]]),
                  rank = as.numeric(data[[rank]]),
                  p = as.numeric(data[[probability]]), stringsAsFactors = FALSE)
  check_numeric(d$p, probability)
  if (any(d$p < 0 | d$p > 1, na.rm = TRUE)) {
    cli::cli_abort("Rank probabilities must lie in [0, 1].")
  }
  if (check) {
    sums <- vapply(split(d$p, d$rank), sum, numeric(1))
    bad <- names(sums)[abs(sums - 1) > tolerance]
    if (length(bad)) {
      cli::cli_abort(c(
        "Rank probabilities do not sum to 1 at rank{?s} {.val {bad}}.",
        x = sprintf("Rank %s sums to %.3f.", bad[1], sums[bad[1]]),
        i = "Every treatment holds some rank, so each rank's probabilities partition across treatments.",
        i = "Set {.code check = FALSE} to draw them anyway."
      ))
    }
  }

  d$tx <- factor(d$tx, levels = unique(d$tx))
  if (cumulative) {
    d <- d[order(d$tx, d$rank), , drop = FALSE]
    d$p <- unlist(lapply(split(d$p, d$tx), cumsum), use.names = FALSE)
  }

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(d, ggplot2::aes(.data$rank, .data$p, colour = .data$tx)) +
    ggplot2::geom_line(linewidth = tk$rule$emphasis) +
    ggplot2::geom_point(size = tk$geom$point_small) +
    scale_colour_vellum(palette) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$rank))) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = function(z) sprintf("%.0f%%", 100 * z)) +
    ggplot2::labs(x = "Rank", colour = NULL,
                  y = if (cumulative) "Cumulative probability" else "Probability of rank") +
    theme_vellum(venue = venue, grid = "y")
}
