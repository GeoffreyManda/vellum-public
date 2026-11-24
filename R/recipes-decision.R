# Decision-analytic recipes. These take the output of an economic model, not the
# model itself: vellum draws prepared estimates and checks that what it has been
# handed is internally coherent, which is a different job from computing it.

#' Cost-effectiveness plane
#'
#' @description
#' Incremental cost against incremental effect, with the origin at the
#' comparator. Each point is one iteration of a probabilistic analysis, so the
#' cloud is the joint uncertainty in both dimensions -- which is the thing a
#' point estimate of the ICER cannot show.
#'
#' The willingness-to-pay line is what makes the plane readable: everything below
#' it is cost-effective at that threshold and everything above is not, so the
#' proportion of the cloud beneath the line is the probability of
#' cost-effectiveness, and it is the same number [fig_ceac()] plots against
#' threshold.
#'
#' The quadrants are not symmetric and the figure says so. A cloud spanning the
#' vertical axis means the sign of the effect is uncertain, and an ICER computed
#' across that boundary is meaningless: the same ratio describes "cheaper and
#' better" and "dearer and worse".
#'
#' @param data A data frame of simulation draws.
#' @param effect,cost Column names giving incremental effect and incremental cost.
#' @param group Optional column when several strategies are compared to one
#'   comparator.
#' @param wtp Willingness-to-pay threshold, in cost units per effect unit. `NULL`
#'   draws no threshold line.
#' @param ellipse Draw a normal-approximation confidence ellipse per group.
#' @param level Ellipse coverage.
#' @param effect_label,cost_label Axis labels; name the units, because a plane
#'   without them cannot be read.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(4)
#' draws <- data.frame(
#'   effect = stats::rnorm(1000, 0.08, 0.05),
#'   cost = stats::rnorm(1000, 1400, 900)
#' )
#' fig_ce_plane(draws, wtp = 20000)
fig_ce_plane <- function(data, effect = "effect", cost = "cost", group = NULL,
                         wtp = NULL, ellipse = TRUE, level = 0.95,
                         effect_label = "Incremental effect",
                         cost_label = "Incremental cost",
                         venue = "nature", palette = "okabe_ito") {
  check_bool(ellipse, "ellipse")
  check_number(level, "level", min = 0.5, max = 0.9999)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame of draws.")
  missing_cols <- setdiff(c(effect, cost, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(e = as.numeric(data[[effect]]), c = as.numeric(data[[cost]]),
                  stringsAsFactors = FALSE)
  check_numeric(d$e, effect)
  check_numeric(d$c, cost)
  d$group <- if (is.null(group)) "" else as.character(data[[group]])
  d <- d[is.finite(d$e) & is.finite(d$c), , drop = FALSE]
  if (!nrow(d)) cli::cli_abort("No finite draws to plot.")

  tk <- vellum_tokens(venue)
  lim_e <- max(abs(range(d$e))) * 1.08
  lim_c <- max(abs(range(d$c))) * 1.08

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$e, .data$c))

  if (!is.null(wtp)) {
    check_number(wtp, "wtp", min = 0)
    # Drawn before the points, because the line is the reference and the cloud is
    # the finding; a threshold on top of the data reads as part of the data.
    p <- p + ggplot2::geom_abline(
      slope = wtp, intercept = 0, colour = tk$ink$weak,
      linewidth = tk$rule$geom, linetype = "22")
  }
  p <- p +
    ggplot2::geom_hline(yintercept = 0, colour = tk$ink$axis, linewidth = tk$rule$axis) +
    ggplot2::geom_vline(xintercept = 0, colour = tk$ink$axis, linewidth = tk$rule$axis)

  if (is.null(group)) {
    p <- p + ggplot2::geom_point(colour = tk$ink$data, size = tk$geom$point_small,
                                 alpha = tk$geom$alpha_light, stroke = 0)
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = .data$group),
                                 size = tk$geom$point_small,
                                 alpha = tk$geom$alpha_light, stroke = 0) +
      scale_colour_vellum(palette)
  }

  if (ellipse) {
    ell <- do.call(rbind, lapply(split(d, d$group), function(g) {
      if (nrow(g) < 4L) return(NULL)
      ce <- stats::cov(cbind(g$e, g$c))
      if (any(!is.finite(ce))) return(NULL)
      ev <- eigen(ce, symmetric = TRUE)
      if (any(ev$values <= 0)) return(NULL)
      r <- sqrt(stats::qchisq(level, df = 2))
      a <- seq(0, 2 * pi, length.out = 121)
      xy <- ev$vectors %*% diag(sqrt(ev$values)) %*% rbind(cos(a), sin(a)) * r
      data.frame(e = xy[1, ] + mean(g$e), c = xy[2, ] + mean(g$c),
                 group = g$group[1], stringsAsFactors = FALSE)
    }))
    if (!is.null(ell)) {
      p <- p + if (is.null(group)) {
        ggplot2::geom_path(data = ell, ggplot2::aes(.data$e, .data$c),
                           colour = tk$ink$text, linewidth = tk$rule$geom)
      } else {
        ggplot2::geom_path(data = ell,
                           ggplot2::aes(.data$e, .data$c, colour = .data$group),
                           linewidth = tk$rule$emphasis)
      }
    }
  }

  caption <- if (!is.null(wtp)) {
    below <- mean(d$c <= wtp * d$e)
    sprintf("Threshold %s per unit effect; %.1f%% of draws below it.",
            format(wtp, big.mark = " ", trim = TRUE), 100 * below)
  } else NULL

  p +
    ggplot2::coord_cartesian(xlim = c(-lim_e, lim_e), ylim = c(-lim_c, lim_c)) +
    ggplot2::labs(x = effect_label, y = cost_label, colour = NULL, caption = caption) +
    theme_vellum(venue = venue, grid = "none", axis_style = "none")
}

#' Cost-effectiveness acceptability curve
#'
#' @description
#' The probability that each strategy is optimal, across willingness-to-pay
#' thresholds. It answers the question a single ICER cannot: not "is this good
#' value" but "at what price does the answer change".
#'
#' A set of CEACs describes a partition -- at every threshold exactly one
#' strategy is optimal -- so the curves must sum to one at each threshold. That
#' is checked here, because a set that does not sum to one has usually been built
#' from pairwise comparisons against a common comparator, which answers a
#' different question and cannot be read as this figure implies.
#'
#' A CEAC says nothing about the *magnitude* of the loss from being wrong. Pair
#' it with [fig_evpi()], which does.
#'
#' @param data A data frame in long form.
#' @param wtp,probability Column names giving threshold and probability optimal.
#' @param group Column naming the strategy. `NULL` for a single curve.
#' @param frontier Optional logical column marking the strategy on the
#'   cost-effectiveness frontier at each threshold, drawn as a heavier line.
#' @param check Verify that the probabilities sum to one at every threshold.
#' @param tolerance Absolute tolerance for that check.
#' @param currency Label for the threshold axis.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' wtp <- seq(0, 50000, by = 1000)
#' pa <- stats::plogis((wtp - 20000) / 6000)
#' d <- data.frame(
#'   wtp = rep(wtp, 2),
#'   strategy = rep(c("Usual care", "New programme"), each = length(wtp)),
#'   prob = c(1 - pa, pa)
#' )
#' fig_ceac(d, wtp = "wtp", probability = "prob", group = "strategy")
fig_ceac <- function(data, wtp = "wtp", probability = "probability", group = NULL,
                     frontier = NULL, check = TRUE, tolerance = 0.01,
                     currency = "Willingness to pay per unit effect",
                     venue = "nature", palette = "okabe_ito") {
  check_bool(check, "check")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(wtp, probability, group, frontier), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(wtp = as.numeric(data[[wtp]]), p = as.numeric(data[[probability]]),
                  stringsAsFactors = FALSE)
  check_numeric(d$wtp, wtp)
  check_numeric(d$p, probability)
  if (any(d$p < 0 | d$p > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{sum(d$p < 0 | d$p > 1, na.rm = TRUE)} probabilit{?y/ies} {?is/are} outside [0, 1].",
      i = "The vertical axis of a CEAC is a probability, not a net benefit."
    ))
  }
  d$group <- if (is.null(group)) "" else as.character(data[[group]])

  if (check && !is.null(group)) {
    sums <- vapply(split(d$p, d$wtp), sum, numeric(1))
    bad <- names(sums)[abs(sums - 1) > tolerance]
    if (length(bad)) {
      cli::cli_abort(c(
        "The curves do not sum to 1 at {length(bad)} threshold{?s}.",
        x = sprintf("For example at %s they sum to %.3f.", bad[1], sums[bad[1]]),
        i = "At any threshold exactly one strategy is optimal, so the probabilities partition.",
        i = "Curves built from separate pairwise comparisons against one comparator do not partition and cannot be read this way.",
        i = "Set {.code check = FALSE} to draw them anyway."
      ))
    }
  }

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$wtp, .data$p))
  p <- p + if (is.null(group)) {
    ggplot2::geom_line(colour = tk$ink$data, linewidth = tk$rule$emphasis)
  } else {
    ggplot2::geom_line(ggplot2::aes(colour = .data$group), linewidth = tk$rule$emphasis)
  }

  if (!is.null(frontier)) {
    front <- d[as.logical(data[[frontier]]), , drop = FALSE]
    if (nrow(front)) {
      p <- p + ggplot2::geom_point(data = front, colour = tk$ink$text,
                                   size = tk$geom$point_small, stroke = 0)
    }
  }

  out <- p +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                                labels = function(z) sprintf("%.0f%%", 100 * z),
                                expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_x_continuous(labels = function(z) format(z, big.mark = " ", trim = TRUE),
                                expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = currency, y = "Probability optimal", colour = NULL) +
    theme_vellum(venue = venue, grid = "y")
  if (!is.null(group)) out <- out + scale_colour_vellum(palette)
  out
}

#' Value-of-information curve
#'
#' @description
#' The expected value of resolving uncertainty, across willingness-to-pay
#' thresholds. It converts "we are not sure" into a quantity in the same units as
#' the decision, which is the only form in which uncertainty can be traded
#' against the cost of collecting more evidence.
#'
#' EVPI is bounded below by zero -- information cannot make a decision worse in
#' expectation -- so a negative value is an error in the calculation rather than
#' a finding, and is refused. It peaks where the decision is closest to a tie,
#' which is usually not where the point estimate sits.
#'
#' Give `population` to scale per-person EVPI to the decision it actually
#' informs. The per-person figure is almost always too small to justify a trial
#' and the population figure almost always is not; which of the two is quoted
#' has decided more funding arguments than the model has.
#'
#' @param data A data frame in long form.
#' @param wtp,value Column names giving threshold and expected value.
#' @param group Optional column, for EVPPI by parameter alongside overall EVPI.
#' @param population Number of people the decision applies to, or `NULL` for
#'   per-person values.
#' @param currency Label for the value axis.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' wtp <- seq(0, 50000, by = 1000)
#' d <- data.frame(wtp = wtp, evpi = 400 * exp(-((wtp - 20000) / 9000)^2))
#' fig_evpi(d, wtp = "wtp", value = "evpi", population = 30000)
fig_evpi <- function(data, wtp = "wtp", value = "value", group = NULL,
                     population = NULL, currency = "Expected value of information",
                     venue = "nature", palette = "okabe_ito") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(wtp, value, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(wtp = as.numeric(data[[wtp]]), v = as.numeric(data[[value]]),
                  stringsAsFactors = FALSE)
  check_numeric(d$wtp, wtp)
  check_numeric(d$v, value)
  if (any(d$v < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{sum(d$v < 0, na.rm = TRUE)} value{?s} {?is/are} negative.",
      i = "Perfect information cannot make a decision worse in expectation, so EVPI is bounded below by zero.",
      i = "A negative value means the optimal strategy was chosen per draw rather than on the expectation."
    ))
  }
  d$group <- if (is.null(group)) "" else as.character(data[[group]])

  if (!is.null(population)) {
    check_number(population, "population", min = 0)
    d$v <- d$v * population
  }

  tk <- vellum_tokens(venue)
  peak <- d[which.max(d$v), , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$wtp, .data$v))
  p <- p + if (is.null(group)) {
    # A list, not a sum: two ggproto layers cannot be added to each other, only
    # to a plot.
    list(ggplot2::geom_area(fill = tk$ink$grid),
         ggplot2::geom_line(colour = tk$ink$data, linewidth = tk$rule$emphasis))
  } else {
    ggplot2::geom_line(ggplot2::aes(colour = .data$group), linewidth = tk$rule$emphasis)
  }

  out <- p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)),
                                labels = function(z) format(z, big.mark = " ", trim = TRUE,
                                                            scientific = FALSE)) +
    ggplot2::scale_x_continuous(labels = function(z) format(z, big.mark = " ", trim = TRUE),
                                expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(
      x = "Willingness to pay per unit effect", y = currency, colour = NULL,
      caption = sprintf("Peak %s at a threshold of %s%s.",
                        format(signif(peak$v, 3), big.mark = " ", trim = TRUE),
                        format(peak$wtp, big.mark = " ", trim = TRUE),
                        if (is.null(population)) " (per person)" else
                          sprintf(" (population of %s)",
                                  format(population, big.mark = " ", trim = TRUE)))) +
    theme_vellum(venue = venue, grid = "y")
  if (!is.null(group)) out <- out + scale_colour_vellum(palette)
  out
}

#' Decision curve
#'
#' @description
#' Net benefit against threshold probability, for judging whether a model is
#' worth acting on rather than only whether it discriminates. A model can have
#' excellent AUC and still be useless clinically, because discrimination says
#' nothing about the cost of the two kinds of mistake; net benefit weighs a false
#' positive against a true positive at the odds the threshold implies.
#'
#' The two reference strategies are the whole point and are drawn by default:
#' treat everyone, and treat no one. A model is worth using only over the range
#' of thresholds where its curve is above both. Reporting a decision curve
#' without them is reporting the answer without the question.
#'
#' @param data A data frame in long form.
#' @param threshold,net_benefit Column names giving threshold probability and net
#'   benefit.
#' @param group Optional column naming the model.
#' @param prevalence Outcome prevalence, used to draw the treat-all reference. If
#'   `NULL`, supply the reference yourself as a group in `data`.
#' @param clip Lower limit of the vertical axis. Net benefit is unbounded below
#'   and the interesting range is just above zero, so an unclipped axis is mostly
#'   empty.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' th <- seq(0.01, 0.5, by = 0.01)
#' d <- data.frame(threshold = th, nb = 0.14 - 0.28 * th + 0.02 * sin(th * 22))
#' fig_decision_curve(d, threshold = "threshold", net_benefit = "nb",
#'                    prevalence = 0.16)
fig_decision_curve <- function(data, threshold = "threshold",
                               net_benefit = "net_benefit", group = NULL,
                               prevalence = NULL, clip = NULL,
                               venue = "nature", palette = "okabe_ito") {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(threshold, net_benefit, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(th = as.numeric(data[[threshold]]),
                  nb = as.numeric(data[[net_benefit]]), stringsAsFactors = FALSE)
  check_numeric(d$th, threshold)
  check_numeric(d$nb, net_benefit)
  if (any(d$th <= 0 | d$th >= 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Threshold probabilities must lie in (0, 1).",
      i = "The threshold is the probability at which a reader would act; 0 and 1 make the odds weight undefined."
    ))
  }
  d$group <- if (is.null(group)) "Model" else as.character(data[[group]])

  tk <- vellum_tokens(venue)
  refs <- data.frame(th = numeric(0), nb = numeric(0), ref = character(0))
  if (!is.null(prevalence)) {
    check_number(prevalence, "prevalence", min = 0, max = 1)
    th <- sort(unique(d$th))
    refs <- rbind(
      data.frame(th = th, nb = 0, ref = "Treat none", stringsAsFactors = FALSE),
      data.frame(th = th, nb = prevalence - (1 - prevalence) * th / (1 - th),
                 ref = "Treat all", stringsAsFactors = FALSE)
    )
  } else {
    refs <- data.frame(th = sort(unique(d$th)), nb = 0, ref = "Treat none",
                       stringsAsFactors = FALSE)
  }

  clip <- clip %||% (-0.5 * max(abs(d$nb), na.rm = TRUE))

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = refs,
                       ggplot2::aes(.data$th, .data$nb, group = .data$ref),
                       colour = tk$ink$weak, linewidth = tk$rule$geom, linetype = "22")
  p <- p + if (is.null(group)) {
    ggplot2::geom_line(data = d, ggplot2::aes(.data$th, .data$nb),
                       colour = tk$ink$data, linewidth = tk$rule$emphasis)
  } else {
    ggplot2::geom_line(data = d,
                       ggplot2::aes(.data$th, .data$nb, colour = .data$group),
                       linewidth = tk$rule$emphasis)
  }

  # The references are named on the plot rather than in a legend, because they
  # are not a third and fourth series: they are the floor the model has to beat.
  #
  # Anchored at the left end, not the right: treat-all falls away steeply and by
  # the right-hand edge it is far below the clipped axis, so a label placed at
  # its last point is a label the reader never sees.
  ref_labels <- refs[!duplicated(refs$ref), , drop = FALSE]
  out <- p +
    ggplot2::geom_text(data = ref_labels,
                       ggplot2::aes(.data$th, .data$nb, label = .data$ref),
                       hjust = 0, vjust = -0.7, size = fs(tk$type$size$small),
                       colour = tk$ink$weak, family = tk$type$family) +
    ggplot2::coord_cartesian(ylim = c(clip, NA)) +
    ggplot2::labs(x = "Threshold probability", y = "Net benefit", colour = NULL) +
    theme_vellum(venue = venue, grid = "y")
  if (!is.null(group)) out <- out + scale_colour_vellum(palette)
  out
}
