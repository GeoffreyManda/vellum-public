# Causal and epidemiological recipes. vellum already draws the assumption --
# fig_dag() -- and these draw what follows from it: whether the design achieved
# the balance it claims, whether the effect appears when it should, and how a
# population or an outbreak is actually distributed.

#' Covariate balance plot
#'
#' @description
#' Standardised mean differences before and after matching, weighting or
#' stratification, one row per covariate. It is the evidence that a design did
#' what it was meant to do, and it is the figure a reader looks for before
#' believing an effect estimate from observational data.
#'
#' The 0.1 line is a convention, not a test. It has no distributional
#' justification and does not become one by being drawn; it is a widely used
#' rule of thumb for "probably close enough", and a covariate at 0.11 is not
#' meaningfully worse balanced than one at 0.09. Read the pattern, not the
#' crossings.
#'
#' The same covariates must appear before and after. Balancing on a different set
#' than you started with is not balance, it is a change of subject, so a
#' mismatch is refused rather than plotted.
#'
#' @param data A data frame in long form, one row per covariate and stage.
#' @param covariate,smd Column names giving the covariate and its standardised
#'   mean difference.
#' @param stage Column distinguishing before from after. `NULL` draws one stage.
#' @param absolute Plot `|SMD|`, which is the usual convention because the sign
#'   of an imbalance is rarely the point.
#' @param threshold Reference line, or `NULL` for none.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(11)
#' cov <- c("Age", "Sex", "Deprivation", "Comorbidity", "Smoking",
#'          "BMI", "Prior admission", "Region")
#' d <- data.frame(
#'   covariate = rep(cov, 2),
#'   stage = rep(c("Unadjusted", "Weighted"), each = length(cov)),
#'   smd = c(stats::runif(8, 0.08, 0.42), stats::runif(8, 0.005, 0.09))
#' )
#' fig_balance(d, "covariate", "smd", stage = "stage")
fig_balance <- function(data, covariate = "covariate", smd = "smd", stage = NULL,
                        absolute = TRUE, threshold = 0.1,
                        venue = "nature", palette = "okabe_ito") {
  check_bool(absolute, "absolute")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(covariate, smd, stage), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(cov = as.character(data[[covariate]]),
                  smd = as.numeric(data[[smd]]), stringsAsFactors = FALSE)
  check_numeric(d$smd, smd)
  d$stage <- if (is.null(stage)) "" else as.character(data[[stage]])

  if (!is.null(stage)) {
    sets <- lapply(split(d$cov, d$stage), sort)
    if (length(unique(lapply(sets, paste, collapse = "\r"))) > 1L) {
      all_cov <- unique(unlist(sets))
      missing_from <- vapply(names(sets), function(s)
        paste(setdiff(all_cov, sets[[s]]), collapse = ", "), character(1))
      cli::cli_abort(c(
        "The stages do not cover the same covariates.",
        stats::setNames(sprintf("%s is missing: %s", names(sets), missing_from),
                        rep("x", length(sets)))[nzchar(missing_from)],
        i = "Balance is a comparison of the same covariate set before and after; a different set is a different question."
      ))
    }
  }
  if (absolute) d$smd <- abs(d$smd)

  # Order by the worst stage, so the rows a reader scans first are the ones that
  # are still out of balance rather than the ones that always were.
  worst <- vapply(split(d$smd, d$cov), max, numeric(1))
  d$cov <- factor(d$cov, levels = names(sort(worst)))
  d$stage <- factor(d$stage, levels = unique(d$stage))

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$smd, .data$cov))
  if (!is.null(threshold)) {
    check_number(threshold, "threshold", min = 0)
    p <- p + ggplot2::geom_vline(xintercept = if (absolute) threshold else c(-threshold, threshold),
                                 colour = tk$ink$weak, linewidth = tk$rule$geom,
                                 linetype = "22")
  }
  if (!absolute) {
    p <- p + ggplot2::geom_vline(xintercept = 0, colour = tk$ink$axis,
                                 linewidth = tk$rule$axis)
  }
  p <- p + if (is.null(stage)) {
    ggplot2::geom_point(colour = tk$ink$data, size = tk$geom$point)
  } else {
    list(
      ggplot2::geom_line(ggplot2::aes(group = .data$cov), colour = tk$ink$grid,
                         linewidth = tk$rule$geom),
      ggplot2::geom_point(ggplot2::aes(colour = .data$stage), size = tk$geom$point),
      scale_colour_vellum(palette)
    )
  }
  p +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::labs(x = if (absolute) "Absolute standardised mean difference"
                  else "Standardised mean difference",
                  y = NULL, colour = NULL,
                  caption = if (!is.null(threshold))
                    sprintf("Reference at %s is a convention, not a test.", format(threshold))
                  else NULL) +
    theme_vellum(venue = venue, grid = "none")
}

#' Event-study plot
#'
#' @description
#' Coefficients on leads and lags of a treatment, for difference-in-differences
#' and related designs. The lags are the effect; the *leads* are the assumption,
#' and they are the reason this figure exists. Pre-treatment coefficients
#' indistinguishable from zero are the visible form of parallel trends, and a
#' pre-trend is the finding that invalidates the design.
#'
#' A reference period is required rather than defaulted. Every coefficient is
#' measured against some omitted period, and a plot that does not say which is
#' uninterpretable: the same data with a different reference produces a different
#' picture and both are correct.
#'
#' @param data A data frame, one row per period.
#' @param period Column of event time, negative before treatment.
#' @param estimate,lower,upper Column names for the coefficient and its interval.
#' @param reference The omitted period, drawn as a pinned zero.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   k = -4:5,
#'   est = c(0.01, -0.02, 0.03, 0, 0.12, 0.21, 0.28, 0.31, 0.3, 0.33),
#'   lo = c(-0.09, -0.12, -0.07, 0, 0.02, 0.11, 0.17, 0.19, 0.17, 0.18),
#'   hi = c(0.11, 0.08, 0.13, 0, 0.22, 0.31, 0.39, 0.43, 0.43, 0.48)
#' )
#' fig_event_study(d, "k", "est", "lo", "hi", reference = -1)
fig_event_study <- function(data, period = "period", estimate = "estimate",
                            lower = "lower", upper = "upper", reference,
                            venue = "nature", palette = "okabe_ito") {
  if (missing(reference)) {
    cli::cli_abort(c(
      "{.arg reference} is required.",
      i = "Every coefficient is measured against an omitted period; a plot that does not name it cannot be read.",
      i = "The same data with a different reference gives a different picture, and both are correct."
    ))
  }
  check_number(reference, "reference")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(period, estimate, lower, upper), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(k = as.numeric(data[[period]]),
                  est = as.numeric(data[[estimate]]),
                  lo = as.numeric(data[[lower]]),
                  hi = as.numeric(data[[upper]]), stringsAsFactors = FALSE)
  check_numeric(d$k, period)
  if (!reference %in% d$k) {
    cli::cli_abort("{.arg reference} period {.val {reference}} is not in {.val {period}}.")
  }
  if (!any(d$k < reference)) {
    cli::cli_abort(c(
      "There are no pre-treatment periods.",
      i = "The leads are what makes this an event study; without them it is a plot of the effect only."
    ))
  }
  d$phase <- ifelse(d$k < reference, "Pre-treatment", "Post-treatment")
  d$phase <- factor(d$phase, levels = c("Pre-treatment", "Post-treatment"))

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(d, ggplot2::aes(.data$k, .data$est)) +
    ggplot2::geom_hline(yintercept = 0, colour = tk$ink$axis, linewidth = tk$rule$axis) +
    ggplot2::geom_vline(xintercept = reference, colour = tk$ink$weak,
                        linewidth = tk$rule$geom, linetype = "22") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lo, ymax = .data$hi,
                                         colour = .data$phase),
                            linewidth = tk$geom$interval) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$phase), size = tk$geom$point) +
    scale_colour_vellum(palette) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$k))) +
    ggplot2::labs(x = "Periods from treatment", y = "Estimate", colour = NULL,
                  caption = sprintf("Period %s omitted as reference.", format(reference))) +
    theme_vellum(venue = venue, grid = "none")
}

#' Regression discontinuity plot
#'
#' @description
#' The outcome against the running variable, with the fits estimated separately
#' on each side of the cutoff. The whole identifying claim is the jump at the
#' threshold, so the two fits must never be joined across it — a single smooth
#' through the cutoff draws away the discontinuity it exists to show.
#'
#' Binned means rather than raw points by default: at realistic sample sizes the
#' cloud hides the jump, and the binning is the standard presentation. The bins
#' are the analyst's choice and change the picture, so the number is reported on
#' the figure.
#'
#' @param data A data frame.
#' @param x,y Column names for the running variable and the outcome.
#' @param cutoff Threshold value.
#' @param bins Number of bins each side, or `NULL` to plot raw observations.
#' @param bandwidth Optional window either side of the cutoff to restrict to.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(12)
#' d <- data.frame(score = stats::runif(600, 0, 100))
#' d$outcome <- 20 + 0.35 * d$score + 12 * (d$score >= 60) + stats::rnorm(600, 0, 7)
#' fig_rdd(d, "score", "outcome", cutoff = 60)
fig_rdd <- function(data, x, y, cutoff, bins = 20, bandwidth = NULL,
                    venue = "nature", palette = "okabe_ito") {
  check_number(cutoff, "cutoff")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(x, y), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(x = as.numeric(data[[x]]), y = as.numeric(data[[y]]),
                  stringsAsFactors = FALSE)
  check_numeric(d$x, x)
  check_numeric(d$y, y)
  d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
  if (!is.null(bandwidth)) {
    check_number(bandwidth, "bandwidth", min = 0)
    d <- d[abs(d$x - cutoff) <= bandwidth, , drop = FALSE]
  }
  d$side <- ifelse(d$x < cutoff, "below", "at or above")
  if (!all(c("below", "at or above") %in% d$side)) {
    cli::cli_abort(c(
      "All observations fall on one side of the cutoff.",
      i = "A discontinuity needs support either side of it."
    ))
  }
  d$side <- factor(d$side, levels = c("below", "at or above"))

  tk <- vellum_tokens(venue)
  points <- if (is.null(bins)) {
    d
  } else {
    check_number(bins, "bins", min = 2)
    do.call(rbind, lapply(levels(d$side), function(s) {
      g <- d[d$side == s, , drop = FALSE]
      br <- seq(min(g$x), max(g$x), length.out = bins + 1L)
      cut_id <- cut(g$x, br, include.lowest = TRUE)
      data.frame(x = vapply(split(g$x, cut_id), mean, numeric(1)),
                 y = vapply(split(g$y, cut_id), mean, numeric(1)),
                 side = s, stringsAsFactors = FALSE)
    }))
  }
  points <- points[is.finite(points$x) & is.finite(points$y), , drop = FALSE]
  points$side <- factor(points$side, levels = levels(d$side))

  ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_vline(xintercept = cutoff, colour = tk$ink$text,
                        linewidth = tk$rule$geom, linetype = "22") +
    ggplot2::geom_point(data = points, ggplot2::aes(colour = .data$side),
                        size = tk$geom$point, alpha = 0.85) +
    # `se = FALSE` and one fit per side: a band across the cutoff would join what
    # the design says must stay apart.
    ggplot2::geom_smooth(ggplot2::aes(colour = .data$side, group = .data$side),
                         method = "lm", formula = y ~ x, se = FALSE,
                         linewidth = tk$rule$emphasis) +
    scale_colour_vellum(palette) +
    ggplot2::labs(x = x, y = y, colour = NULL,
                  caption = if (is.null(bins)) NULL
                  else sprintf("Binned means, %d bins each side.", bins)) +
    theme_vellum(venue = venue, grid = "none")
}

#' Lexis diagram
#'
#' @description
#' Age against calendar time, in which each person's life is a line of slope one
#' and a cohort is a diagonal band. It is the figure that makes the
#' age-period-cohort identity visible: age, period and cohort are three
#' coordinates on this plane and any two determine the third, which is why no
#' model can separate their linear effects and why the diagram is worth drawing
#' before attempting one.
#'
#' @param data Optional data frame of individual lifelines with entry and exit.
#' @param entry_time,entry_age,exit_time,exit_age Column names for each
#'   lifeline's start and end.
#' @param event Optional logical column marking a terminal event, drawn as a
#'   point at the end of the line.
#' @param period_range,age_range Axis limits.
#' @param cohorts Cohort diagonals to draw, as birth years.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(13)
#' d <- data.frame(entry_time = stats::runif(24, 2000, 2016))
#' d$entry_age <- stats::runif(24, 20, 60)
#' d$span <- stats::runif(24, 1, 9)
#' d$exit_time <- d$entry_time + d$span
#' d$exit_age <- d$entry_age + d$span
#' d$died <- stats::rbinom(24, 1, 0.35) == 1
#' fig_lexis(d, event = "died", period_range = c(2000, 2026),
#'           age_range = c(15, 75), cohorts = seq(1940, 2000, by = 20))
fig_lexis <- function(data = NULL, entry_time = "entry_time", entry_age = "entry_age",
                      exit_time = "exit_time", exit_age = "exit_age", event = NULL,
                      period_range, age_range, cohorts = NULL,
                      venue = "nature", palette = "okabe_ito") {
  if (missing(period_range) || missing(age_range)) {
    cli::cli_abort("{.arg period_range} and {.arg age_range} are required.")
  }
  check_numeric(period_range, "period_range")
  check_numeric(age_range, "age_range")
  tk <- vellum_tokens(venue)

  p <- ggplot2::ggplot()
  if (!is.null(cohorts)) {
    check_numeric(cohorts, "cohorts")
    # A cohort is the diagonal age = period - birth year. Drawn first, because
    # they are the reference grid the lifelines are read against.
    coh <- data.frame(birth = cohorts)
    p <- p + ggplot2::geom_abline(data = coh,
                                  ggplot2::aes(slope = 1, intercept = -.data$birth),
                                  colour = tk$ink$grid, linewidth = tk$rule$grid)
  }
  if (!is.null(data)) {
    if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
    missing_cols <- setdiff(c(entry_time, entry_age, exit_time, exit_age, event),
                            names(data))
    if (length(missing_cols)) {
      cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
    }
    d <- data.frame(t0 = as.numeric(data[[entry_time]]),
                    a0 = as.numeric(data[[entry_age]]),
                    t1 = as.numeric(data[[exit_time]]),
                    a1 = as.numeric(data[[exit_age]]), stringsAsFactors = FALSE)
    # Time and age advance together, always. A lifeline of any other slope is a
    # data error, not a modelling choice.
    slope_err <- abs((d$a1 - d$a0) - (d$t1 - d$t0))
    if (any(slope_err > 1e-6, na.rm = TRUE)) {
      cli::cli_abort(c(
        "{sum(slope_err > 1e-6, na.rm = TRUE)} lifeline{?s} {?does/do} not advance in age and time together.",
        i = "On a Lexis diagram every life has slope exactly 1: a year lived is a year older.",
        i = "Check that entry and exit ages match their times."
      ))
    }
    d$event <- if (is.null(event)) FALSE else as.logical(data[[event]])
    cols <- vellum_pal(palette, 2)
    p <- p +
      ggplot2::geom_segment(data = d,
                            ggplot2::aes(x = .data$t0, y = .data$a0,
                                         xend = .data$t1, yend = .data$a1),
                            colour = cols[1], linewidth = tk$rule$geom) +
      ggplot2::geom_point(data = d[d$event, , drop = FALSE],
                          ggplot2::aes(.data$t1, .data$a1),
                          colour = cols[2], size = tk$geom$point_small)
  }
  p +
    ggplot2::coord_cartesian(xlim = period_range, ylim = age_range, expand = FALSE) +
    ggplot2::labs(x = "Calendar time", y = "Age") +
    theme_vellum(venue = venue, grid = "none", axis_style = "frame")
}

#' Epidemic curve
#'
#' @description
#' Cases by date of onset, as stacked bars one case deep. The unit is the case,
#' not the height: an epidemic curve is a histogram of onsets and its bin width
#' is an analytic choice that changes the shape, so the interval is stated on the
#' figure rather than left in the code.
#'
#' Date of onset, not date of report. Reporting dates carry the shape of the
#' surveillance system — weekday troughs, batch uploads — and an epidemic curve
#' drawn on them describes the laboratory rather than the outbreak.
#'
#' @param data A data frame of aggregated counts.
#' @param date,count Column names for the onset interval and the number of cases.
#' @param group Optional column mapped to fill, for classification or setting.
#' @param interval Description of the bin width, printed on the figure.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(14)
#' d <- expand.grid(day = 1:40, status = c("Confirmed", "Probable"))
#' d$cases <- round(c(stats::dnorm(1:40, 22, 7) * 320,
#'                    stats::dnorm(1:40, 19, 8) * 130))
#' fig_epicurve(d, "day", "cases", group = "status", interval = "1 day")
fig_epicurve <- function(data, date = "date", count = "count", group = NULL,
                         interval = "1 day", venue = "nature",
                         palette = "okabe_ito") {
  check_string(interval, "interval")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(date, count, group), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  n <- as.numeric(data[[count]])
  check_numeric(n, count)
  if (any(n < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Case counts cannot be negative.",
      i = "A retraction is a correction to an earlier interval, not a negative bar in this one."
    ))
  }
  d <- data.frame(x = data[[date]], n = n, stringsAsFactors = FALSE)
  d$group <- if (is.null(group)) "" else as.character(data[[group]])
  d$group <- factor(d$group, levels = unique(d$group))

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$n))
  p <- p + if (is.null(group)) {
    ggplot2::geom_col(fill = tk$ink$data, width = 0.92)
  } else {
    list(ggplot2::geom_col(ggplot2::aes(fill = .data$group), width = 0.92,
                           colour = tk$ink$paper, linewidth = tk$rule$hairline),
         scale_fill_vellum(palette))
  }
  p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Date of onset", y = "Cases", fill = NULL,
                  caption = sprintf("One bar per %s, by date of onset.", interval)) +
    theme_vellum(venue = venue, grid = "y")
}
