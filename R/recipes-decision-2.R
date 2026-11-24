# Decision-analytic recipes, part two: what the model does over time, which
# strategies survive the comparison, and where the remaining uncertainty is.

#' Markov trace
#'
#' @description
#' State occupancy through model time: the proportion of the cohort in each
#' health state at each cycle. It is the figure that shows a model behaving, and
#' the one that catches the errors a cost-effectiveness ratio cannot — a cohort
#' that leaks, an absorbing state that lets people out, a tunnel state that
#' fills and never empties.
#'
#' Occupancy must sum to one at every cycle. A trace that does not is describing
#' a cohort that gains or loses members between cycles, and every downstream cost
#' and QALY is wrong by that amount. It is checked here because it is invisible
#' in the plot: bands that do not fill the panel look like a design choice.
#'
#' @param data A data frame in long form.
#' @param cycle,state,proportion Column names.
#' @param check Verify that occupancy sums to one in every cycle.
#' @param tolerance Absolute tolerance for that check.
#' @param absorbing Optional names of absorbing states, ordered last so the
#'   accumulating band sits at the top where its growth is readable.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' cycles <- 0:40
#' well <- 0.98^cycles
#' dead <- 1 - 0.995^cycles
#' sick <- pmax(0, 1 - well - dead)
#' d <- data.frame(
#'   cycle = rep(cycles, 3),
#'   state = rep(c("Well", "Sick", "Dead"), each = length(cycles)),
#'   p = c(well, sick, dead)
#' )
#' fig_markov_trace(d, "cycle", "state", "p", absorbing = "Dead")
fig_markov_trace <- function(data, cycle = "cycle", state = "state",
                             proportion = "proportion", check = TRUE,
                             tolerance = 1e-6, absorbing = NULL,
                             venue = "nature", palette = "okabe_ito") {
  check_bool(check, "check")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(cycle, state, proportion), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(cycle = as.numeric(data[[cycle]]),
                  state = as.character(data[[state]]),
                  p = as.numeric(data[[proportion]]), stringsAsFactors = FALSE)
  check_numeric(d$cycle, cycle)
  check_numeric(d$p, proportion)
  if (any(d$p < 0 | d$p > 1, na.rm = TRUE)) {
    cli::cli_abort(c(
      "State occupancy must lie in [0, 1].",
      i = "A trace holds proportions of the cohort, not counts."
    ))
  }

  if (check) {
    sums <- vapply(split(d$p, d$cycle), sum, numeric(1))
    bad <- names(sums)[abs(sums - 1) > tolerance]
    if (length(bad)) {
      cli::cli_abort(c(
        "Occupancy does not sum to 1 in {length(bad)} cycle{?s}.",
        x = sprintf("Cycle %s sums to %.6f.", bad[1], sums[bad[1]]),
        i = "The cohort is closed: everybody is somewhere at every cycle.",
        i = "A leak here makes every cost and QALY downstream of it wrong.",
        i = "Set {.code check = FALSE} to draw it anyway."
      ))
    }
  }

  # Absorbing states last, so the band that only grows sits at the top of the
  # stack where its accumulation is read against a straight edge.
  lv <- unique(d$state)
  if (!is.null(absorbing)) {
    unknown <- setdiff(absorbing, lv)
    if (length(unknown)) {
      cli::cli_abort("{.arg absorbing} names state{?s} not present: {.val {unknown}}.")
    }
    lv <- c(setdiff(lv, absorbing), absorbing)
  }
  d$state <- factor(d$state, levels = lv)

  tk <- vellum_tokens(venue)
  ggplot2::ggplot(d, ggplot2::aes(.data$cycle, .data$p, fill = .data$state)) +
    ggplot2::geom_area(colour = tk$ink$paper, linewidth = tk$rule$hairline * 2) +
    scale_fill_vellum(palette) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = function(z) sprintf("%.0f%%", 100 * z),
                                expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = "Cycle", y = "Cohort in state", fill = NULL) +
    theme_vellum(venue = venue, grid = "none")
}

#' Cost-effectiveness efficiency frontier
#'
#' @description
#' All strategies in cost-effect space, with the frontier joining those that
#' survive comparison. Everything off the frontier is excluded, and the two ways
#' of being excluded are different: a *dominated* strategy costs more and does
#' less than some single alternative; an *extendedly dominated* one is beaten by
#' a mixture of two others and is the case people miss.
#'
#' The exclusions are computed here rather than taken on trust, because
#' incremental cost-effectiveness ratios calculated against the wrong comparator
#' are the commonest error in an economic evaluation and they look entirely
#' plausible on the page. ICERs are reported along the frontier only — a ratio
#' against a dominated comparator has no interpretation.
#'
#' @param data A data frame, one row per strategy.
#' @param cost,effect,strategy Column names.
#' @param wtp Optional willingness-to-pay threshold, drawn as a slope.
#' @param label_icer Print the incremental ratio on each frontier segment.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object, with the classification in the `vellum_frontier`
#'   attribute.
#' @export
#' @examples
#' d <- data.frame(
#'   strategy = c("Usual care", "Screening", "Screen + treat", "Intensive",
#'                "Add-on"),
#'   cost = c(1200, 3400, 6100, 9800, 5200),
#'   effect = c(4.1, 4.9, 5.6, 5.9, 4.8)
#' )
#' fig_frontier(d, "cost", "effect", "strategy", wtp = 20000)
fig_frontier <- function(data, cost = "cost", effect = "effect",
                         strategy = "strategy", wtp = NULL, label_icer = TRUE,
                         venue = "nature", palette = "okabe_ito") {
  check_bool(label_icer, "label_icer")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(cost, effect, strategy), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(strategy = as.character(data[[strategy]]),
                  cost = as.numeric(data[[cost]]),
                  effect = as.numeric(data[[effect]]), stringsAsFactors = FALSE)
  check_numeric(d$cost, cost)
  check_numeric(d$effect, effect)
  if (nrow(d) < 2L) cli::cli_abort("A frontier needs at least two strategies.")

  d <- d[order(d$effect, d$cost), , drop = FALSE]
  # Simple dominance: something cheaper and at least as effective exists.
  d$dominated <- vapply(seq_len(nrow(d)), function(i)
    any(d$cost < d$cost[i] & d$effect >= d$effect[i]), logical(1))

  # Extended dominance: walk the remaining strategies in effect order and drop
  # any whose incremental ratio is higher than the one after it, repeating until
  # the ratios increase monotonically. That monotone sequence IS the frontier.
  keep <- which(!d$dominated)
  repeat {
    if (length(keep) < 3L) break
    icer <- diff(d$cost[keep]) / diff(d$effect[keep])
    bad <- which(diff(icer) < 0)
    if (!length(bad)) break
    keep <- keep[-(bad[1] + 1L)]
  }
  d$status <- ifelse(seq_len(nrow(d)) %in% keep, "On frontier",
                     ifelse(d$dominated, "Dominated", "Extendedly dominated"))
  d$status <- factor(d$status,
                     levels = c("On frontier", "Dominated", "Extendedly dominated"))

  front <- d[keep, , drop = FALSE]
  seg <- if (nrow(front) > 1L) {
    data.frame(
      x = utils::head(front$effect, -1), y = utils::head(front$cost, -1),
      xend = front$effect[-1], yend = front$cost[-1],
      icer = diff(front$cost) / diff(front$effect), stringsAsFactors = FALSE)
  } else NULL

  tk <- vellum_tokens(venue)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$effect, .data$cost))
  if (!is.null(wtp)) {
    check_number(wtp, "wtp", min = 0)
    ref <- front[1, , drop = FALSE]
    p <- p + ggplot2::geom_abline(slope = wtp,
                                  intercept = ref$cost - wtp * ref$effect,
                                  colour = tk$ink$weak, linewidth = tk$rule$geom,
                                  linetype = "22")
  }
  if (!is.null(seg)) {
    p <- p + ggplot2::geom_segment(data = seg,
                                   ggplot2::aes(x = .data$x, y = .data$y,
                                                xend = .data$xend, yend = .data$yend),
                                   colour = tk$ink$text, linewidth = tk$rule$emphasis)
  }
  p <- p +
    ggplot2::geom_point(ggplot2::aes(colour = .data$status, shape = .data$status),
                        size = tk$geom$point_large) +
    ggplot2::geom_text(ggplot2::aes(label = .data$strategy), vjust = -1.1,
                       size = fs(tk$type$size$small), colour = tk$ink$text,
                       family = tk$type$family) +
    scale_colour_vellum(palette) +
    ggplot2::scale_shape_manual(values = c(`On frontier` = 19, Dominated = 4,
                                           `Extendedly dominated` = 1), drop = FALSE)
  if (label_icer && !is.null(seg)) {
    p <- p + ggplot2::geom_text(
      data = seg, ggplot2::aes(x = (.data$x + .data$xend) / 2,
                               y = (.data$y + .data$yend) / 2,
                               label = format(round(.data$icer), big.mark = " ", trim = TRUE)),
      hjust = 1.2, size = fs(tk$type$size$small), colour = tk$ink$weak,
      family = tk$type$family, inherit.aes = FALSE)
  }
  structure(
    p +
      ggplot2::scale_y_continuous(labels = function(z) format(z, big.mark = " ", trim = TRUE),
                                  expand = ggplot2::expansion(mult = 0.09)) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.09)) +
      ggplot2::labs(x = "Effect", y = "Cost", colour = NULL, shape = NULL) +
      theme_vellum(venue = venue, grid = "none"),
    vellum_frontier = d[, c("strategy", "cost", "effect", "status")]
  )
}

#' Expected value of partial perfect information
#'
#' @description
#' The value of resolving each parameter or group of parameters, at a stated
#' threshold. It is the figure that turns "the model is uncertain" into a
#' research agenda: a parameter with high EVPPI is worth measuring, one with
#' near-zero EVPPI is not worth another study however uncertain it is.
#'
#' Two properties are checked because both are violated routinely and neither is
#' visible once drawn. EVPPI cannot be negative — perfect information about a
#' subset cannot make a decision worse in expectation. And no subset can be worth
#' more than the whole, so every EVPPI must sit at or below the overall EVPI; a
#' bar above the line means the two were computed from different runs or
#' different thresholds.
#'
#' @param data A data frame, one row per parameter or group.
#' @param parameter,value Column names.
#' @param evpi Overall EVPI at the same threshold, drawn as a ceiling.
#' @param population Number of people the decision applies to.
#' @param check Verify non-negativity and the EVPI ceiling.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- data.frame(
#'   parameter = c("Treatment effect", "Baseline risk", "Utility, on treatment",
#'                 "Cost of care", "Discontinuation", "Utility, off treatment"),
#'   evppi = c(310, 184, 96, 41, 22, 6)
#' )
#' fig_evppi(d, "parameter", "evppi", evpi = 402, population = 30000)
fig_evppi <- function(data, parameter = "parameter", value = "value",
                      evpi = NULL, population = NULL, check = TRUE,
                      venue = "nature", palette = "okabe_ito") {
  check_bool(check, "check")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  missing_cols <- setdiff(c(parameter, value), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }
  d <- data.frame(parameter = as.character(data[[parameter]]),
                  v = as.numeric(data[[value]]), stringsAsFactors = FALSE)
  check_numeric(d$v, value)

  if (check) {
    if (any(d$v < 0, na.rm = TRUE)) {
      cli::cli_abort(c(
        "{sum(d$v < 0, na.rm = TRUE)} value{?s} {?is/are} negative.",
        i = "Perfect information about a subset of parameters cannot make a decision worse in expectation."
      ))
    }
    if (!is.null(evpi)) {
      check_number(evpi, "evpi", min = 0)
      over <- d$parameter[d$v > evpi * (1 + 1e-8)]
      if (length(over)) {
        cli::cli_abort(c(
          "{length(over)} parameter{?s} {?has/have} EVPPI above the overall EVPI: {.val {over}}.",
          i = "No subset of the uncertainty can be worth more than all of it.",
          i = "This usually means the two were computed at different thresholds or from different runs."
        ))
      }
    }
  }

  scale_by <- population %||% 1
  if (!is.null(population)) check_number(population, "population", min = 0)
  d$v <- d$v * scale_by
  d <- d[order(d$v), , drop = FALSE]
  d$parameter <- factor(d$parameter, levels = d$parameter)

  tk <- vellum_tokens(venue)
  cols <- vellum_pal(palette, 1)
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$v, .data$parameter)) +
    ggplot2::geom_col(fill = cols[1], width = 0.7)
  if (!is.null(evpi)) {
    p <- p + ggplot2::geom_vline(xintercept = evpi * scale_by, colour = tk$ink$text,
                                 linewidth = tk$rule$geom, linetype = "22")
  }
  p +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      labels = function(z) format(z, big.mark = " ", trim = TRUE, scientific = FALSE)) +
    ggplot2::labs(x = "Expected value of perfect information", y = NULL,
                  caption = if (!is.null(evpi))
                    sprintf("Dashed line: overall EVPI%s.",
                            if (is.null(population)) " (per person)" else
                              sprintf(", population of %s",
                                      format(population, big.mark = " ", trim = TRUE)))
                  else NULL) +
    theme_vellum(venue = venue, grid = "none")
}
