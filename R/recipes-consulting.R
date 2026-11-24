# Consultancy-grade analytical recipes --------------------------------------

recipe_theme <- function(venue, profile) {
  theme_vellum_profile(profile = profile, venue = venue)
}

#' Kaplan-Meier survival curve from prepared estimates
#'
#' @param data A data frame with time and survival probability columns.
#' @param time,survival,group Column names as strings.
#' @param lower,upper Optional confidence-limit columns.
#' @param censored Optional logical censoring-indicator column.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_survival <- function(data, time, survival, group, lower = NULL, upper = NULL,
                         censored = NULL, venue = "nature", profile = "clinical") {
  d <- data.frame(.time = data[[time]], .survival = data[[survival]],
                  .group = factor(data[[group]]))
  if (!is.null(lower) && !is.null(upper)) {
    d$.lower <- data[[lower]]
    d$.upper <- data[[upper]]
  }
  if (!is.null(censored)) d$.censored <- as.logical(data[[censored]])
  d <- d[stats::complete.cases(d[c(".time", ".survival", ".group")]), , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.time, .data$.survival,
                                       colour = .data$.group,
                                       linetype = .data$.group))
  if (all(c(".lower", ".upper") %in% names(d))) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper,
                   fill = .data$.group),
      alpha = 0.10, colour = NA, show.legend = FALSE
    )
  }
  p <- p +
    ggplot2::geom_step(linewidth = lw(0.8)) +
    scale_colour_vellum() + scale_fill_vellum() +
    scale_linetype_vellum() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(),
                                limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(x = time, y = "Survival probability",
                  colour = group, linetype = group) +
    recipe_theme(venue, profile)
  if (".censored" %in% names(d) && any(d$.censored, na.rm = TRUE)) {
    p <- p + ggplot2::geom_point(
      data = d[d$.censored %in% TRUE, , drop = FALSE],
      shape = 3, size = ps(1.3), stroke = lw(0.55), show.legend = FALSE
    )
  }
  p
}

#' Receiver-operating-characteristic curves
#'
#' @param data A data frame of prepared false-positive and true-positive rates.
#' @param fpr,tpr,model Column names as strings.
#' @param auc Optional named numeric vector or column name containing AUC values.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_roc <- function(data, fpr, tpr, model, auc = NULL,
                    venue = "nature", profile = "clinical") {
  d <- data.frame(.fpr = data[[fpr]], .tpr = data[[tpr]],
                  .model = factor(data[[model]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.fpr, .data$.tpr,
                                       colour = .data$.model,
                                       linetype = .data$.model)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#767676",
                         linewidth = lw(0.4), linetype = 3) +
    ggplot2::geom_path(linewidth = lw(0.85)) +
    scale_colour_vellum() + scale_linetype_vellum() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = "False-positive rate", y = "True-positive rate",
                  colour = model, linetype = model) +
    recipe_theme(venue, profile)
  if (!is.null(auc)) {
    vals <- if (is.character(auc) && length(auc) == 1L) {
      stats::aggregate(data[[auc]], list(data[[model]]), function(z) z[1])
    } else {
      data.frame(Group.1 = names(auc) %||% levels(d$.model), x = as.numeric(auc))
    }
    p <- p + ggplot2::labs(
      caption = paste(paste0(vals[[1]], " AUC ", formatC(vals[[2]], digits = 3,
                                                         format = "f")),
                      collapse = " \u00b7 ")
    )
  }
  p
}

#' Calibration plot with uncertainty
#'
#' @param data A data frame of prepared predicted and observed risks.
#' @param predicted,observed Column names as strings.
#' @param lower,upper Optional confidence-limit columns.
#' @param group Optional model/group column.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_calibration <- function(data, predicted, observed, lower = NULL, upper = NULL,
                            group = NULL, venue = "nature", profile = "clinical") {
  d <- data.frame(.predicted = data[[predicted]], .observed = data[[observed]],
                  .group = factor(if (is.null(group)) "Model" else data[[group]]))
  if (!is.null(lower) && !is.null(upper)) {
    d$.lower <- data[[lower]]
    d$.upper <- data[[upper]]
  }
  d <- d[stats::complete.cases(d[c(".predicted", ".observed", ".group")]), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.predicted, .data$.observed,
                                       colour = .data$.group,
                                       group = .data$.group)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#767676",
                         linewidth = lw(0.45), linetype = 3)
  if (all(c(".lower", ".upper") %in% names(d))) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper),
      linewidth = lw(0.45), alpha = 0.75
    )
  }
  p + ggplot2::geom_line(linewidth = lw(0.75)) +
    ggplot2::geom_point(size = ps(1.5)) +
    scale_colour_vellum() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(x = "Predicted probability", y = "Observed proportion",
                  colour = if (is.null(group)) NULL else group) +
    recipe_theme(venue, profile)
}

#' Bland-Altman agreement plot
#'
#' @param data A data frame.
#' @param mean,difference Columns containing pair means and differences.
#' @param group Optional group column.
#' @param bias,lower,upper Optional pre-specified agreement statistics. Defaults
#'   to the sample mean and mean plus or minus 1.96 standard deviations.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_bland_altman <- function(data, mean, difference, group = NULL,
                             bias = NULL, lower = NULL, upper = NULL,
                             venue = "nature", profile = "clinical") {
  d <- data.frame(.mean = data[[mean]], .difference = data[[difference]],
                  .group = factor(if (is.null(group)) "Observations" else data[[group]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  bias <- bias %||% base::mean(d$.difference)
  sd_diff <- stats::sd(d$.difference)
  lower <- lower %||% bias - 1.96 * sd_diff
  upper <- upper %||% bias + 1.96 * sd_diff
  ggplot2::ggplot(d, ggplot2::aes(.data$.mean, .data$.difference,
                                  colour = .data$.group,
                                  shape = .data$.group)) +
    ggplot2::geom_hline(yintercept = c(lower, upper), linetype = 2,
                        colour = "#767676", linewidth = lw(0.45)) +
    ggplot2::geom_hline(yintercept = bias, linewidth = lw(0.75)) +
    ggplot2::geom_point(alpha = 0.65, size = ps(1.4)) +
    scale_colour_vellum() + scale_shape_vellum() +
    ggplot2::labs(x = "Mean of methods", y = "Difference between methods",
                  colour = if (is.null(group)) NULL else group,
                  shape = if (is.null(group)) NULL else group,
                  caption = paste0("Bias ", label_vellum(digits = 2)(bias),
                                   "; 95% limits of agreement ",
                                   label_vellum(digits = 2)(lower), " to ",
                                   label_vellum(digits = 2)(upper))) +
    recipe_theme(venue, profile)
}

#' Funnel plot for study estimates
#'
#' @param data A data frame.
#' @param estimate,se Effect estimate and standard-error columns.
#' @param reference Optional pooled reference estimate.
#' @param group Optional grouping column.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_funnel <- function(data, estimate, se, reference = NULL, group = NULL,
                       venue = "nature", profile = "clinical") {
  d <- data.frame(.estimate = data[[estimate]], .se = data[[se]],
                  .group = factor(if (is.null(group)) "Study" else data[[group]]))
  d <- d[stats::complete.cases(d) & d$.se > 0, , drop = FALSE]
  reference <- reference %||% stats::weighted.mean(d$.estimate, 1 / d$.se^2)
  edge <- data.frame(
    .se = seq(0, max(d$.se) * 1.05, length.out = 100)
  )
  edge$.left <- reference - 1.96 * edge$.se
  edge$.right <- reference + 1.96 * edge$.se
  ggplot2::ggplot(d, ggplot2::aes(.data$.estimate, .data$.se,
                                  colour = .data$.group,
                                  shape = .data$.group)) +
    ggplot2::geom_ribbon(data = edge,
                         ggplot2::aes(xmin = .data$.left, xmax = .data$.right,
                                      y = .data$.se),
                         inherit.aes = FALSE, orientation = "y",
                         fill = "#EDEDED", colour = NA) +
    ggplot2::geom_line(data = edge, ggplot2::aes(.data$.left, .data$.se),
                       inherit.aes = FALSE, colour = "#767676", linetype = 2) +
    ggplot2::geom_line(data = edge, ggplot2::aes(.data$.right, .data$.se),
                       inherit.aes = FALSE, colour = "#767676", linetype = 2) +
    ggplot2::geom_vline(xintercept = reference, linewidth = lw(0.55)) +
    ggplot2::geom_point(size = ps(1.5), alpha = 0.75) +
    scale_colour_vellum() + scale_shape_vellum() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = "Effect estimate", y = "Standard error",
                  colour = if (is.null(group)) NULL else group,
                  shape = if (is.null(group)) NULL else group) +
    recipe_theme(venue, profile)
}

#' Volcano plot with explicit thresholds
#'
#' @param data A data frame.
#' @param effect,p_value Effect and P-value columns.
#' @param label Optional label column.
#' @param effect_threshold,p_threshold Decision thresholds.
#' @param max_labels Maximum number of directly labelled observations.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_volcano <- function(data, effect, p_value, label = NULL,
                        effect_threshold = 1, p_threshold = 0.05,
                        max_labels = 8, venue = "nature", profile = "technical") {
  d <- data.frame(.effect = data[[effect]], .p = data[[p_value]],
                  .label = if (is.null(label)) "" else as.character(data[[label]]))
  d <- d[is.finite(d$.effect) & is.finite(d$.p) & d$.p > 0 & d$.p <= 1, , drop = FALSE]
  d$.logp <- -log10(d$.p)
  d$.class <- factor(
    ifelse(d$.p < p_threshold & d$.effect >= effect_threshold, "Higher",
           ifelse(d$.p < p_threshold & d$.effect <= -effect_threshold, "Lower",
                  "Not selected")),
    levels = c("Not selected", "Lower", "Higher")
  )
  ord <- order(d$.p, -abs(d$.effect))
  lab <- d[utils::head(ord[d$.class[ord] != "Not selected"], max_labels), ,
           drop = FALSE]
  if (nrow(lab)) {
    yr <- range(d$.logp)
    gap <- max(diff(yr) * 0.065, .12)
    lab$.label_y <- unlist(lapply(
      split(seq_len(nrow(lab)), lab$.effect >= 0),
      function(i) spread_labels(lab$.logp[i], gap, limits = yr)
    ), use.names = FALSE)
    # split() changes row order; calculate again by side without reordering.
    for (side in unique(lab$.effect >= 0)) {
      i <- which((lab$.effect >= 0) == side)
      lab$.label_y[i] <- spread_labels(lab$.logp[i], gap, limits = yr)
    }
  }
  cols <- c("Not selected" = "#A6A6A6", "Lower" = "#0072B2", "Higher" = "#D55E00")
  ggplot2::ggplot(d, ggplot2::aes(.data$.effect, .data$.logp,
                                  colour = .data$.class)) +
    ggplot2::geom_vline(xintercept = c(-effect_threshold, effect_threshold),
                        colour = "#767676", linewidth = lw(0.4), linetype = 3) +
    ggplot2::geom_hline(yintercept = -log10(p_threshold),
                        colour = "#767676", linewidth = lw(0.4), linetype = 3) +
    ggplot2::geom_point(size = ps(1.0), alpha = 0.65) +
    ggplot2::geom_segment(
      data = lab,
      ggplot2::aes(xend = .data$.effect, yend = .data$.label_y),
      colour = "#767676", linewidth = lw(0.35), show.legend = FALSE
    ) +
    ggplot2::geom_text(data = lab,
                       ggplot2::aes(y = .data$.label_y, label = .data$.label),
                       size = fs(vellum_tokens(venue)$type$size$small),
                       hjust = ifelse(lab$.effect >= 0, -0.12, 1.12),
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cols, drop = FALSE) +
    ggplot2::labs(x = effect, y = expression(-log[10](italic(P))),
                  colour = "Classification") +
    recipe_theme(venue, profile)
}

#' Longitudinal trajectories with an optional group summary
#'
#' @param data A data frame.
#' @param time,value,id Column names as strings.
#' @param group Optional group column.
#' @param show_summary Add group means through time.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_trajectory <- function(data, time, value, id, group = NULL,
                           show_summary = TRUE, venue = "nature",
                           profile = "restrained") {
  d <- data.frame(.time = data[[time]], .value = data[[value]],
                  .id = factor(data[[id]]),
                  .group = factor(if (is.null(group)) "All" else data[[group]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.time, .data$.value,
                                       group = .data$.id,
                                       colour = .data$.group)) +
    ggplot2::geom_line(alpha = 0.18, linewidth = lw(0.4)) +
    scale_colour_vellum() +
    ggplot2::labs(x = time, y = value,
                  colour = if (is.null(group)) NULL else group) +
    recipe_theme(venue, profile)
  if (show_summary) {
    p <- p + ggplot2::stat_summary(
      ggplot2::aes(group = .data$.group),
      fun = mean, geom = "line", linewidth = lw(1.1), alpha = 1
    )
  }
  p
}

#' Paired start-to-end slope graph
#'
#' @param data A data frame.
#' @param start,end Start and end value columns.
#' @param label Observation label column.
#' @param start_label,end_label Axis labels.
#' @param highlight Optional labels to emphasise.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_slope <- function(data, start, end, label, start_label = start,
                      end_label = end, highlight = NULL,
                      venue = "nature", profile = "editorial") {
  d <- data.frame(.start = data[[start]], .end = data[[end]],
                  .label = as.character(data[[label]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.focus <- factor(ifelse(d$.label %in% highlight, "Highlighted", "Context"),
                     levels = c("Context", "Highlighted"))
  cols <- c(Context = "#A6A6A6", Highlighted = "#D55E00")
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 1, xend = 2, y = .data$.start, yend = .data$.end,
                   colour = .data$.focus),
      linewidth = lw(0.7), alpha = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(1, .data$.start, colour = .data$.focus),
                        size = ps(1.4)) +
    ggplot2::geom_point(ggplot2::aes(2, .data$.end, colour = .data$.focus),
                        size = ps(1.4)) +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::scale_x_continuous(breaks = 1:2,
                                labels = c(start_label, end_label),
                                limits = c(0.85, 2.15)) +
    ggplot2::labs(x = NULL, y = "Value") +
    recipe_theme(venue, profile)
}

#' Ordered lollipop ranking
#'
#' @param data A data frame.
#' @param category,value Column names as strings.
#' @param highlight Optional category values to emphasise.
#' @param reference Optional reference line.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_lollipop <- function(data, category, value, highlight = NULL,
                         reference = NULL, venue = "nature",
                         profile = "editorial") {
  d <- data.frame(.category = as.character(data[[category]]),
                  .value = data[[value]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d <- d[order(d$.value), , drop = FALSE]
  d$.category <- factor(d$.category, levels = d$.category)
  d$.focus <- factor(ifelse(as.character(d$.category) %in% highlight,
                            "Highlighted", "Context"),
                     levels = c("Context", "Highlighted"))
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.value, .data$.category)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$.value,
                                       yend = .data$.category),
                          colour = "#DCDCDC", linewidth = lw(0.55)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$.focus),
                        size = ps(1.8)) +
    ggplot2::scale_colour_manual(
      values = c(Context = "#0072B2", Highlighted = "#D55E00"), guide = "none"
    ) +
    ggplot2::labs(x = value, y = NULL) +
    recipe_theme(venue, profile)
  if (!is.null(reference)) p <- p + annotate_null(reference)
  p
}

#' Tornado sensitivity plot
#'
#' @param data A data frame.
#' @param parameter,low,high Parameter and scenario-result columns.
#' @param baseline Reference result.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_tornado <- function(data, parameter, low, high, baseline,
                        venue = "nature", profile = "technical") {
  d <- data.frame(.parameter = as.character(data[[parameter]]),
                  .low = data[[low]], .high = data[[high]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.span <- abs(d$.high - d$.low)
  d <- d[order(d$.span), , drop = FALSE]
  d$.parameter <- factor(d$.parameter, levels = d$.parameter)
  ggplot2::ggplot(d, ggplot2::aes(y = .data$.parameter)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$.low, xend = baseline, yend = .data$.parameter),
      colour = "#0072B2", linewidth = lw(3.0), lineend = "butt"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = baseline, xend = .data$.high, yend = .data$.parameter),
      colour = "#D55E00", linewidth = lw(3.0), lineend = "butt"
    ) +
    ggplot2::geom_vline(xintercept = baseline, linewidth = lw(0.65)) +
    ggplot2::labs(x = "Outcome under one-way sensitivity analysis", y = NULL) +
    recipe_theme(venue, profile)
}

#' Correlation matrix with values and perceptual colour
#'
#' @param data A data frame or numeric matrix.
#' @param columns Optional numeric columns to include.
#' @param method Correlation method.
#' @param triangle Matrix triangle to display.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_correlation <- function(data, columns = NULL,
                            method = c("pearson", "spearman", "kendall"),
                            triangle = c("lower", "upper", "full"),
                            venue = "nature", profile = "technical") {
  method <- match.arg(method)
  triangle <- match.arg(triangle)
  x <- as.data.frame(data)
  if (!is.null(columns)) x <- x[columns]
  x <- x[vapply(x, is.numeric, logical(1))]
  if (ncol(x) < 2L) cli::cli_abort("At least two numeric columns are required.")
  mat <- stats::cor(x, use = "pairwise.complete.obs", method = method)
  d <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(d) <- c(".row", ".col", ".value")
  ii <- match(d$.row, rownames(mat))
  jj <- match(d$.col, colnames(mat))
  keep <- switch(triangle, lower = ii >= jj, upper = ii <= jj, full = rep(TRUE, nrow(d)))
  d <- d[keep, , drop = FALSE]
  d$.ink <- ifelse(abs(d$.value) >= 0.58, "white", "black")
  ggplot2::ggplot(d, ggplot2::aes(.data$.col, .data$.row, fill = .data$.value)) +
    ggplot2::geom_tile(colour = "white", linewidth = lw(0.35)) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$.value)),
                       size = fs(vellum_tokens(venue)$type$size$small),
                       colour = d$.ink) +
    scale_fill_vellum_div(midpoint = 0, symmetric = TRUE) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
    recipe_theme(venue, profile) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank())
}

#' PCA score plot with optional loading vectors
#'
#' @param scores Data frame containing prepared component scores.
#' @param x,y Score-column names.
#' @param group Optional group column.
#' @param label Optional observation-label column.
#' @param loadings Optional loading data frame.
#' @param loading_x,loading_y,loading_label Loading-column names.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_pca_biplot <- function(scores, x, y, group = NULL, label = NULL,
                           loadings = NULL, loading_x = x, loading_y = y,
                           loading_label = NULL, venue = "nature",
                           profile = "technical") {
  d <- data.frame(.x = scores[[x]], .y = scores[[y]],
                  .group = factor(if (is.null(group)) "Observations" else scores[[group]]),
                  .label = if (is.null(label)) "" else as.character(scores[[label]]))
  d <- d[stats::complete.cases(d[c(".x", ".y", ".group")]), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.x, .data$.y,
                                       colour = .data$.group,
                                       shape = .data$.group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#DCDCDC", linewidth = lw(0.35)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#DCDCDC", linewidth = lw(0.35)) +
    ggplot2::geom_point(size = ps(1.5), alpha = 0.70) +
    scale_colour_vellum() + scale_shape_vellum() +
    ggplot2::labs(x = x, y = y, colour = if (is.null(group)) NULL else group,
                  shape = if (is.null(group)) NULL else group) +
    recipe_theme(venue, profile)
  if (!is.null(loadings)) {
    scale_factor <- 0.75 * min(diff(range(d$.x)), diff(range(d$.y)))
    l <- data.frame(.x = 0, .y = 0,
                    .xend = loadings[[loading_x]] * scale_factor,
                    .yend = loadings[[loading_y]] * scale_factor,
                    .label = if (is.null(loading_label)) rownames(loadings)
                    else loadings[[loading_label]])
    xr <- range(c(d$.x, l$.xend))
    yr <- range(c(d$.y, l$.yend))
    l$.label_y <- spread_labels(l$.yend, max(diff(yr) * .08, .1), limits = yr)
    # Anchor loading labels just inside their vector endpoints. Keeping the text
    # inward prevents clipping and avoids collisions with an outside legend.
    l$.label_x <- l$.xend - ifelse(l$.xend >= 0, 1, -1) * diff(xr) * .025
    l$.hjust <- ifelse(l$.xend >= 0, 1, 0)
    p <- p +
      ggplot2::geom_segment(
        data = l, ggplot2::aes(x = .data$.x, y = .data$.y,
                               xend = .data$.xend, yend = .data$.yend),
        inherit.aes = FALSE, arrow = grid::arrow(length = grid::unit(1.2, "mm")),
        colour = "#000000", linewidth = lw(0.5)
      ) +
      ggplot2::geom_segment(
        data = l,
        ggplot2::aes(x = .data$.xend, y = .data$.yend,
                     xend = .data$.label_x, yend = .data$.label_y),
        inherit.aes = FALSE, colour = "#767676", linewidth = lw(0.3)
      ) +
      ggplot2::geom_text(
        data = l,
        ggplot2::aes(.data$.label_x, .data$.label_y, label = .data$.label),
        inherit.aes = FALSE, size = fs(vellum_tokens(venue)$type$size$small),
        colour = "#000000", hjust = l$.hjust
      ) +
      ggplot2::expand_limits(
        x = c(min(l$.label_x) - diff(xr) * .05,
              max(l$.label_x) + diff(xr) * .05),
        y = c(min(l$.label_y) - diff(yr) * .03,
              max(l$.label_y) + diff(yr) * .03)
      )
  }
  p
}

#' Likelihood-impact risk matrix
#'
#' @param data A data frame.
#' @param likelihood,impact,label Column names as strings.
#' @param group Optional grouping column.
#' @param levels Number of ordinal levels on each axis.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_risk_matrix <- function(data, likelihood, impact, label, group = NULL,
                            levels = 5, venue = "nature",
                            profile = "technical") {
  check_number(levels, "levels", min = 2, max = 10)
  d <- data.frame(.likelihood = data[[likelihood]], .impact = data[[impact]],
                  .label = data[[label]],
                  .group = factor(if (is.null(group)) "Risk" else data[[group]]))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$.label_y <- d$.impact + rep(c(-0.20, 0.20), length.out = nrow(d))
  bg <- expand.grid(.likelihood = seq_len(levels), .impact = seq_len(levels))
  bg$.risk <- bg$.likelihood * bg$.impact
  ggplot2::ggplot(bg, ggplot2::aes(.data$.likelihood, .data$.impact,
                                   fill = .data$.risk)) +
    ggplot2::geom_tile(colour = "white", linewidth = lw(0.6)) +
    scale_fill_vellum_c("ember", guide = "none") +
    ggplot2::geom_point(
      data = d, ggplot2::aes(.data$.likelihood, .data$.impact,
                             shape = .data$.group),
      inherit.aes = FALSE, fill = "white", colour = "black",
      size = ps(2.2), stroke = lw(0.6)
    ) +
    ggplot2::geom_text(
      data = d, ggplot2::aes(.data$.likelihood, .data$.label_y,
                             label = .data$.label),
      inherit.aes = FALSE, size = fs(vellum_tokens(venue)$type$size$small),
      vjust = 0.5
    ) +
    scale_shape_vellum() +
    ggplot2::scale_x_continuous(breaks = seq_len(levels),
                                limits = c(0.5, levels + 0.5)) +
    ggplot2::scale_y_continuous(breaks = seq_len(levels),
                                limits = c(0.5, levels + 0.5)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Likelihood", y = "Impact",
                  shape = if (is.null(group)) NULL else group) +
    recipe_theme(venue, profile)
}
