# Evidence-synthesis recipes that show the structure of a body of evidence
# rather than its pooled result.

#' GOSH plot
#'
#' @description
#' Every subset of a meta-analysis, plotted as its pooled estimate against its
#' heterogeneity. Where a forest plot shows one answer and an influence analysis
#' shows one study at a time, this shows the whole combinatorial space at once,
#' and its shape is the finding: a single cloud means the evidence is coherent,
#' and two or more separated clusters mean the studies are estimating different
#' things and the pooled estimate lies between them, describing neither.
#'
#' Colour by a study's membership to identify which one splits the cloud. That is
#' what turns the picture into an action -- not "there is heterogeneity", which
#' `I²` already said, but "it is this trial, and here is the estimate with and
#' without it".
#'
#' This draws prepared subset results; it does not enumerate them. With more than
#' about twenty studies the full subset space is too large to compute exhaustively
#' and a random sample of subsets is the usual substitute -- say which you used.
#'
#' @param data A data frame, one row per subset.
#' @param estimate,heterogeneity Column names giving the subset's pooled estimate
#'   and its heterogeneity statistic.
#' @param colour Optional column mapped to point colour, usually whether a
#'   particular study is in the subset.
#' @param k Optional column giving the number of studies in each subset, mapped
#'   to point size.
#' @param reference Optional vertical reference, usually the estimate from all
#'   studies.
#' @param log_scale Draw the estimate axis on a log scale, for ratio measures.
#' @param estimate_label,heterogeneity_label Axis labels.
#' @param venue Target venue.
#' @param palette Palette name.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' set.seed(5)
#' n <- 600
#' outlier <- stats::rbinom(n, 1, 0.4) == 1
#' d <- data.frame(
#'   estimate = ifelse(outlier, stats::rnorm(n, 0.82, 0.05),
#'                     stats::rnorm(n, 0.62, 0.04)),
#'   i2 = ifelse(outlier, stats::runif(n, 55, 88), stats::runif(n, 2, 30)),
#'   has_trial_7 = ifelse(outlier, "Included", "Excluded")
#' )
#' fig_gosh(d, estimate = "estimate", heterogeneity = "i2",
#'          colour = "has_trial_7", reference = 0.7)
fig_gosh <- function(data, estimate = "estimate", heterogeneity = "i2",
                     colour = NULL, k = NULL, reference = NULL, log_scale = FALSE,
                     estimate_label = "Pooled estimate",
                     heterogeneity_label = expression(italic(I)^2 * " (%)"),
                     venue = "nature", palette = "okabe_ito") {
  check_bool(log_scale, "log_scale")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame of subset results.")
  missing_cols <- setdiff(c(estimate, heterogeneity, colour, k), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  d <- data.frame(est = as.numeric(data[[estimate]]),
                  het = as.numeric(data[[heterogeneity]]), stringsAsFactors = FALSE)
  check_numeric(d$est, estimate)
  check_numeric(d$het, heterogeneity)
  if (any(d$het < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "{sum(d$het < 0, na.rm = TRUE)} heterogeneity value{?s} {?is/are} negative.",
      i = "{.field I^2} is truncated at zero by definition; a negative value means it was not truncated."
    ))
  }
  if (log_scale && any(d$est <= 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg log_scale} needs positive estimates; {sum(d$est <= 0, na.rm = TRUE)} {?is/are} not.")
  }
  if (!is.null(colour)) d$colour <- as.character(data[[colour]])
  if (!is.null(k)) d$k <- as.numeric(data[[k]])

  tk <- vellum_tokens(venue)
  # Alpha rather than smaller points: the density of the cloud is the signal, and
  # overplotting at full opacity hides exactly the sparse region between two
  # clusters that the reader is looking for.
  point <- if (is.null(colour)) {
    ggplot2::geom_point(colour = tk$ink$data, alpha = tk$geom$alpha_light,
                        size = tk$geom$point_small, stroke = 0)
  } else {
    ggplot2::geom_point(ggplot2::aes(colour = .data$colour),
                        alpha = tk$geom$alpha_light,
                        size = tk$geom$point_small, stroke = 0)
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$est, .data$het))
  if (!is.null(reference)) {
    check_number(reference, "reference")
    p <- p + ggplot2::geom_vline(xintercept = reference, colour = tk$ink$weak,
                                 linewidth = tk$rule$geom, linetype = "22")
  }
  p <- p + point +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::labs(x = estimate_label, y = heterogeneity_label, colour = NULL,
                  caption = sprintf("%s subsets.",
                                    format(nrow(d), big.mark = " ", trim = TRUE))) +
    theme_vellum(venue = venue, grid = "none")

  if (log_scale) p <- p + ggplot2::scale_x_log10()
  if (!is.null(colour)) {
    p <- p + scale_colour_vellum(palette) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(alpha = 1, size = tk$geom$point)))
  }
  p
}

#' Risk-of-bias matrix
#'
#' @description
#' Structured judgements as a study-by-domain grid, in the traffic-light form
#' Cochrane's tools produce. Its job is to make the *pattern* visible -- a domain
#' that fails across the whole body of evidence, a study that fails everywhere --
#' which a per-study table cannot show however carefully it is read.
#'
#' Colour alone will not carry it. The colours come from Okabe-Ito rather than
#' from literal red, amber and green so that they survive deuteranopia, and every
#' cell also carries the symbol from [judgement_scale()], so the grid stays
#' readable in greyscale, in a photocopy, and to a reader who sees no colour at
#' all. The symbols are not decoration and are not optional.
#'
#' A judgement outside the known levels is refused rather than dropped, because a
#' silently missing cell reads as "not assessed" and that is a different claim.
#'
#' @param data A data frame in long form, one row per study-domain judgement.
#' @param study,domain,judgement Column names.
#' @param levels Judgement levels, in the order they should appear in the legend.
#'   Must be a subset of the names in [judgement_scale()].
#' @param symbols Draw the symbol in each cell.
#' @param summary Append a row giving each domain's distribution across studies.
#' @param venue Target venue.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' d <- expand.grid(
#'   study = paste("Trial", 1:6),
#'   domain = c("Randomisation", "Deviations", "Missing data",
#'              "Measurement", "Reporting")
#' )
#' set.seed(6)
#' d$judgement <- sample(c("low", "some", "high"), nrow(d), TRUE,
#'                       prob = c(0.5, 0.3, 0.2))
#' fig_rob_matrix(d)
fig_rob_matrix <- function(data, study = "study", domain = "domain",
                           judgement = "judgement",
                           levels = c("low", "some", "high", "unclear"),
                           symbols = TRUE, summary = FALSE, venue = "nature") {
  check_bool(symbols, "symbols")
  check_bool(summary, "summary")
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame in long form.")
  missing_cols <- setdiff(c(study, domain, judgement), names(data))
  if (length(missing_cols)) {
    cli::cli_abort("{.arg data} has no column{?s} {.val {missing_cols}}.")
  }

  scale_def <- judgement_scale()
  unknown_levels <- setdiff(levels, names(scale_def$colours))
  if (length(unknown_levels)) {
    cli::cli_abort(c(
      "{.arg levels} names judgement{?s} with no colour: {.val {unknown_levels}}.",
      i = "Available: {.val {names(scale_def$colours)}}. See {.fn judgement_scale}."
    ))
  }

  j <- as.character(data[[judgement]])
  unknown <- setdiff(unique(j), levels)
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unrecognised judgement{?s}: {.val {unknown}}.",
      i = "Every cell must carry a judgement the legend explains; an unlabelled cell reads as 'not assessed'.",
      i = "Add them to {.arg levels}, or recode them to {.val {levels}}."
    ))
  }

  d <- data.frame(
    study = factor(as.character(data[[study]]),
                   levels = rev(unique(as.character(data[[study]])))),
    domain = factor(as.character(data[[domain]]),
                    levels = unique(as.character(data[[domain]]))),
    j = factor(j, levels = levels),
    stringsAsFactors = FALSE
  )
  dup <- duplicated(d[c("study", "domain")])
  if (any(dup)) {
    cli::cli_abort("{sum(dup)} study-domain pair{?s} {?is/are} judged more than once.")
  }

  tk <- vellum_tokens(venue)
  d$symbol <- scale_def$symbols[as.character(d$j)]

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$domain, .data$study)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$j), colour = tk$ink$paper,
                       linewidth = tk$rule$hairline * 3, width = 0.94, height = 0.94) +
    ggplot2::scale_fill_manual(values = scale_def$colours[levels], drop = FALSE,
                               labels = sprintf("%s  %s", scale_def$symbols[levels], levels)) +
    ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    theme_vellum(venue = venue, grid = "none", axis_style = "none", ticks = "none") +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 40, hjust = 0, vjust = 0),
      legend.position = "bottom")

  if (symbols) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = .data$symbol), size = fs(tk$type$size$annot),
      colour = "#000000", family = tk$type$family)
  }
  if (summary) {
    # The summary is a proportion per domain, drawn under the matrix on the same
    # x scale so the columns line up and the eye can travel down one domain.
    shares <- as.data.frame(prop.table(table(d$domain, d$j), margin = 1),
                            stringsAsFactors = FALSE)
    names(shares) <- c("domain", "j", "share")
    shares$domain <- factor(shares$domain, levels = base::levels(d$domain))
    shares$j <- factor(shares$j, levels = levels)
    p <- p + ggplot2::geom_col(
      data = shares, inherit.aes = FALSE,
      ggplot2::aes(x = .data$domain, y = .data$share * 0.8 - 0.9,
                   fill = .data$j),
      position = "stack", width = 0.94)
  }
  p
}
