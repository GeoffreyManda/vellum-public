# Uncertainty grammar --------------------------------------------------------

#' Define the uncertainty represented by a figure
#'
#' @param type Interval or uncertainty type.
#' @param level Coverage probability in `(0, 1)`, or `NA` where not applicable.
#' @param scope `"pointwise"` or `"simultaneous"`.
#' @param method Estimation method.
#' @param unit Optional analysis unit.
#' @return A `vellum_interval` object.
#' @export
interval_spec <- function(type = c("confidence", "credible", "prediction",
                                    "bootstrap", "standard-error", "iqr", "range"),
                          level = 0.95,
                          scope = c("pointwise", "simultaneous"),
                          method = "not stated",
                          unit = NULL) {
  type <- match.arg(type)
  scope <- match.arg(scope)
  if (!is.na(level)) check_number(level, "level", min = .Machine$double.eps,
                                   max = 1 - .Machine$double.eps)
  check_string(method, "method")
  if (!is.null(unit)) check_string(unit, "unit")
  structure(list(type = type, level = level, scope = scope, method = method,
                 unit = unit), class = "vellum_interval")
}

#' @export
print.vellum_interval <- function(x, ...) {
  cat(interval_label(x), "\n", sep = "")
  invisible(x)
}

#' Format an uncertainty specification
#'
#' @param x A `vellum_interval`.
#' @return A concise human-readable label.
#' @export
interval_label <- function(x) {
  if (!inherits(x, "vellum_interval")) {
    cli::cli_abort("{.arg x} must be created by {.fn interval_spec}.")
  }
  level <- if (is.na(x$level)) "" else paste0(formatC(100 * x$level, format = "fg"), "% ")
  unit <- if (is.null(x$unit)) "" else paste0("; analysis unit: ", x$unit)
  paste0(level, x$scope, " ", x$type, " interval; ", x$method, unit)
}

#' Wilson score intervals for binomial proportions
#'
#' @param success Number of successes.
#' @param total Number of trials.
#' @param level Coverage probability.
#' @return A data frame with `estimate`, `lower`, `upper`, `success` and `total`.
#' @export
wilson_interval <- function(success, total, level = 0.95) {
  success <- as.numeric(success)
  total <- as.numeric(total)
  if (length(success) != length(total) && length(success) != 1L && length(total) != 1L) {
    cli::cli_abort("{.arg success} and {.arg total} must have compatible lengths.")
  }
  n <- max(length(success), length(total))
  success <- rep_len(success, n)
  total <- rep_len(total, n)
  if (any(!is.finite(success)) || any(!is.finite(total)) ||
      any(total <= 0) || any(success < 0) || any(success > total)) {
    cli::cli_abort("Binomial counts must satisfy 0 <= {.arg success} <= {.arg total} and total > 0.")
  }
  check_number(level, "level", min = .Machine$double.eps,
               max = 1 - .Machine$double.eps)
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- success / total
  den <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / den
  half <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / den
  data.frame(estimate = p, lower = pmax(0, centre - half),
             upper = pmin(1, centre + half), success = success, total = total)
}

#' Generic estimate-and-interval plot
#'
#' @param data A data frame containing estimates and interval limits.
#' @param estimate,lower,upper,label Column names as strings.
#' @param group Optional grouping column.
#' @param null Optional null value.
#' @param orientation `"horizontal"` or `"vertical"`.
#' @param interval Optional [interval_spec()] attached to the plot.
#' @param venue Target venue.
#' @return A ggplot object.
#' @export
fig_interval <- function(data, estimate, lower, upper, label, group = NULL,
                         null = NULL,
                         orientation = c("horizontal", "vertical"),
                         interval = interval_spec(),
                         venue = "nature") {
  orientation <- match.arg(orientation)
  d <- data.frame(
    .estimate = as.numeric(data[[estimate]]),
    .lower = as.numeric(data[[lower]]),
    .upper = as.numeric(data[[upper]]),
    .label = factor(data[[label]], levels = rev(unique(data[[label]]))),
    .group = factor(if (is.null(group)) "Estimate" else data[[group]])
  )
  keep <- stats::complete.cases(d)
  d <- d[keep, , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$.estimate, .data$.label,
                                       colour = .data$.group)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$.lower, xmax = .data$.upper),
                            orientation = "y", linewidth = lw(0.55),
                            show.legend = !is.null(group)) +
    ggplot2::geom_point(size = ps(1.8), show.legend = !is.null(group)) +
    scale_colour_vellum() +
    ggplot2::labs(x = estimate, y = NULL,
                  colour = if (is.null(group)) NULL else group,
                  caption = interval_label(interval)) +
    theme_venue(venue)
  if (!is.null(null)) p <- p + annotate_null(null)
  if (orientation == "vertical") p <- p + ggplot2::coord_flip()
  attr(p, "vellum_interval") <- interval
  p
}
