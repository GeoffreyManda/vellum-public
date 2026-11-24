# Figure review and safe refinement -----------------------------------------

review_row <- function(id, severity, message, repairable = FALSE) {
  data.frame(id = id, severity = severity, message = message,
             repairable = repairable, stringsAsFactors = FALSE)
}

plot_geom_names <- function(plot) {
  unique(vapply(plot$layers, function(layer) class(layer$geom)[1], character(1)))
}

plot_mapping_names <- function(plot) {
  global <- names(plot$mapping)
  local <- unlist(lapply(plot$layers, function(layer) names(layer$mapping)),
                  use.names = FALSE)
  unique(c(global, local))
}

#' Review the communication quality of a scientific plot
#'
#' `venue_check()` asks whether an artwork file satisfies a publisher. This
#' function asks the complementary question: whether the plot communicates an
#' explicit, supported and accessible scientific claim. It reports deterministic
#' diagnostics; it does not pretend to infer scientific meaning from pixels.
#'
#' @param plot A ggplot object.
#' @param venue Target venue. Defaults to the attached contract's venue, then
#'   `"nature"`.
#' @param require_contract Require a [figure_contract()].
#' @param run_venue_check Include venue-compliance findings.
#' @return A classed data frame with one row per diagnostic.
#' @export
vellum_review <- function(plot, venue = NULL, require_contract = TRUE,
                          run_venue_check = TRUE) {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a ggplot object.")
  }
  check_bool(require_contract, "require_contract")
  check_bool(run_venue_check, "run_venue_check")
  con <- plot_contract(plot)
  venue <- venue %||% con$venue %||% "nature"
  out <- list()
  add <- function(id, severity, message, repairable = FALSE) {
    out[[length(out) + 1L]] <<- review_row(id, severity, message, repairable)
  }

  if (is.null(con)) {
    add("contract", if (require_contract) "fail" else "warn",
        "No figure contract: claim, evidence, support and uncertainty are not recorded.")
  } else {
    add("contract", "pass", paste0("Structured ", con$claim_type,
                                    " claim and evidence are attached."))
    add("support", if (con$support == "not stated") "warn" else "pass",
        if (con$support == "not stated") "Support or denominator is not stated."
        else paste0("Support recorded: ", con$support))
    add("provenance", if (con$provenance == "not stated") "warn" else "pass",
        if (con$provenance == "not stated") "Data or asset provenance is not stated."
        else "Data or asset provenance is recorded.")
    add("alt-text", if (!nzchar(con$alt_text)) "fail" else "pass",
        if (!nzchar(con$alt_text)) "Alt text is missing." else "Alt text is available.")
  }

  labs <- plot$labels
  add("x-label", if (is.null(labs$x) || !nzchar(as.character(labs$x))) "warn" else "pass",
      if (is.null(labs$x) || !nzchar(as.character(labs$x))) "The x axis has no semantic label."
      else paste0("x axis labelled '", labs$x, "'."))
  add("y-label", if (is.null(labs$y) || !nzchar(as.character(labs$y))) "warn" else "pass",
      if (is.null(labs$y) || !nzchar(as.character(labs$y))) "The y axis has no semantic label."
      else paste0("y axis labelled '", labs$y, "'."))

  has_title <- !is.null(labs$title) && nzchar(as.character(labs$title))
  add("journal-title", if (has_title) "warn" else "pass",
      if (has_title) "On-plot title should normally move to the manuscript caption."
      else "No on-plot manuscript title.", repairable = has_title)

  geoms <- plot_geom_names(plot)
  interval_geoms <- c("GeomErrorbar", "GeomErrorbarh", "GeomLinerange",
                      "GeomPointrange", "GeomRibbon", "GeomSmooth",
                      "GeomPointinterval", "GeomBoxplot", "GeomHline",
                      "GeomSegment", "GeomPath", "GeomPolygon", "GeomSf")
  has_interval <- any(geoms %in% interval_geoms)
  expects_interval <- !is.null(con) &&
    grepl("interval|band|limits|range|iqr|credible|confidence|prediction|bootstrap|wilson",
          tolower(con$uncertainty))
  add("uncertainty", if (expects_interval && !has_interval) "fail" else "pass",
      if (expects_interval && !has_interval) {
        paste0("The contract promises ", con$uncertainty,
               " but no interval or band geometry was detected.")
      } else if (has_interval) "An interval or band geometry is present."
      else "The contract does not require an uncertainty geometry.")

  aesthetics <- plot_mapping_names(plot)
  colour_used <- any(c("colour", "color", "fill") %in% aesthetics)
  redundant <- any(c("shape", "linetype", "group") %in% aesthetics)
  add("redundant-encoding", if (colour_used && !redundant) "warn" else "pass",
      if (colour_used && !redundant) {
        "A mapped colour or fill has no detected shape, line-type, group or direct-label channel."
      } else "Meaning is not detectably dependent on colour alone.")

  if (run_venue_check) {
    vc <- suppressWarnings(venue_check(plot, venue = venue))
    tab <- summary(vc)
    bad <- tab$verdict %in% c("fail", "warn")
    if (any(bad)) {
      for (i in which(bad)) {
        add(paste0("venue-", tab$check[i]), tab$verdict[i], tab$detail[i])
      }
    } else {
      add("venue", "pass", paste0("All applicable ", venue,
                                   " pre-flight checks pass."))
    }
  }

  result <- do.call(rbind, out)
  weight <- c(pass = 0, warn = 1, fail = 3, skip = 0)
  # A severity outside the four known levels is a finding that has not been
  # weighted, not a finding worth nothing, and it must not take the whole score
  # to NA with it.
  penalty <- unname(weight[result$severity])
  penalty[is.na(penalty)] <- weight[["warn"]]
  attr(result, "score") <- max(0, 100 - sum(penalty))
  attr(result, "venue") <- venue
  class(result) <- c("vellum_review", "data.frame")
  result
}

#' @export
print.vellum_review <- function(x, ...) {
  cli::cli_h2("Vellum review - score {attr(x, 'score')}/100")
  sym <- c(pass = "\u2714", warn = "!", fail = "\u2716", skip = "\u2139")
  for (i in seq_len(nrow(x))) {
    mark <- if (x$severity[i] %in% names(sym)) sym[[x$severity[i]]] else "*"
    cli::cli_bullets(stats::setNames(x$message[i], mark))
  }
  invisible(x)
}

#' Apply conservative visual repairs
#'
#' Safe refinement applies a named design profile, moves an on-plot title out of
#' the plotting area when requested, and preserves any attached figure contract.
#' It never changes data, scales, estimates or uncertainty.
#'
#' @param plot A ggplot object.
#' @param profile A Vellum design profile.
#' @param venue Target venue.
#' @param move_title Remove the on-plot title and retain it as metadata.
#' @param legend Optional legend-position override.
#' @return A refined ggplot object.
#' @export
vellum_refine <- function(
    plot,
    profile = c("restrained", "editorial", "clinical", "technical", "presentation"),
    venue = NULL,
    move_title = TRUE,
    legend = NULL) {
  if (!inherits(plot, "ggplot")) cli::cli_abort("{.arg plot} must be a ggplot object.")
  profile <- match.arg(profile)
  check_bool(move_title, "move_title")
  con <- plot_contract(plot)
  venue <- venue %||% con$venue %||% if (profile == "presentation") "slide" else "nature"
  old_title <- plot$labels$title
  out <- plot + theme_vellum_profile(profile, venue = venue)
  if (!is.null(legend)) out <- out + ggplot2::theme(legend.position = legend)
  if (move_title && !is.null(old_title)) {
    out <- out + ggplot2::labs(title = NULL)
    attr(out, "vellum_moved_title") <- old_title
  }
  if (!is.null(con)) out <- attach_contract(out, con)
  out
}

#' Produce a deterministic visual-QC manifest
#'
#' @param plots A named list of ggplot objects.
#' @param venue Target venue.
#' @return A data frame containing review scores and issue counts.
#' @export
visual_qc_manifest <- function(plots, venue = "nature") {
  if (!is.list(plots) || !length(plots) || any(!vapply(plots, inherits, logical(1), "ggplot"))) {
    cli::cli_abort("{.arg plots} must be a non-empty list of ggplot objects.")
  }
  nm <- names(plots)
  if (is.null(nm) || any(!nzchar(nm))) nm <- paste0("plot-", seq_along(plots))
  rows <- lapply(seq_along(plots), function(i) {
    r <- vellum_review(plots[[i]], venue = venue, require_contract = FALSE)
    data.frame(
      plot = nm[i],
      score = attr(r, "score"),
      pass = sum(r$severity == "pass"),
      warn = sum(r$severity == "warn"),
      fail = sum(r$severity == "fail"),
      contract = !is.null(plot_contract(plots[[i]])),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
