# Figure contracts -----------------------------------------------------------

#' Describe what a scientific figure is allowed to claim
#'
#' A figure contract keeps interpretation, evidence, support, uncertainty,
#' provenance, accessibility and destination beside the plot they govern. It is
#' deliberately small enough to write before plotting and structured enough to
#' generate captions, alt text and review diagnostics.
#'
#' @param claim One sentence stating the conclusion the figure earns.
#' @param claim_type The inferential status of the claim.
#' @param evidence Data source, experiment, model or conceptual basis.
#' @param support Denominator, sample size, coverage or other support statement.
#' @param uncertainty Named uncertainty display, or `"not applicable"`.
#' @param provenance Source, licence or reproducibility statement.
#' @param accessibility Redundant encoding and accessibility intent.
#' @param editability Source/editability statement.
#' @param venue Target venue or communication setting.
#' @param alt_text Optional concise alt text. When omitted it is derived from the
#'   claim, support and uncertainty.
#' @param adjustment Optional model adjustment or conditioning statement.
#' @return A `vellum_contract` object.
#' @export
figure_contract <- function(
    claim,
    claim_type = c("descriptive", "prognostic", "mechanistic", "interventional",
                   "causal", "methodological", "interpretive", "conceptual"),
    evidence,
    support = "not stated",
    uncertainty = "not applicable",
    provenance = "not stated",
    accessibility = "colour is supplemented by position, shape, line type, or labels",
    editability = "vector source retained",
    venue = "nature",
    alt_text = NULL,
    adjustment = NULL) {
  check_string(claim, "claim")
  claim_type <- match.arg(claim_type)
  check_string(evidence, "evidence")
  fields <- list(support = support, uncertainty = uncertainty, provenance = provenance,
                 accessibility = accessibility, editability = editability, venue = venue)
  for (nm in names(fields)) check_string(fields[[nm]], nm)
  if (!is.null(alt_text)) check_string(alt_text, "alt_text")
  if (!is.null(adjustment)) check_string(adjustment, "adjustment")

  out <- list(
    claim = claim,
    claim_type = claim_type,
    evidence = evidence,
    support = support,
    uncertainty = uncertainty,
    provenance = provenance,
    accessibility = accessibility,
    editability = editability,
    venue = venue,
    adjustment = adjustment
  )
  out$alt_text <- alt_text %||% contract_alt_text(out)
  structure(out, class = "vellum_contract")
}

#' @export
print.vellum_contract <- function(x, ...) {
  cli::cli_h2("Figure contract")
  cli::cli_bullets(c(
    "*" = "{.strong Claim} ({x$claim_type}): {x$claim}",
    "*" = "{.strong Evidence}: {x$evidence}",
    "*" = "{.strong Support}: {x$support}",
    "*" = "{.strong Uncertainty}: {x$uncertainty}",
    "*" = "{.strong Venue}: {x$venue}"
  ))
  invisible(x)
}

#' Attach a figure contract to a plot
#'
#' @param plot A ggplot or patchwork object.
#' @param contract A [figure_contract()] object.
#' @return `plot`, with its contract attached.
#' @export
attach_contract <- function(plot, contract) {
  if (!inherits(contract, "vellum_contract")) {
    cli::cli_abort("{.arg contract} must be created by {.fn figure_contract}.")
  }
  attr(plot, "vellum_contract") <- contract
  plot
}

#' Retrieve a plot's figure contract
#'
#' @param plot A ggplot or patchwork object.
#' @param required Error if no contract is attached.
#' @return A `vellum_contract`, or `NULL`.
#' @export
plot_contract <- function(plot, required = FALSE) {
  check_bool(required, "required")
  out <- attr(plot, "vellum_contract", exact = TRUE)
  if (required && is.null(out)) {
    cli::cli_abort("The plot has no figure contract. Attach one with {.fn attach_contract}.")
  }
  out
}

#' Generate a manuscript-ready caption from a figure contract
#'
#' @param x A `vellum_contract` or a plot carrying one.
#' @param include_provenance Include the provenance sentence.
#' @return A single caption string.
#' @export
contract_caption <- function(x, include_provenance = FALSE) {
  check_bool(include_provenance, "include_provenance")
  con <- if (inherits(x, "vellum_contract")) x else plot_contract(x, required = TRUE)
  pieces <- c(
    paste0(con$claim, " "),
    paste0("Evidence: ", con$evidence, ". "),
    if (con$support != "not stated") paste0("Support: ", con$support, ". ") else NULL,
    if (!is.null(con$adjustment)) paste0("Adjustment: ", con$adjustment, ". ") else NULL,
    paste0("Uncertainty: ", con$uncertainty, "."),
    if (include_provenance) paste0(" Provenance: ", con$provenance, ".") else NULL
  )
  gsub("[.][.]+", ".", paste0(pieces, collapse = ""))
}

#' Generate or retrieve figure alt text
#'
#' @param x A `vellum_contract` or a plot carrying one.
#' @return A single alt-text string.
#' @export
contract_alt_text <- function(x) {
  con <- if (inherits(x, "vellum_contract") || (is.list(x) && !is.null(x$claim))) {
    x
  } else {
    plot_contract(x, required = TRUE)
  }
  if (!is.null(con$alt_text)) return(con$alt_text)
  support <- if (!is.null(con$support) && con$support != "not stated") {
    paste0(" Support: ", con$support, ".")
  } else ""
  unc <- if (!is.null(con$uncertainty) && con$uncertainty != "not applicable") {
    paste0(" Uncertainty shown as ", con$uncertainty, ".")
  } else ""
  paste0("Scientific figure showing: ", con$claim, ".", support, unc)
}
