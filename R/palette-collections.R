# Semantic palette collections ----------------------------------------------
#
# These colours are intentionally kept as role-bearing collections rather than
# copied into an anonymous colour bag. Several came from project- or
# publication-specific skills: preserving their names, provenance and intended
# use prevents a brand palette from quietly becoming Vellum's universal default.

.vellum_role_collections <- list(
  flow_semantic = list(
    colours = c(
      cohort = "#2F5D7C",
      treatment_group = "#5B6F28",
      followup_group = "#4D7B43",
      treatment_outcome = "#007C89",
      followup_outcome = "#3A6596",
      loss = "#A66A00",
      death = "#8A2D3B"
    ),
    display_roles = c(
      "cohort", "treatment_group", "followup_group",
      "treatment_outcome", "followup_outcome", "loss", "death"
    ),
    source = "Flowshark semantic role system",
    intended_use = "Cohort and participant-flow diagrams",
    scope = "general semantic",
    accessibility = "Role-first; pair colour with labels, position or shape."
  ),
  flow_greyscale = list(
    colours = c(
      cohort = "#4D4D4D",
      treatment_group = "#6A6A6A",
      followup_group = "#5A5A5A",
      treatment_outcome = "#333333",
      followup_outcome = "#595959",
      loss = "#777777",
      death = "#222222"
    ),
    display_roles = c(
      "death", "treatment_outcome", "cohort", "followup_group",
      "followup_outcome", "treatment_group", "loss"
    ),
    source = "Flowshark greyscale role system",
    intended_use = "Monochrome flow diagrams and print fallbacks",
    scope = "general semantic",
    accessibility = "Greyscale by construction; labels remain mandatory."
  ),
  flow_okabe_ito = list(
    colours = c(
      cohort = "#0072B2",
      treatment_group = "#009E73",
      followup_group = "#56B4E9",
      treatment_outcome = "#009E73",
      followup_outcome = "#0072B2",
      loss = "#E69F00",
      death = "#D55E00"
    ),
    display_roles = c(
      "cohort", "treatment_group", "followup_group", "loss", "death"
    ),
    source = "Flowshark roles using Okabe-Ito colours",
    intended_use = "Colour-vision-aware participant-flow diagrams",
    scope = "general semantic",
    accessibility = "Colour-vision-aware; repeated semantic roles share colour."
  ),
  flow_jama = list(
    colours = c(
      cohort = "#374E55",
      treatment_group = "#79AF97",
      followup_group = "#6A6599",
      treatment_outcome = "#00A1D5",
      followup_outcome = "#374E55",
      loss = "#DF8F44",
      death = "#B24745"
    ),
    display_roles = c(
      "cohort", "treatment_group", "followup_group",
      "treatment_outcome", "loss", "death"
    ),
    source = "Flowshark JAMA-inspired role system",
    intended_use = "JAMA-styled participant-flow and editorial diagrams",
    scope = "editorial",
    accessibility = "Direct labels and redundant encodings are required."
  ),
  flow_lancet = list(
    colours = c(
      cohort = "#00468B",
      treatment_group = "#42B540",
      followup_group = "#0099B4",
      treatment_outcome = "#0099B4",
      followup_outcome = "#00468B",
      loss = "#FDAF91",
      death = "#ED0000"
    ),
    display_roles = c(
      "cohort", "treatment_group", "followup_group", "loss", "death"
    ),
    source = "Flowshark Lancet-inspired role system",
    intended_use = "Lancet-styled participant-flow and editorial diagrams",
    scope = "editorial",
    accessibility = "Direct labels and redundant encodings are required."
  ),
  protein_plus = list(
    colours = c(
      unadjusted = "#2166AC",
      adjustment_1 = "#E08214",
      adjustment_2 = "#969696",
      background_band = "#EDF2F9",
      alternate_band = "#FAFAFA",
      significance = "#C0392B"
    ),
    display_roles = c("unadjusted", "adjustment_1", "adjustment_2"),
    source = "Protein Plus R plotting theme",
    intended_use = "Protein Plus adjustment levels and significance",
    scope = "project-specific",
    accessibility = "Not a package default; preserve the named analytical roles."
  ),
  structured_domains = list(
    colours = c(
      drought = "#D97706",
      heat = "#EA580C",
      systems = "#1D4ED8",
      nutrition = "#15803D",
      model = "#374151"
    ),
    display_roles = c("drought", "heat", "systems", "nutrition", "model"),
    source = "Structured-diagrams domain system",
    intended_use = "Domain-aware systems and evidence diagrams",
    scope = "general semantic",
    accessibility = "Colour must be paired with labels, position or shape."
  ),
  structured_severity = list(
    colours = c(
      watch = "#15803D",
      warning = "#D97706",
      high = "#EA580C",
      emergency = "#DC2626"
    ),
    display_roles = c("watch", "warning", "high", "emergency"),
    source = "Structured-diagrams severity system",
    intended_use = "Ordered warning and severity states",
    scope = "general semantic",
    accessibility = "Order and text must remain visible without colour."
  ),
  atni = list(
    colours = c(
      purple = "#9658AD",
      blue = "#0099D9",
      green = "#3F783F",
      yellow = "#ECC441",
      orange = "#EE952F",
      red = "#B63735",
      pink = "#FC7984",
      light_neutral = "#E4E4E4",
      dark_neutral = "#787878"
    ),
    display_roles = c(
      "purple", "blue", "green", "yellow", "orange", "red", "pink"
    ),
    source = "Nature Portfolio figures skill, ATNi-facing identity",
    intended_use = "ATNi-facing figures and branded visual systems",
    scope = "brand-specific",
    accessibility = "Not a universal default; audit the selected subset."
  ),
  cameo_monochrome = list(
    colours = c(
      ink = "#111111",
      muted = "#555555",
      rule = "#C9D2D8",
      band = "#FCFCFA",
      panel = "#FAFAF7"
    ),
    display_roles = c("ink", "muted", "rule"),
    source = "CAMEO journal SVG and supplied flowchart conventions",
    intended_use = "Restrained monochrome diagrams and structural figures",
    scope = "editorial",
    accessibility = "Use hierarchy, rules and whitespace as the primary encoding."
  )
)

palette_collection_key <- function(x) vellum_key(x)

#' List Vellum's semantic palette collections
#'
#' @description
#' Lists the role-bearing colour systems imported from the user's visualisation
#' skills. Unlike a generic qualitative palette, every colour retains its
#' semantic role, provenance, intended use and scope. Project- and brand-specific
#' systems are therefore available without becoming Vellum defaults.
#'
#' @param collection Optional collection name.
#' @return A data frame with one row per collection.
#' @export
#' @examples
#' vellum_palette_collections()
#' vellum_palette_collections("flow_jama")
vellum_palette_collections <- function(collection = NULL) {
  rows <- lapply(names(.vellum_role_collections), function(nm) {
    x <- .vellum_role_collections[[nm]]
    data.frame(
      collection = nm,
      n = length(x$display_roles),
      roles = paste(names(x$colours), collapse = ", "),
      source = x$source,
      intended_use = x$intended_use,
      scope = x$scope,
      accessibility = x$accessibility,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (!is.null(collection)) {
    check_string(collection, "collection")
    key <- palette_collection_key(collection)
    if (!key %in% out$collection) {
      cli::cli_abort(c(
        "Unknown palette collection {.val {collection}}.",
        i = "See {.fn vellum_palette_collections} for available collections."
      ))
    }
    out <- out[out$collection == key, , drop = FALSE]
  }
  out
}

#' Retrieve a semantic role palette
#'
#' @description
#' Returns the exact named colours for a semantic collection. Supplying `roles`
#' is preferred: it makes the visual meaning explicit in code and prevents a
#' project-specific colour from being mistaken for a universal category colour.
#'
#' Use [vellum_pal()] when the task truly needs an unnamed qualitative sequence.
#' Chord diagrams accept either accessor through their `palette` or `colours`
#' arguments, while retaining direct labels as the primary identity encoding.
#'
#' @param collection Collection name from [vellum_palette_collections()].
#' @param roles Optional character vector of role names, in the desired order.
#' @param named Return the role names with the colours.
#' @return A character vector of hex colours.
#' @export
#' @examples
#' vellum_role_palette("flow_semantic", c("cohort", "loss", "death"))
#' vellum_role_palette("protein_plus")
vellum_role_palette <- function(collection, roles = NULL, named = TRUE) {
  check_string(collection, "collection")
  check_bool(named, "named")
  key <- palette_collection_key(collection)
  if (!key %in% names(.vellum_role_collections)) {
    cli::cli_abort(c(
      "Unknown palette collection {.val {collection}}.",
      i = "See {.fn vellum_palette_collections} for available collections."
    ))
  }
  available <- .vellum_role_collections[[key]]$colours
  if (is.null(roles)) {
    out <- available
  } else {
    if (!is.character(roles) || !length(roles) || anyNA(roles)) {
      cli::cli_abort("{.arg roles} must be a non-empty character vector.")
    }
    missing <- setdiff(roles, names(available))
    if (length(missing)) {
      cli::cli_abort(c(
        "Unknown role selection for collection {.val {key}}: {.val {missing}}.",
        i = "Available roles: {.val {names(available)}}."
      ))
    }
    out <- available[roles]
  }
  if (named) out else unname(out)
}

collection_display_colours <- function(collection, n = NULL) {
  key <- palette_collection_key(collection)
  x <- .vellum_role_collections[[key]]
  out <- x$colours[x$display_roles]
  n <- n %||% length(out)
  check_number(n, "n", min = 1)
  n <- as.integer(n)
  if (n > length(out)) {
    cli::cli_abort(c(
      "Palette collection {.val {key}} provides {length(out)} distinct display colours; {n} were requested.",
      i = "Choose a larger collection, supply explicit named {.arg colours}, or aggregate categories.",
      i = "Vellum never repeats a category colour silently."
    ))
  }
  unname(out[seq_len(n)])
}
