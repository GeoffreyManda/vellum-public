# Brand compiler -------------------------------------------------------------
#
# These APIs turn the identity registry into a semantic compiler: the same plot
# can be rendered for a technical report, policy brief, slide, dashboard, social
# card or journal figure without rewriting its analytical layers.

.brand_modes <- list(
  journal_figure = list(profile = "restrained", venue = "nature", grid = "none",
                        axis_style = "spine", density = "compact", colour = "accessible"),
  technical_report = list(profile = "editorial", venue = "portfolio", grid = "y",
                          axis_style = "spine", density = "comfortable", colour = "brand"),
  policy_brief = list(profile = "editorial", venue = "portfolio", grid = "y",
                      axis_style = "spine", density = "airy", colour = "brand"),
  executive_slide = list(profile = "presentation", venue = "slide", grid = "y",
                         axis_style = "spine", density = "airy", colour = "brand"),
  dashboard = list(profile = "technical", venue = "portfolio", grid = "XY",
                   axis_style = "frame", density = "compact", colour = "accessible"),
  social_card = list(profile = "presentation", venue = "slide", grid = "none",
                     axis_style = "none", density = "airy", colour = "brand")
)

#' List BrandStack output modes
#'
#' @return A data frame describing the six supported design surfaces.
#' @export
brand_modes <- function() {
  do.call(rbind, lapply(names(.brand_modes), function(nm) {
    x <- .brand_modes[[nm]]
    data.frame(mode = nm, profile = x$profile, venue = x$venue,
               grid = x$grid, density = x$density, colour = x$colour,
               stringsAsFactors = FALSE)
  }))
}

#' List, search and inspect brand profiles
#'
#' @param type Optional identity type.
#' @param provenance Optional provenance class.
#' @return `brand_list()` returns the implemented registry.
#' @export
brand_list <- function(type = NULL, provenance = NULL) {
  out <- vellum_identities(type = type)
  if (!is.null(provenance)) out <- out[out$provenance %in% provenance, , drop = FALSE]
  out
}

#' @rdname brand_list
#' @param query Text to search across id, name, provenance, version and notes.
#' @export
brand_search <- function(query) {
  check_string(query, "query")
  x <- brand_list()
  haystack <- apply(x[c("identity", "label", "type", "provenance",
                        "source_version", "note")], 1, paste, collapse = " ")
  x[grepl(query, haystack, ignore.case = TRUE), , drop = FALSE]
}

#' @rdname brand_list
#' @param brand Brand identifier.
#' @export
brand_get <- function(brand) identity_spec(brand)

#' @rdname brand_list
#' @export
brand_info <- function(brand) {
  x <- identity_spec(brand)
  class(x) <- c("vellum_brand_info", "list")
  x
}

#' @export
print.vellum_brand_info <- function(x, ...) {
  cli::cli_h1("{x$label}")
  cli::cli_text("{.strong Provenance} {x$provenance} \u00b7 confidence {sprintf('%.2f', x$confidence)}")
  cli::cli_text("{.strong Version} {x$source_version} \u00b7 verified {as.character(x$verified_on)}")
  cli::cli_text("{.strong Fonts} display {x$fonts$display[1]} \u00b7 body {x$fonts$body[1]}")
  cli::cli_text("{.strong Sources} {length(x$sources)} recorded")
  cli::cli_text("{.strong Legal} logos bundled: no \u00b7 endorsement implied: no")
  cli::cli_text("{x$note}")
  invisible(x)
}

#' Apply a brand to an output surface
#'
#' @param brand Brand or journal id.
#' @param mode One of [brand_modes()]$mode.
#' @param venue Optional destination override.
#' @param colour `"brand"` or `"accessible"`; defaults by output mode.
#' @param ... Passed to [theme_identity()].
#' @return A complete ggplot2 theme with brand/output metadata.
#' @export
theme_brand <- function(brand, mode = names(.brand_modes), venue = NULL,
                        colour = NULL, ...) {
  mode <- match.arg(mode)
  spec <- identity_get(brand)
  cfg <- .brand_modes[[mode]]
  if (spec$type == "journal" && mode == "journal_figure" && is.null(venue)) {
    venue <- spec$venue
  }
  venue <- venue %||% cfg$venue
  colour <- colour %||% cfg$colour
  colour <- match.arg(colour, c("brand", "accessible"))
  args <- list(
    identity = spec$name, venue = venue, profile = cfg$profile, mode = colour,
    grid = cfg$grid, axis_style = cfg$axis_style, density = cfg$density
  )
  dots <- list(...)
  args[names(dots)] <- dots
  th <- do.call(theme_identity, args)
  attr(th, "vellum_brand_mode") <- list(
    brand = spec$name, mode = mode, venue = venue, colour = colour
  )
  th
}

#' Semantic identity colours and scales
#'
#' @description
#' Semantic scales map analytical states to named roles. This prevents a brand
#' colour from changing meaning when category order changes.
#'
#' @param brand Brand identifier.
#' @param mapping Optional named character vector whose values are semantic roles
#'   such as `"positive"`, `"warning"`, `"negative"` or `"neutral"`.
#' @param named Preserve names.
#' @return A named colour vector.
#' @export
brand_semantic <- function(brand, mapping = NULL, named = TRUE) {
  check_bool(named, "named")
  sem <- identity_get(brand)$semantic
  if (is.null(mapping)) {
    out <- sem
  } else {
    if (!is.character(mapping) || is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
      cli::cli_abort("{.arg mapping} must be a named character vector of semantic roles.")
    }
    unknown <- setdiff(unname(mapping), names(sem))
    if (length(unknown)) {
      cli::cli_abort(c(
        "Unknown semantic role{?s}: {.val {unknown}}.",
        i = "Available roles: {.val {names(sem)}}."
      ))
    }
    out <- unname(sem[unname(mapping)])
    names(out) <- names(mapping)
  }
  if (!named) names(out) <- NULL
  out
}

#' @rdname brand_semantic
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @param aesthetics Aesthetic name.
#' @export
scale_colour_semantic <- function(brand, mapping, ..., aesthetics = "colour") {
  values <- brand_semantic(brand, mapping)
  ggplot2::discrete_scale(
    aesthetics, palette = function(n) {
      if (n > length(values)) {
        cli::cli_abort("Semantic mapping defines {length(values)} states; {n} were requested.")
      }
      unname(values[seq_len(n)])
    }, limits = names(values), ...
  )
}

#' @rdname brand_semantic
#' @export
scale_color_semantic <- scale_colour_semantic

#' @rdname brand_semantic
#' @export
scale_fill_semantic <- function(brand, mapping, ..., aesthetics = "fill") {
  scale_colour_semantic(brand, mapping, ..., aesthetics = aesthetics)
}

.font_substitutions <- data.frame(
  reference_font = c("Univers", "Univers LT Pro", "Myriad Pro", "Gill Sans",
                     "Futura", "Proxima Nova", "Bliss Pro", "KPMG Font",
                     "Helvetica", "Arial", "Andes"),
  open_default = c("Source Sans 3", "Source Sans 3", "Source Sans 3", "Cabin",
                   "Jost", "Inter", "Source Sans 3", "Inter Tight",
                   "Inter", "Liberation Sans", "Source Sans 3"),
  secondary_option = c("Noto Sans", "Noto Sans", "Public Sans", "Source Sans 3",
                       "Montserrat", "Manrope", "Noto Sans", "Archivo Narrow",
                       "Nimbus Sans", "Noto Sans", "Noto Sans"),
  stringsAsFactors = FALSE
)

#' Font substitution map and resolver
#'
#' @return `brand_font_substitutions()` returns the documented substitution map.
#' @export
brand_font_substitutions <- function() .font_substitutions

#' @rdname brand_font_substitutions
#' @param requested Requested reference family.
#' @param fallback Optional explicit fallback.
#' @param prefer_installed Prefer the requested font when installed.
#' @export
brand_font_resolve <- function(requested, fallback = NULL, prefer_installed = TRUE) {
  check_string(requested, "requested")
  check_bool(prefer_installed, "prefer_installed")
  row <- .font_substitutions[.font_substitutions$reference_font == requested, , drop = FALSE]
  candidates <- unique(c(
    if (prefer_installed) requested,
    fallback,
    if (nrow(row)) row$open_default,
    if (nrow(row)) row$secondary_option,
    "Arial", "Helvetica", "sans"
  ))
  candidates <- candidates[nzchar(candidates)]
  available <- tryCatch(unique(systemfonts::system_fonts()$family),
                        error = function(e) character())
  hit <- candidates[candidates %in% c(available, "sans", "serif", "mono")]
  resolved <- if (length(hit)) hit[1] else "sans"
  structure(resolved, requested = requested, candidates = candidates,
            substituted = !identical(resolved, requested))
}

#' Register, extend or reset identity profiles
#'
#' @param spec A list containing at least `id`, `name`, `provenance`,
#'   `confidence`, `fonts`, `colours` and `palettes`.
#' @return Invisibly, the registered specification.
#' @export
brand_register <- function(spec) {
  if (!is.list(spec)) cli::cli_abort("{.arg spec} must be a list.")
  required <- c("id", "name", "provenance", "confidence", "fonts", "colours", "palettes")
  missing <- setdiff(required, names(spec))
  if (length(missing)) cli::cli_abort("Brand spec is missing: {.val {missing}}.")
  check_string(spec$id, "spec$id")
  check_string(spec$name, "spec$name")
  if (!spec$provenance %in% .provenance_levels) {
    cli::cli_abort("{.arg spec$provenance} must be one of {.val {.provenance_levels}}.")
  }
  check_number(spec$confidence, "spec$confidence", min = 0, max = 1)
  if (!all(c("body", "display") %in% names(spec$fonts))) {
    cli::cli_abort("{.arg spec$fonts} must define {.val body} and {.val display}.")
  }
  core <- spec$colours
  required_core <- c("primary", "secondary", "ink", "weak", "paper", "panel", "grid", "na")
  if (!all(required_core %in% names(core))) {
    cli::cli_abort("{.arg spec$colours} must define {.val {required_core}}.")
  }
  categorical <- spec$palettes$categorical %||% spec$palettes$identity
  sequential <- spec$palettes$sequential
  diverging <- spec$palettes$diverging
  if (is.null(categorical) || is.null(sequential) || is.null(diverging)) {
    cli::cli_abort("{.arg spec$palettes} must define categorical, sequential and diverging colours.")
  }
  entry <- .identity_entry(
    identity_key(spec$id), spec$name, spec$type %||% "institution",
    spec$status %||% "observed", spec$source %||% NA_character_,
    spec$fonts, core, categorical, sequential, diverging,
    accessible = spec$palettes$accessible %||% .identity_accessible,
    venue = spec$venue %||% "portfolio", note = spec$note %||% "",
    retrieved = as.character(spec$verified_on %||% Sys.Date()),
    palette_source = spec$palette_source %||% spec$source %||% NA_character_,
    font_source = spec$font_source %||% spec$source %||% NA_character_,
    provenance = spec$provenance, confidence = spec$confidence,
    source_version = spec$source_version %||% "User-registered profile",
    semantic = spec$semantic, geographic = spec$palettes$geographic,
    layout = spec$layout %||% list(), chart_rules = spec$chart_rules %||% list(),
    table_rules = spec$table_rules %||% list(), legal = spec$legal %||% list(),
    sources = spec$sources
  )
  assign(entry$name, entry, envir = .identity_user)
  cli::cli_inform("Registered brand {.val {entry$name}} with provenance {.val {entry$provenance}}.")
  invisible(entry)
}

#' @rdname brand_register
#' @param parent Parent identity id.
#' @param id New child id.
#' @param overrides Named list of fields to override recursively.
#' @export
brand_extend <- function(parent, id, overrides = list()) {
  check_string(parent, "parent")
  check_string(id, "id")
  if (!is.list(overrides)) cli::cli_abort("{.arg overrides} must be a list.")
  child <- utils::modifyList(identity_spec(parent), overrides)
  child$name <- identity_key(id)
  child$label <- overrides$label %||% id
  child$source_version <- overrides$source_version %||%
    paste0("Extension of ", parent)
  child$note <- paste(trimws(child$note %||% ""),
                      "Regional or programme extension; parent identity retained.")
  assign(child$name, child, envir = .identity_user)
  invisible(child)
}

#' @rdname brand_register
#' @export
brand_reset <- function() {
  rm(list = ls(.identity_user), envir = .identity_user)
  invisible(TRUE)
}

#' Preview and compare identities
#'
#' @param brand Brand id.
#' @param variant Palette variant.
#' @return `brand_preview()` returns a ggplot; `brand_compare()` a data frame.
#' @export
brand_preview <- function(brand, variant = "categorical") {
  spec <- identity_get(brand)
  cols <- identity_palette(brand, variant, named = TRUE)
  d <- data.frame(role = names(cols) %none% paste0("colour_", seq_along(cols)),
                  colour = unname(cols), order = seq_along(cols),
                  stringsAsFactors = FALSE)
  # Long institutional systems become illegible when forced into one strip.
  # Eight swatches per row keeps role names and hexadecimal values inside their
  # own tiles while short palettes remain a single, easily scanned sequence.
  max_cols <- if (nrow(d) > 10L) 8L else nrow(d)
  d$column <- (d$order - 1L) %% max_cols + 1L
  d$row <- -((d$order - 1L) %/% max_cols)
  d$text_colour <- vapply(d$colour, function(cl) {
    if (contrast_ratio("#111111", cl) >= contrast_ratio("#FFFFFF", cl)) "#111111" else "#FFFFFF"
  }, character(1))
  ggplot2::ggplot(d, ggplot2::aes(.data$column, .data$row, fill = .data$colour)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(.data$role, .data$colour, sep = "\n"),
                                    colour = .data$text_colour),
                       fontface = "bold", size = 3.1) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = NULL, expand = ggplot2::expansion(mult = .005)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(mult = .03)) +
    ggplot2::labs(title = spec$label,
                  subtitle = paste(spec$provenance, "confidence", sprintf("%.2f", spec$confidence)),
                  x = NULL, y = NULL) +
    theme_brand(brand, "technical_report", grid = "none", axis_style = "none",
                legend = "none")
}

#' @rdname brand_preview
#' @param brands Character vector of brand ids.
#' @export
brand_compare <- function(brands, variant = "categorical") {
  if (!is.character(brands) || !length(brands)) {
    cli::cli_abort("{.arg brands} must be a non-empty character vector.")
  }
  do.call(rbind, lapply(brands, function(id) {
    spec <- identity_get(id)
    audit <- palette_audit(identity_palette(id, variant, named = FALSE),
                           type = if (variant %in% c("sequential", "diverging")) variant else "qualitative")
    data.frame(
      identity = id, label = spec$label, provenance = spec$provenance,
      confidence = spec$confidence, palette = variant,
      colours = length(audit$colours), verdict = audit$verdict,
      min_normal = unname(audit$separation[["normal"]]),
      min_deutan = unname(audit$separation[["deuteranopia"]]),
      min_protan = unname(audit$separation[["protanopia"]]),
      min_tritan = unname(audit$separation[["tritanopia"]]),
      stringsAsFactors = FALSE
    )
  }))
}

#' Check brand provenance, colour, fonts and legal safeguards
#'
#' @param brand Brand id.
#' @param output Intended output format.
#' @param simulate_cvd Audit colour-vision-deficiency separation.
#' @param check_fonts Check requested and resolved families.
#' @param check_trademarks Check that marks are excluded and warnings recorded.
#' @return A classed data frame with pass/warn/fail results.
#' @export
brand_check <- function(brand, output = c("pdf", "svg", "png", "dashboard"),
                        simulate_cvd = TRUE, check_fonts = TRUE,
                        check_trademarks = TRUE) {
  output <- match.arg(output)
  check_bool(simulate_cvd, "simulate_cvd")
  check_bool(check_fonts, "check_fonts")
  check_bool(check_trademarks, "check_trademarks")
  spec <- identity_get(brand)
  audit <- palette_audit(identity_palette(brand, "categorical", named = FALSE),
                         type = "qualitative")
  rows <- list(
    data.frame(id = "provenance", severity = if (spec$confidence >= .75) "pass" else "warn",
               evidence = paste(spec$provenance, sprintf("confidence %.2f", spec$confidence))),
    data.frame(id = "text-contrast", severity = if (contrast_ratio(spec$core[["ink"]], spec$core[["paper"]]) >= 4.5) "pass" else "fail",
               evidence = sprintf("%.2f:1", contrast_ratio(spec$core[["ink"]], spec$core[["paper"]]))),
    data.frame(id = "semantic-roles", severity = if (all(c("positive", "warning", "negative", "missing") %in% names(spec$semantic))) "pass" else "fail",
               evidence = paste(length(spec$semantic), "named roles")),
    data.frame(id = "output", severity = "pass",
               evidence = paste(output, if (output %in% c("pdf", "svg")) "vector-capable" else "review/screen output"))
  )
  if (simulate_cvd) {
    sep <- audit$separation[c("normal", "deuteranopia", "protanopia", "tritanopia")]
    worst <- names(sep)[which.min(sep)]
    rows[[length(rows) + 1L]] <- data.frame(
      id = "categorical-cvd", severity = audit$verdict,
      evidence = sprintf("worst %.1f dE00 under %s", min(sep), worst)
    )
  }
  if (check_fonts) {
    requested <- identity_font(brand, "body", resolve = FALSE)[1]
    resolved <- as.character(identity_font(brand, "body"))
    rows[[length(rows) + 1L]] <- data.frame(
      id = "font", severity = if (identical(requested, resolved)) "pass" else "warn",
      evidence = paste(requested, "->", resolved)
    )
  }
  if (check_trademarks) rows[[length(rows) + 1L]] <- data.frame(
    id = "trademark", severity = if (!isTRUE(spec$legal$logos_included)) "pass" else "fail",
    evidence = "logos disabled; permission and endorsement remain external"
  )
  out <- do.call(rbind, rows)
  out$brand <- spec$name
  rownames(out) <- NULL
  class(out) <- c("vellum_brand_check", "data.frame")
  out
}

#' @export
print.vellum_brand_check <- function(x, ...) {
  cli::cli_h1("brand check \u00b7 {unique(x$brand)}")
  for (i in seq_len(nrow(x))) {
    mark <- switch(x$severity[i], pass = "v", warn = "!", fail = "x", "-")
    cli::cli_text("{mark} {x$id[i]}: {x$evidence[i]}")
  }
  invisible(x)
}

.global_brand_families <- list(
  `United Nations system` = c("un", "who", "unicef", "undp", "unfpa", "unhcr", "wfp", "fao", "unesco", "unwomen", "ilo", "iom", "unep", "unocha", "unaids", "unhabitat", "unodc"),
  `Multilateral and development banks` = c("world_bank", "ifc", "imf", "afdb", "adb", "iadb", "ebrd", "eib", "oecd", "european_commission"),
  `Bilateral development institutions` = c("usaid", "uk_aid", "giz", "kfw", "afd", "sida", "norad", "danida", "sdc", "jica", "koica", "global_affairs_canada", "dfat_australia", "irish_aid"),
  `Global health and philanthropy` = c("gates_foundation", "wellcome", "gavi", "global_fund", "cepi", "path", "chai", "rockefeller_foundation", "ciff", "gain", "atni", "nutrition_international"),
  `Evidence and research organisations` = c("ifpri", "cgdev", "odi_global", "rand", "brookings", "chatham_house", "cochrane", "campbell_collaboration", "jpal", "ipa"),
  `Professional services` = c("bdo", "kpmg", "pwc", "ey", "deloitte", "accenture", "mckinsey", "bcg", "bain", "grant_thornton"),
  `Scientific publishing` = c("nature", "science", "cell", "lancet", "nejm", "jama", "bmj", "plos", "elife", "frontiers", "mdpi", "springer", "elsevier", "wiley", "oup", "cambridge", "sage", "taylor_francis", "pnas")
)

#' Global identity research catalogue
#'
#' @description
#' Lists the full research roadmap without inventing palettes for organisations
#' whose current evidence has not been verified. Only `implemented` rows can be
#' passed to [brand_get()].
#' @return A data frame with organisation family and implementation status.
#' @export
brand_catalogue <- function() {
  ids <- vellum_identities()$identity
  out <- do.call(rbind, lapply(names(.global_brand_families), function(family) {
    id <- .global_brand_families[[family]]
    data.frame(family = family, identity = id,
               status = ifelse(id %in% ids, "implemented", "research_pending"),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' Journal figure modes and dimensions
#'
#' @param journal Journal id.
#' @param mode `"single_column"`, `"double_column"`, `"extended_data"` or
#'   `"graphical_abstract"`.
#' @return A one-row data frame describing the physical output contract.
#' @export
journal_dimensions <- function(journal, mode = c("single_column", "double_column",
                                                  "extended_data", "graphical_abstract")) {
  mode <- match.arg(mode)
  id <- identity_get(journal, "journal")
  venue <- venue_spec(id$venue)
  widths <- venue$widths
  wide_name <- utils::tail(intersect(c("double", "triple", "full", "max", "single"),
                                     names(widths)), 1)
  width <- if (mode == "single_column") unname(widths[["single"]]) else
    unname(widths[[wide_name]])
  height <- switch(mode,
    single_column = min(venue$height_max, width * .72),
    double_column = min(venue$height_max, width * .62),
    extended_data = min(venue$height_max, width * .90),
    graphical_abstract = min(venue$height_max, width * .56)
  )
  data.frame(journal = id$name, mode = mode, venue = venue$name,
             width_mm = width, height_mm = height,
             text_min_pt = venue$text_min, text_max_pt = venue$text_max,
             verified = venue$verified, stringsAsFactors = FALSE)
}

#' @rdname journal_dimensions
#' @param colour `"accessible"` or `"editorial"`.
#' @param ... Passed to [theme_journal()].
#' @export
journal_theme <- function(journal, mode = c("single_column", "double_column",
                                             "extended_data", "graphical_abstract"),
                          colour = c("accessible", "editorial"), ...) {
  mode <- match.arg(mode)
  colour <- match.arg(colour)
  th <- theme_journal(journal, mode = colour, ...)
  attr(th, "vellum_journal_mode") <- journal_dimensions(journal, mode)
  th
}

#' Export a brand-compiled figure
#'
#' @param plot Plot or patchwork.
#' @param filename Output stem or filename.
#' @param brand Brand id.
#' @param mode Brand output mode.
#' @param width_mm,height_mm Optional explicit dimensions.
#' @param ... Passed to [save_figure()].
#' @return The [save_figure()] manifest invisibly.
#' @export
brand_export <- function(plot, filename, brand,
                         mode = names(.brand_modes), width_mm = NULL,
                         height_mm = NULL, ...) {
  mode <- match.arg(mode)
  spec <- identity_get(brand)
  cfg <- .brand_modes[[mode]]
  venue <- if (spec$type == "journal" && mode == "journal_figure") spec$venue else cfg$venue
  width_mm <- width_mm %||% venue_width(venue, if (mode == "journal_figure") "single" else "double")
  height_mm <- height_mm %||% width_mm * if (mode %in% c("executive_slide", "social_card")) 9 / 16 else .62
  save_figure(plot, filename, venue = venue, width = width_mm,
              height = height_mm, ...)
}

.scientific_domains <- list(
  mechanistic_pathway = list(profile = "technical", grid = "none", axis_style = "none"),
  epidemiology = list(profile = "clinical", grid = "y", axis_style = "spine"),
  clinical_trial = list(profile = "clinical", grid = "x", axis_style = "spine"),
  systematic_review = list(profile = "clinical", grid = "x", axis_style = "spine"),
  genomics = list(profile = "technical", grid = "XY", axis_style = "frame"),
  spatial_health = list(profile = "technical", grid = "none", axis_style = "none"),
  health_economics = list(profile = "editorial", grid = "x", axis_style = "spine"),
  causal_dag = list(profile = "restrained", grid = "none", axis_style = "none"),
  survival_analysis = list(profile = "clinical", grid = "y", axis_style = "spine"),
  evidence_grade = list(profile = "clinical", grid = "x", axis_style = "frame")
)

#' Scientific-domain theme modes
#'
#' @param domain Scientific domain.
#' @param venue Output venue.
#' @param ... Passed to [theme_vellum_profile()].
#' @return A complete ggplot2 theme with domain metadata.
#' @export
theme_scientific_domain <- function(domain = names(.scientific_domains),
                                    venue = "nature", ...) {
  domain <- match.arg(domain)
  cfg <- .scientific_domains[[domain]]
  th <- theme_vellum_profile(cfg$profile, venue = venue, grid = cfg$grid,
                             axis_style = cfg$axis_style, ...)
  attr(th, "vellum_scientific_domain") <- domain
  th
}

#' @rdname theme_scientific_domain
#' @export
theme_mechanistic_pathway <- function(...) theme_scientific_domain("mechanistic_pathway", ...)
#' @rdname theme_scientific_domain
#' @export
theme_epidemiology <- function(...) theme_scientific_domain("epidemiology", ...)
#' @rdname theme_scientific_domain
#' @export
theme_clinical_trial <- function(...) theme_scientific_domain("clinical_trial", ...)
#' @rdname theme_scientific_domain
#' @export
theme_systematic_review <- function(...) theme_scientific_domain("systematic_review", ...)
#' @rdname theme_scientific_domain
#' @export
theme_genomics <- function(...) theme_scientific_domain("genomics", ...)
#' @rdname theme_scientific_domain
#' @export
theme_spatial_health <- function(...) theme_scientific_domain("spatial_health", ...)
#' @rdname theme_scientific_domain
#' @export
theme_health_economics <- function(...) theme_scientific_domain("health_economics", ...)
#' @rdname theme_scientific_domain
#' @export
theme_causal_dag <- function(...) theme_scientific_domain("causal_dag", ...)
#' @rdname theme_scientific_domain
#' @export
theme_survival_analysis <- function(...) theme_scientific_domain("survival_analysis", ...)
#' @rdname theme_scientific_domain
#' @export
theme_evidence_grade <- function(...) theme_scientific_domain("evidence_grade", ...)
