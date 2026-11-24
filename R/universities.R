# University brand stack for Vellum -------------------------------------------
#
# Canonical data live in inst/universities/university_brand_registry.yml.
# This file deliberately treats university identity palettes as role-bearing
# collections, preserving the provenance/accessibility principles used by
# Vellum's existing semantic palette registry.

.university_registry_cache <- new.env(parent = emptyenv())

.university_provenance_map <- c(
  official_public = "official_public",
  official_observed = "official_observed",
  private_inferred = "private_inferred",
  website_inferred = "official_observed"
)

.university_registry_path <- function(path = NULL) {
  if (!is.null(path)) return(path)
  candidate <- system.file(
    "universities", "university_brand_registry.yml", package = "vellum"
  )
  if (!nzchar(candidate)) {
    candidate <- file.path("inst", "universities", "university_brand_registry.yml")
  }
  candidate
}

.validate_university_registry <- function(reg, bundled = TRUE) {
  required_top <- c("schema_version", "generated_on", "selection",
                    "provenance_classes", "global_rules", "profiles")
  missing_top <- setdiff(required_top, names(reg))
  if (length(missing_top)) {
    cli::cli_abort("University registry is missing: {.val {missing_top}}.")
  }
  if (anyDuplicated(names(reg$profiles))) {
    cli::cli_abort("University registry profiles must be uniquely named.")
  }
  # The count is a property of the shipped selection, not of the schema, so a
  # registry supplied through `path` is free to hold any number of profiles.
  if (bundled && length(reg$profiles) != 50L) {
    cli::cli_abort("The bundled university registry must contain 50 profiles.")
  }
  required_roles <- c("primary", "secondary", "accent", "support_1", "support_2",
                      "ink", "muted", "rule", "surface", "background")
  allowed_provenance <- names(.university_provenance_map)
  for (id in names(reg$profiles)) {
    x <- reg$profiles[[id]]
    missing_roles <- setdiff(required_roles, names(x$colours))
    if (length(missing_roles)) {
      cli::cli_abort("University {.val {id}} is missing colour roles: {.val {missing_roles}}.")
    }
    colours <- unlist(x$colours, use.names = FALSE)
    if (any(!grepl("^#[0-9A-Fa-f]{6}$", colours))) {
      cli::cli_abort("University {.val {id}} contains an invalid hexadecimal colour.")
    }
    if (!x$provenance %in% allowed_provenance) {
      cli::cli_abort("University {.val {id}} has unknown provenance {.val {x$provenance}}.")
    }
    if (!is.numeric(x$confidence) || length(x$confidence) != 1L ||
        x$confidence < 0 || x$confidence > 1) {
      cli::cli_abort("University {.val {id}} has invalid confidence.")
    }
    if (isTRUE(x$legal$bundle_logos) || isTRUE(x$legal$autoload_marks)) {
      cli::cli_abort("University {.val {id}} attempts to bundle or autoload protected marks.")
    }
  }
  invisible(reg)
}

#' Read the Vellum university brand registry
#'
#' @param path Optional path to the registry YAML file.
#' @return The parsed registry as a list.
#' @export
vellum_university_registry <- function(path = NULL) {
  check_installed("yaml", "to read the university registry")
  bundled <- is.null(path)
  path <- .university_registry_path(path)
  if (!file.exists(path)) {
    cli::cli_abort("University registry not found at {.path {path}}.")
  }
  path <- normalizePath(path, mustWork = TRUE)
  stamp <- as.character(file.info(path)$mtime)
  key <- paste(path, stamp, sep = "::")
  if (exists(key, envir = .university_registry_cache, inherits = FALSE)) {
    return(get(key, envir = .university_registry_cache, inherits = FALSE))
  }
  reg <- yaml::read_yaml(path)
  .validate_university_registry(reg, bundled = bundled)
  rm(list = ls(.university_registry_cache), envir = .university_registry_cache)
  assign(key, reg, envir = .university_registry_cache)
  reg
}

#' List university brand profiles
#'
#' @param group Optional group: malawi_public, russell_group, or usa_qs2027_top20.
#' @param provenance Optional provenance class.
#' @param path Optional registry path.
#' @return A data frame with one row per profile.
#' @export
vellum_university_brands <- function(group = NULL, provenance = NULL, path = NULL) {
  reg <- vellum_university_registry(path)
  rows <- lapply(names(reg$profiles), function(id) {
    x <- reg$profiles[[id]]
    data.frame(
      id = id,
      name = x$name,
      group = x$group,
      order = as.integer(x$order),
      provenance = x$provenance,
      brand_provenance = unname(.university_provenance_map[[x$provenance]]),
      confidence = as.numeric(x$confidence),
      design_family = x$design_family,
      open_display = x$fonts$open_display,
      open_body = x$fonts$open_body,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (!is.null(group)) out <- out[out$group %in% group, , drop = FALSE]
  if (!is.null(provenance)) {
    out <- out[out$provenance %in% provenance, , drop = FALSE]
  }
  out <- out[order(out$group, out$order), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Retrieve one university brand profile
#'
#' @param id Registry identifier.
#' @param path Optional registry path.
#' @return A named profile list.
#' @export
vellum_university_brand <- function(id, path = NULL) {
  check_string(id, "id")
  if (!nzchar(id)) cli::cli_abort("{.arg id} must not be empty.")
  reg <- vellum_university_registry(path)
  if (!id %in% names(reg$profiles)) {
    cli::cli_abort(c(
      "Unknown university brand {.val {id}}.",
      i = "See {.fn vellum_university_brands} for the registered identifiers."
    ))
  }
  x <- reg$profiles[[id]]
  c(list(id = id,
         source_provenance = x$provenance,
         brand_provenance = unname(.university_provenance_map[[x$provenance]])), x)
}

#' Retrieve a university identity palette by semantic role
#'
#' @param id Registry identifier.
#' @param roles Optional role names, in requested order.
#' @param named Keep role names.
#' @param path Optional registry path.
#' @return Character vector of hex colours.
#' @export
vellum_university_palette <- function(
    id,
    roles = NULL,
    named = TRUE,
    path = NULL
) {
  x <- vellum_university_brand(id, path)
  available <- unlist(x$colours, use.names = TRUE)
  if (is.null(roles)) {
    roles <- names(available)
  }
  unknown <- setdiff(roles, names(available))
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown palette roles: {.val {unknown}}.",
      i = "Available: {.val {names(available)}}."
    ))
  }
  out <- available[roles]
  if (isTRUE(named)) out else unname(out)
}

#' Resolve the open and reference font stacks for a university
#'
#' @param id Registry identifier.
#' @param prefer_reference Return reference fonts before open substitutes.
#' @param path Optional registry path.
#' @return A named character vector.
#' @export
vellum_university_fonts <- function(
    id,
    prefer_reference = FALSE,
    path = NULL
) {
  x <- vellum_university_brand(id, path)
  f <- x$fonts
  if (isTRUE(prefer_reference)) {
    display <- f$reference_display %||% f$open_display
    body <- f$reference_body %||% f$open_body
  } else {
    display <- f$open_display
    body <- f$open_body
  }
  c(display = display, body = body, mono = f$mono)
}

.university_mode_aliases <- c(
  figure = "journal_figure",
  report = "technical_report",
  slide = "executive_slide",
  journal_figure = "journal_figure",
  technical_report = "technical_report",
  policy_brief = "policy_brief",
  executive_slide = "executive_slide",
  dashboard = "dashboard",
  social_card = "social_card"
)


#' Resolve a usable university font family
#'
#' @param id Registry identifier.
#' @param role display, body, or mono.
#' @param path Optional registry path.
#' @return The first installed preferred family, carrying the requested and
#'   candidate font chain as attributes.
#' @export
vellum_university_font_resolve <- function(
    id,
    role = c("display", "body", "mono"),
    path = NULL
) {
  role <- match.arg(role)
  x <- vellum_university_brand(id, path)
  fonts <- vellum_university_fonts(id, prefer_reference = FALSE, path = path)
  preferred <- unname(fonts[[role]])
  generic <- if (role == "mono") "mono" else if (role == "display") "sans" else "sans"
  candidates <- unique(c(preferred, "Noto Sans", "Source Sans 3", "Arial", generic))
  requested <- x$fonts[[paste0("reference_", role)]] %||% preferred
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(structure(preferred %||% generic, requested = requested,
                     candidates = candidates, substituted = !identical(requested, preferred)))
  }
  installed <- unique(systemfonts::system_fonts()$family)
  hit <- candidates[candidates %in% installed]
  resolved <- if (length(hit)) hit[[1L]] else generic
  structure(resolved, requested = requested, candidates = candidates,
            substituted = !identical(requested, resolved), role = role)
}

.university_tokens <- function(id, venue, colour, base_size = NULL,
                               base_family = NULL, density = "comfortable",
                               path = NULL) {
  x <- vellum_university_brand(id, path)
  c <- x$colours
  body <- base_family %||% as.character(vellum_university_font_resolve(id, "body", path))
  display <- as.character(vellum_university_font_resolve(id, "display", path))
  tk <- vellum_tokens(
    venue = venue, base_size = base_size, family = device_safe_family(body),
    ink = c$ink, accent = c$primary, palette = "okabe_ito", density = density
  )
  tk$ink$text <- c$ink
  tk$ink$axis <- c$ink
  tk$ink$tick <- c$ink
  tk$ink$data <- c$ink
  tk$ink$weak <- c$muted
  tk$ink$paper <- c$background
  tk$ink$panel <- c$surface
  tk$ink$grid <- c$rule
  tk$ink$grid_minor <- grDevices::adjustcolor(c$rule, alpha.f = 0.55)
  tk$ink$na <- c$rule
  tk$type$family_display <- device_safe_family(display)
  tk$meta$university <- id
  tk$meta$university_label <- x$name
  tk$meta$university_source_provenance <- x$source_provenance
  tk$meta$university_brand_provenance <- x$brand_provenance
  tk$meta$university_confidence <- x$confidence
  tk$meta$university_colour_mode <- colour
  tk
}

.university_palette_elements <- function(id, colour, path = NULL) {
  identity_values <- vellum_university_palette(
    id, c("primary", "secondary", "accent", "support_1", "support_2"),
    named = FALSE, path = path
  )
  values <- if (colour == "accessible") .identity_accessible else identity_values
  discrete <- function(n) {
    if (n > length(values)) {
      cli::cli_abort(c(
        "University palette provides {length(values)} colours; {n} were requested.",
        i = "Vellum never silently recycles identity colours."
      ))
    }
    unname(values[seq_len(n)])
  }
  list(palette.colour.discrete = discrete, palette.fill.discrete = discrete)
}

#' Apply a university profile to a ggplot
#'
#' @param id Registry identifier.
#' @param mode One of Vellum's six [brand_modes()] or the compatibility aliases
#'   `"figure"`, `"report"`, or `"slide"`.
#' @param colour `"brand"` or `"accessible"`; defaults by output mode.
#' @param base_size Optional body size overriding the mode default.
#' @param base_family Optional font overriding the open body font.
#' @param path Optional registry path.
#' @param ... Additional [ggplot2::theme()] elements.
#' @return A complete ggplot2 theme.
#' @export
theme_vellum_university <- function(
    id,
    mode = names(.university_mode_aliases),
    colour = NULL,
    base_size = NULL,
    base_family = NULL,
    path = NULL,
    ...
) {
  mode <- match.arg(mode)
  x <- vellum_university_brand(id, path)
  compiler_mode <- unname(.university_mode_aliases[[mode]])
  cfg <- .brand_modes[[compiler_mode]]
  colour <- match.arg(colour %||% cfg$colour, c("brand", "accessible"))
  tk <- .university_tokens(
    id, venue = cfg$venue, colour = colour, base_size = base_size,
    base_family = base_family, density = cfg$density, path = path
  )
  th <- theme_vellum(
    tokens = tk, grid = cfg$grid, axis_style = cfg$axis_style,
    ticks = "out", legend = .vellum_profiles[[cfg$profile]]$legend
  )
  th <- th + do.call(ggplot2::theme, .university_palette_elements(id, colour, path))
  th <- th + ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = tk$type$family_display, colour = x$colours$primary, face = "bold"
    ),
    strip.text = ggplot2::element_text(
      family = tk$type$family_display, colour = x$colours$secondary, face = "bold"
    ),
    legend.title = ggplot2::element_text(family = tk$type$family_display)
  )
  extra <- list(...)
  if (length(extra)) th <- th + do.call(ggplot2::theme, extra)
  attr(th, "vellum_tokens") <- tk
  attr(th, "vellum_university") <- list(
    id = id, name = x$name, mode = compiler_mode, colour = colour,
    source_provenance = x$source_provenance,
    brand_provenance = x$brand_provenance, confidence = x$confidence
  )
  th
}

#' Discrete university colour scale
#'
#' @param id Registry identifier.
#' @param roles Ordered identity roles. Defaults to five display roles.
#' @param ... Passed to ggplot2::discrete_scale().
#' @param path Optional registry path.
#' @export
scale_colour_university_d <- function(
    id,
    roles = c("primary", "secondary", "accent", "support_1", "support_2"),
    ...,
    path = NULL
) {
  values <- vellum_university_palette(id, roles, named = FALSE, path = path)
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) {
      if (n > length(values)) {
        cli::cli_abort(c(
          "The {.val {id}} palette provides {length(values)} display colours; {n} were requested.",
          i = "Aggregate categories, or supply explicit semantic colours."
        ))
      }
      values[seq_len(n)]
    },
    ...
  )
}

#' @rdname scale_colour_university_d
#' @export
scale_color_university_d <- scale_colour_university_d

#' Discrete university fill scale
#' @inheritParams scale_colour_university_d
#' @export
scale_fill_university_d <- function(
    id,
    roles = c("primary", "secondary", "accent", "support_1", "support_2"),
    ...,
    path = NULL
) {
  values <- vellum_university_palette(id, roles, named = FALSE, path = path)
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) {
      if (n > length(values)) {
        cli::cli_abort(c(
          "The {.val {id}} palette provides {length(values)} display colours; {n} were requested.",
          i = "Aggregate categories, or supply explicit semantic colours."
        ))
      }
      values[seq_len(n)]
    },
    ...
  )
}

#' Register university profiles in Vellum's role-collection registry
#'
#' @description
#' Adds one brand-specific role collection per university to an existing
#' `.vellum_role_collections` object. This preserves the user's existing
#' role-first architecture instead of flattening universities into anonymous
#' colour bags.
#'
#' @param collections Existing collection list.
#' @param path Optional registry path.
#' @return The augmented collection list.
#' @export
vellum_register_university_palettes <- function(
    collections = get0(".vellum_role_collections", inherits = TRUE),
    path = NULL
) {
  collections <- collections %||% list()
  reg <- vellum_university_registry(path)
  for (id in names(reg$profiles)) {
    x <- reg$profiles[[id]]
    key <- paste0("university_", id)
    collections[[key]] <- list(
      colours = unlist(x$colours, use.names = TRUE),
      display_roles = c("primary", "secondary", "accent", "support_1", "support_2"),
      source = paste0(x$name, " university identity profile; ", x$provenance),
      intended_use = paste0(x$name, "-facing figures, reports, slides and dashboards"),
      scope = "brand-specific",
      provenance = x$provenance,
      brand_provenance = unname(.university_provenance_map[[x$provenance]]),
      confidence = x$confidence,
      accessibility = paste0(
        "Identity palette only. Preserve labels and redundant encodings; ",
        "run contrast and colour-vision checks. Confidence: ", x$confidence, "."
      )
    )
  }
  collections
}

#' Audit one university brand profile
#'
#' @param id Registry identifier.
#' @param path Optional registry path.
#' @return A compact audit data frame.
#' @export
vellum_university_audit <- function(id, path = NULL) {
  x <- vellum_university_brand(id, path)
  display <- vellum_university_palette(
    id, c("primary", "secondary", "accent", "support_1", "support_2"),
    named = FALSE, path = path
  )
  colour_audit <- palette_audit(display, type = "qualitative")
  text_contrast <- contrast_ratio(x$colours$ink, x$colours$background)
  data.frame(
    id = id,
    name = x$name,
    provenance = x$source_provenance,
    brand_provenance = x$brand_provenance,
    confidence = x$confidence,
    verified_on = x$verified_on,
    source_count = length(x$sources),
    text_contrast = text_contrast,
    text_contrast_pass = text_contrast >= 4.5,
    palette_verdict = colour_audit$verdict,
    min_deuteranopia = unname(colour_audit$separation[["deuteranopia"]]),
    proprietary_display_font = !is.null(x$fonts$reference_display),
    proprietary_body_font = !is.null(x$fonts$reference_body),
    logos_bundled = isTRUE(x$legal$bundle_logos),
    external_review_recommended = x$source_provenance %in% c("private_inferred", "website_inferred"),
    stringsAsFactors = FALSE
  )
}

# Add packaged university profiles to the role-bearing palette registry. This is
# deliberately an identity collection, not a semantic outcome scale.
.vellum_role_collections <- vellum_register_university_palettes(
  collections = .vellum_role_collections
)
# Do not serialize the installation-time registry path into the package lazy-load
# database. Runtime calls repopulate the cache from the installed location.
rm(list = ls(.university_registry_cache), envir = .university_registry_cache)
