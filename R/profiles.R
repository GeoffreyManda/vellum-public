# Design profiles ------------------------------------------------------------

.vellum_profiles <- list(
  restrained = list(
    description = "Quiet journal figure; data and uncertainty dominate.",
    grid = "none", axis_style = "spine", legend = "right",
    density = "comfortable", palette = "okabe_ito"
  ),
  editorial = list(
    description = "Direct, spacious explanatory graphic for reports and features.",
    grid = "y", axis_style = "spine", legend = "bottom",
    density = "airy", palette = "muted"
  ),
  clinical = list(
    description = "Conservative black typography, aligned estimates and explicit intervals.",
    grid = "x", axis_style = "spine", legend = "bottom",
    density = "compact", palette = "high_contrast"
  ),
  technical = list(
    description = "Fine reference grid and compact geometry for dense analytical fields.",
    grid = "XY", axis_style = "frame", legend = "right",
    density = "compact", palette = "bright"
  ),
  presentation = list(
    description = "Large, high-contrast hierarchy for live explanation.",
    grid = "y", axis_style = "spine", legend = "bottom",
    density = "airy", palette = "okabe_ito"
  )
)

#' List Vellum design profiles
#'
#' @return A data frame describing the available semantic profiles.
#' @export
vellum_profiles <- function() {
  data.frame(
    profile = names(.vellum_profiles),
    description = vapply(.vellum_profiles, `[[`, character(1), "description"),
    palette = vapply(.vellum_profiles, `[[`, character(1), "palette"),
    density = vapply(.vellum_profiles, `[[`, character(1), "density"),
    row.names = NULL
  )
}

#' Apply a chart-aware Vellum design profile
#'
#' Profiles are independent of venue: the venue controls physical constraints,
#' while the profile controls hierarchy, density and explanatory mood. All values
#' still flow through [vellum_tokens()] and [theme_vellum()].
#'
#' @param profile Profile name; see [vellum_profiles()].
#' @param venue Target venue.
#' @param palette Optional palette override.
#' @param ... Further arguments overriding the profile defaults in
#'   [theme_vellum()].
#' @return A complete ggplot2 theme.
#' @export
theme_vellum_profile <- function(
    profile = c("restrained", "editorial", "clinical", "technical", "presentation"),
    venue = NULL, palette = NULL, ...) {
  profile <- match.arg(profile)
  cfg <- .vellum_profiles[[profile]]
  venue <- venue %||% if (profile == "presentation") "slide" else "nature"
  args <- list(
    venue = venue,
    grid = cfg$grid,
    axis_style = cfg$axis_style,
    legend = cfg$legend,
    density = cfg$density,
    palette = palette %||% cfg$palette
  )
  dots <- list(...)
  args[names(dots)] <- dots
  th <- do.call(theme_vellum, args)
  attr(th, "vellum_profile") <- profile
  th
}
