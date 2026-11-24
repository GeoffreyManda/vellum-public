# Spatial and spatiotemporal recipes ----------------------------------------

require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort(c(
      "Spatial recipes require the optional package {.pkg sf}.",
      i = "Install it with {.code install.packages('sf')}."
    ))
  }
}

prepare_sf <- function(data, crs = NULL) {
  require_sf()
  if (!inherits(data, "sf")) {
    cli::cli_abort("{.arg data} must be an {.cls sf} object.")
  }
  if (!is.null(crs)) data <- sf::st_transform(data, crs)
  data
}

spatial_theme <- function(venue, profile, graticule) {
  theme_vellum_map(venue = venue, graticule = graticule) +
    theme_vellum_profile(profile = profile, venue = venue) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FBFCFE", colour = NA),
      panel.border = ggplot2::element_rect(
        fill = NA, colour = "#D7DBDE", linewidth = lw(0.25)
      ),
      panel.grid.major = if (graticule) {
        ggplot2::element_line(colour = "#E4E7EA", linewidth = lw(0.25))
      } else {
        ggplot2::element_blank()
      }
    )
}

#' Publication-grade choropleth
#'
#' @description
#' Draws an `sf` choropleth with an explicit projection, quiet boundaries, a
#' perceptual sequential scale and a visible missing-data colour. Supply `crs`
#' whenever geographic area is part of the comparison; an equal-area projection
#' is generally the defensible choice.
#'
#' @param data An `sf` object.
#' @param value Numeric column mapped to fill.
#' @param crs Optional target coordinate reference system accepted by
#'   [sf::st_transform()].
#' @param bins Number of perceptual bins. Use `NULL` for a continuous scale.
#' @param palette Vellum sequential palette.
#' @param legend_title Legend title; defaults to `value`.
#' @param border Polygon-boundary colour.
#' @param graticule Draw a subdued coordinate graticule.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_choropleth <- function(data, value, crs = NULL, bins = 5,
                           palette = "abyss", legend_title = value,
                           border = "#FFFFFF", graticule = FALSE,
                           venue = "nature", profile = "editorial") {
  d <- prepare_sf(data, crs)
  check_string(value, "value")
  if (!value %in% names(d) || !is.numeric(d[[value]])) {
    cli::cli_abort("{.arg value} must name a numeric column in {.arg data}.")
  }
  if (!is.null(bins)) check_number(bins, "bins", min = 2, max = 12)
  d$.value <- d[[value]]
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.value),
      colour = border, linewidth = lw(0.20)
    )
  p <- if (is.null(bins)) {
    p + scale_fill_vellum_c(palette, name = legend_title)
  } else {
    p + scale_fill_vellum_binned(palette, bins = as.integer(bins),
                                 name = legend_title)
  }
  p +
    ggplot2::coord_sf(crs = sf::st_crs(d), datum = sf::st_crs(d),
                      expand = FALSE) +
    spatial_theme(venue, profile, graticule)
}

#' Small-multiple spatiotemporal map
#'
#' @description
#' Uses one common scale across time so change is comparable rather than
#' re-normalised panel by panel. The result is static and submission-safe; animate
#' it downstream only when the delivery medium genuinely supports motion.
#'
#' @param data An `sf` object containing repeated geometries.
#' @param time Column defining ordered time slices.
#' @param value Numeric column mapped to fill.
#' @param crs Optional target coordinate reference system.
#' @param columns Number of facet columns.
#' @param bins Number of common scale bins; `NULL` gives a continuous scale.
#' @param palette,legend_title,venue,profile See [fig_choropleth()].
#' @return A ggplot object.
#' @export
fig_spatiotemporal_map <- function(data, time, value, crs = NULL,
                                   columns = NULL, bins = 5,
                                   palette = "abyss",
                                   legend_title = value,
                                   venue = "nature",
                                   profile = "editorial") {
  d <- prepare_sf(data, crs)
  check_string(time, "time")
  if (!time %in% names(d)) cli::cli_abort("{.arg time} is not a column in {.arg data}.")
  if (length(unique(d[[time]])) > 12) {
    cli::cli_warn("More than 12 time slices will be difficult to compare at publication size.")
  }
  d$.vellum_time <- factor(d[[time]], levels = unique(d[[time]]))
  p <- fig_choropleth(
    d, value = value, bins = bins, palette = palette,
    legend_title = legend_title, venue = venue, profile = profile
  )
  p + ggplot2::facet_wrap(
    stats::as.formula("~.vellum_time"), ncol = columns
  ) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
}

#' Proportional-symbol map
#'
#' @param data An `sf` point or polygon object. Polygon features are represented
#'   using a point guaranteed to lie on each surface.
#' @param size Numeric magnitude column.
#' @param colour Optional grouping column used as a redundant categorical channel.
#' @param crs Optional target coordinate reference system.
#' @param max_size Maximum symbol diameter in millimetres.
#' @param background Optional `sf` polygon layer.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_spatial_symbols <- function(data, size, colour = NULL, crs = NULL,
                                max_size = 7, background = NULL,
                                venue = "nature", profile = "editorial") {
  d <- prepare_sf(data, crs)
  check_string(size, "size")
  if (!size %in% names(d) || !is.numeric(d[[size]])) {
    cli::cli_abort("{.arg size} must name a numeric column in {.arg data}.")
  }
  if (!all(grepl("POINT", as.character(sf::st_geometry_type(d))))) {
    d <- suppressWarnings(sf::st_point_on_surface(d))
  }
  d$.size <- d[[size]]
  d$.group <- factor(if (is.null(colour)) "Magnitude" else d[[colour]])
  if (!is.null(background)) background <- prepare_sf(background, sf::st_crs(d))
  p <- ggplot2::ggplot()
  if (!is.null(background)) {
    p <- p + ggplot2::geom_sf(data = background, fill = "#F1F3F4",
                              colour = "#FFFFFF", linewidth = lw(0.2))
  }
  p +
    ggplot2::geom_sf(
      data = d,
      ggplot2::aes(size = .data$.size, colour = .data$.group,
                   shape = .data$.group),
      alpha = 0.78, stroke = lw(0.45)
    ) +
    ggplot2::scale_size_area(max_size = max_size, name = size) +
    scale_colour_vellum(name = if (is.null(colour)) NULL else colour) +
    scale_shape_vellum(name = if (is.null(colour)) NULL else colour) +
    ggplot2::guides(
      colour = if (is.null(colour)) "none" else "legend",
      shape = if (is.null(colour)) "none" else "legend"
    ) +
    ggplot2::coord_sf(crs = sf::st_crs(d), datum = sf::st_crs(d),
                      expand = FALSE) +
    spatial_theme(venue, profile, FALSE)
}

#' Origin-destination flow map
#'
#' @description
#' Converts origin and destination coordinates into projected `sf` lines,
#' preserving a clear visual hierarchy: geography recedes, flows carry width,
#' and direction is encoded with restrained arrowheads.
#'
#' @param background An `sf` polygon layer.
#' @param flows Data frame containing origin and destination coordinates.
#' @param origin_lon,origin_lat,destination_lon,destination_lat Coordinate columns.
#' @param weight Numeric flow-magnitude column.
#' @param crs Target projection. Defaults to the background CRS.
#' @param venue,profile Vellum destination and design profile.
#' @return A ggplot object.
#' @export
fig_flow_map <- function(background, flows, origin_lon, origin_lat,
                         destination_lon, destination_lat, weight,
                         crs = NULL, venue = "nature",
                         profile = "technical") {
  bg <- prepare_sf(background, crs)
  cols <- c(origin_lon, origin_lat, destination_lon, destination_lat, weight)
  if (!all(cols %in% names(flows))) {
    cli::cli_abort("{.arg flows} is missing one or more coordinate/weight columns.")
  }
  if (!is.numeric(flows[[weight]]) || any(flows[[weight]] < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg weight} must name a non-negative numeric column.")
  }
  complete <- stats::complete.cases(flows[cols])
  flows <- flows[complete, , drop = FALSE]
  if (!nrow(flows)) cli::cli_abort("{.arg flows} has no complete rows to draw.")
  lines <- lapply(seq_len(nrow(flows)), function(i) {
    sf::st_linestring(matrix(c(
      flows[[origin_lon]][i], flows[[origin_lat]][i],
      flows[[destination_lon]][i], flows[[destination_lat]][i]
    ), ncol = 2, byrow = TRUE))
  })
  f <- sf::st_sf(
    .weight = flows[[weight]],
    geometry = sf::st_sfc(lines, crs = 4326)
  )
  f <- sf::st_transform(f, sf::st_crs(bg))
  destinations <- sf::st_as_sf(
    data.frame(
      .weight = flows[[weight]],
      .lon = flows[[destination_lon]],
      .lat = flows[[destination_lat]]
    ),
    coords = c(".lon", ".lat"), crs = 4326
  )
  destinations <- sf::st_transform(destinations, sf::st_crs(bg))
  ggplot2::ggplot(bg) +
    ggplot2::geom_sf(fill = "#F1F3F4", colour = "#FFFFFF",
                     linewidth = lw(0.2)) +
    ggplot2::geom_sf(
      data = f, ggplot2::aes(linewidth = .data$.weight),
      colour = "#0072B2", alpha = 0.72,
      lineend = "round"
    ) +
    ggplot2::geom_sf(
      data = destinations, shape = 24, fill = "#0072B2",
      colour = "#FFFFFF", size = ps(1.8), stroke = lw(0.35)
    ) +
    ggplot2::scale_linewidth_continuous(range = c(lw(0.35), lw(1.8)),
                                        name = weight) +
    ggplot2::coord_sf(crs = sf::st_crs(bg), datum = sf::st_crs(bg),
                      expand = FALSE) +
    spatial_theme(venue, profile, FALSE)
}
