#' vellum: publication-grade scientific figures with executable venue specifications
#'
#' @description
#' A figure destined for a journal is a physical artefact with a published
#' specification. `vellum` treats that specification as executable rather than
#' aspirational, and organises everything else around it.
#'
#' @section The four things it does that a theme collection cannot:
#'
#' **Venue geometry as data.** [venues()] holds each journal's artwork
#' specification with its provenance: the source it came from, and whether that
#' source has been read directly. Nature and PLOS ship verified against current
#' publisher guidance; unverified journal entries identify themselves as working
#' defaults.
#' [venue_register()] lets you correct any of them permanently.
#'
#' **Exact physical size.** [fit_width()] measures a plot's decoration and sets its
#' panel area so the *whole figure* is exactly 89 mm, or 183 mm, or whatever the
#' venue asks. Setting a device to 89 mm does not do this: it gives an 89 mm canvas
#' that the plot then stretches to fill.
#'
#' **Colour that can be audited.** [vellum_ramp()] builds ramps in a perceptually
#' uniform space, linearises lightness, and gamut-maps by reducing chroma at fixed
#' lightness and hue. [palette_audit()] then measures what the palette actually
#' achieves under normal vision, all three dichromacies, and greyscale, and reports
#' numbers.
#'
#' **Compliance before submission.** [venue_check()] reads a plot's resolved theme
#' and layers and checks them against the venue: text sizes within range, rule
#' weights above the print floor, typeface permitted and installed, panel labels in
#' the right style, size within the column and depth limits. A default ggplot2
#' figure fails Nature on every text element, and the report says which and by how
#' much.
#'
#' @section A minimal workflow:
#' ```r
#' library(ggplot2)
#' library(vellum)
#'
#' p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   labs(x = "Kerb weight (1000 lb)", y = "Fuel economy (mpg)", colour = "Cylinders") +
#'   theme_venue("nature")          # 7 pt Helvetica, audited palette, print weights
#'
#' p <- fit_width(p, "nature", "single")   # exactly 89 mm wide
#' venue_check(p, "nature")                # pre-flight report
#' save_figure(p, "figure-1", venue = "nature")
#' ```
#'
#' @section Where to look next:
#' * `vignette("vellum")` for the reasoning behind each decision.
#' * [theme_vellum()] and its variants for the theme layer.
#' * [vellum_palettes()] for the palette library, and [palette_compare()] to see
#'   any of them as five different readers do.
#' * [fig_dotstrip()], [fig_forest()], [fig_raincloud()], [fig_waffle()],
#'   [fig_heatmap()] for one-call recipes.
#' * [superiority_panel()] for the demonstration that a bar of means hides the data.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||%
#' @importFrom ggplot2 ggplot aes theme element_text element_line element_rect element_blank element_geom
#' @importFrom ggplot2 margin unit labs geom_point geom_line geom_tile geom_raster geom_text geom_rect
#' @importFrom ggplot2 geom_violin geom_boxplot geom_col geom_errorbar geom_linerange geom_vline geom_hline
#' @importFrom ggplot2 annotate facet_wrap facet_grid vars coord_polar coord_equal coord_flip coord_cartesian
#' @importFrom ggplot2 scale_fill_identity scale_x_discrete scale_y_discrete scale_x_log10 scale_size_area
#' @importFrom ggplot2 expansion expand_limits xlim rel ggproto Geom draw_key_blank from_theme
#' @importFrom ggplot2 position_jitter position_nudge stat_summary discrete_scale continuous_scale binned_scale
#' @importFrom ggplot2 complete_theme calc_element ggplot_build ggplot_gtable theme_get theme_set
#' @importFrom ggplot2 scale_y_reverse scale_x_continuous scale_y_continuous layer
NULL
