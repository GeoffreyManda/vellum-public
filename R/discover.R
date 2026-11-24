# Discoverability.
#
# A package with ninety exported functions is unusable to a newcomer who cannot guess
# any of their names. Three mechanisms help, in increasing order of how much they
# actually get used:
#
#   1. Choices in the signature. An editor completes from `formals()`, so an argument
#      declared `c("nature", "science", ...)` offers its options on Tab while one
#      declared `"nature"` offers nothing. This is why `vellum_venues_supported()` and
#      friends exist and why every error message lists the valid values.
#   2. Editor snippets. Cursor and VS Code read `*.code-snippets`; `vellum_snippets()`
#      writes them, so typing `vfig` expands to a working figure skeleton.
#   3. A printed map. `vellum_cheatsheet()` is what a newcomer reads once.

#' Valid values, for autocompletion and for guessing
#'
#' @description
#' Character vectors of everything a `vellum` argument will accept. Editors complete
#' from these, and they are what the package's own error messages list, so a wrong
#' value always tells you the right ones.
#'
#' @return A character vector.
#' @export
#' @examples
#' vellum_venues_supported()
#' vellum_palettes_supported("diverging")
#' vellum_formats_supported()
vellum_venues_supported <- function() sort(venues()$venue)

#' @param type Optionally filter to `"qualitative"`, `"sequential"`, `"diverging"`,
#'   or `"cyclic"`.
#' @param family Optionally filter to a source family; see [vellum_palettes()].
#' @rdname vellum_venues_supported
#' @export
vellum_palettes_supported <- function(type = NULL, family = NULL) {
  p <- vellum_palettes(type, family)
  sort(p$palette)
}

#' @rdname vellum_venues_supported
#' @export
vellum_formats_supported <- function() names(.devices)

#' @rdname vellum_venues_supported
#' @export
vellum_themes_supported <- function() {
  c("theme_vellum", "theme_venue", "theme_vellum_grid", "theme_vellum_frame",
    "theme_vellum_prism", "theme_vellum_void", "theme_vellum_map",
    "theme_vellum_dark", "theme_vellum_slide", "theme_vellum_poster")
}

#' A printed map of the package
#'
#' @description
#' The one-screen orientation for someone who has just installed this and does not know
#' what to type. Prints the minimal workflow, then the functions grouped by the job they
#' do, then the values every argument accepts.
#'
#' @param section Optionally print one section only: `"workflow"`, `"themes"`,
#'   `"colour"`, `"scales"`, `"geoms"`, `"recipes"`, `"geometry"`, `"typography"`,
#'   or `"values"`.
#' @return Invisibly, the sections printed.
#' @export
#' @examples
#' vellum_cheatsheet("workflow")
vellum_cheatsheet <- function(section = NULL) {
  secs <- c("workflow", "themes", "colour", "scales", "geoms", "recipes",
            "geometry", "typography", "values")
  if (!is.null(section)) {
    section <- match.arg(section, secs, several.ok = TRUE)
    secs <- section
  }

  bullet <- function(...) cli::cli_bullets(c(...))

  if ("workflow" %in% secs) {
    cli::cli_h1("vellum: the four-line workflow")
    cli::cli_code(
      'p <- ggplot(d, aes(x, y, colour = g)) + geom_point() +',
      '  theme_venue("nature")               # venue type sizes, audited palette',
      'p <- fit_width(p, "nature", "single") # exactly 89 mm wide',
      'venue_check(p, "nature")              # pre-flight compliance report',
      'save_figure(p, "figure-1", venue = "nature")'
    )
  }
  if ("themes" %in% secs) {
    cli::cli_h2("Themes")
    bullet(
      "*" = "{.fn theme_venue} target a venue: {.code theme_venue('cell')}",
      "*" = "{.fn theme_vellum} the base; {.arg grid} takes {.val none}/{.val x}/{.val y}/{.val both} or flags like {.val Yy}",
      "*" = "variants: {.fn theme_vellum_grid} {.fn theme_vellum_frame} {.fn theme_vellum_prism} {.fn theme_vellum_void} {.fn theme_vellum_map} {.fn theme_vellum_dark} {.fn theme_vellum_slide} {.fn theme_vellum_poster}",
      "*" = "{.fn vellum_set} make it the session default; {.fn vellum_tokens} inspect every constant",
      "*" = "{.fn theme_coverage} which elements the theme actually sets"
    )
  }
  if ("colour" %in% secs) {
    cli::cli_h2("Colour")
    bullet(
      "*" = "{.fn vellum_palettes} list them; {.fn vellum_pal} get colours",
      "*" = "{.fn palette_audit} measure a palette; {.fn palette_compare} see it as five readers do",
      "*" = "{.fn palette_max_safe} how many of a palette are safe; {.fn palette_optimise} build one for any n",
      "*" = "{.fn vellum_ramp} {.fn ramp_through} build ramps; {.fn cvd} {.fn delta_e} {.fn contrast_ratio} measure them",
      "*" = "{.fn emphasise} one accent, rest grey; {.fn vellum_shapes} {.fn vellum_linetypes} redundant channels"
    )
  }
  if ("scales" %in% secs) {
    cli::cli_h2("Scales")
    bullet(
      "*" = "discrete {.fn scale_colour_vellum} {.fn scale_fill_vellum}",
      "*" = "continuous {.fn scale_colour_vellum_c} {.fn scale_fill_vellum_c}",
      "*" = "diverging, anchored {.fn scale_fill_vellum_div} \u2014 always give {.arg midpoint}",
      "*" = "binned {.fn scale_fill_vellum_binned}; emphasis {.fn scale_colour_emphasis}",
      "*" = "redundant {.fn scale_shape_vellum} {.fn scale_linetype_vellum}",
      "*" = "position {.fn scale_x_vellum} {.fn scale_y_vellum}"
    )
  }
  if ("geoms" %in% secs) {
    cli::cli_h2("Marks and annotation")
    bullet(
      "*" = "{.fn geom_pointinterval} estimate with nested intervals \u2014 not a bar of means",
      "*" = "{.fn geom_rangeframe} Tufte range frame; {.fn annotate_null} reference line",
      "*" = "{.fn annotate_signif} bracket with an exact P; {.fn annotate_scalebar} for images and maps",
      "*" = "{.fn label_series} direct labels at line ends, instead of a legend"
    )
  }
  if ("recipes" %in% secs) {
    cli::cli_h2("One-call figures")
    bullet(
      "*" = "{.fn fig_dotstrip} groups; {.fn fig_raincloud} distributions; {.fn fig_forest} estimates",
      "*" = "{.fn fig_heatmap} matrices; {.fn fig_waffle} part-to-whole; {.fn fig_donut} (see its note)",
      "*" = "{.fn fig_surface3d} projected surface, when shape is the finding",
      "*" = "{.fn fig_sankey} flows; {.fn fig_marimekko} two-way composition; {.fn fig_stacked_area} composition over time",
      "*" = "{.fn fig_ridgeline} many distributions; {.fn fig_parallel} {.fn fig_radar} profiles; {.fn fig_horizon} dense series",
      "*" = "{.fn fig_ce_plane} {.fn fig_ceac} {.fn fig_evpi} {.fn fig_decision_curve} decision analysis",
      "*" = "{.fn fig_gosh} {.fn fig_rob_matrix} evidence structure; {.fn fig_manhattan} genome-wide association",
      "*" = "{.fn superiority_panel} the bar-of-means demonstration",
      "i" = "{.fn chart_catalogue} lists all {nrow(chart_catalogue())} forms; {.fn recommend_plot} picks one from a question"
    )
  }
  if ("geometry" %in% secs) {
    cli::cli_h2("Geometry and export")
    bullet(
      "*" = "{.fn fit_width} exact total size; {.fn set_panel_area} exact panel size",
      "*" = "{.fn figure_dim} measure; {.fn bank_aspect} {.fn bank_height} bank slopes to 45 degrees",
      "*" = "{.fn compose_figure} multi-panel with venue-correct tags",
      "*" = "{.fn save_figure} {.fn save_figure_set} write at exact size",
      "*" = "{.fn venue_check} pre-flight; {.fn venue_spec} {.fn venue_register} the specification"
    )
  }
  if ("typography" %in% secs) {
    cli::cli_h2("Typography")
    bullet(
      "*" = "{.fn label_vellum} true minus, SI digit grouping; {.fn label_si} units",
      "*" = "{.fn label_pval} exact P values; {.fn label_log} typeset exponents",
      "*" = "{.fn fmt_estimate} estimate with interval; {.fn italicise} binomials; {.fn label_wrap} long labels",
      "*" = "{.fn lw} {.fn fs} {.fn ps} points and millimetres to ggplot2 units"
    )
  }
  if ("values" %in% secs) {
    cli::cli_h2("What arguments accept")
    cli::cli_text("{.strong venue} {.val {vellum_venues_supported()}}")
    sci <- c("okabe_ito", "tol", "vellum", "collection")
    cli::cli_text("{.strong palette, qualitative} {.val {vellum_palettes_supported('qualitative', sci)}}")
    cli::cli_text("{.strong palette, sequential} {.val {vellum_palettes_supported('sequential', sci)}}")
    cli::cli_text("{.strong palette, diverging} {.val {vellum_palettes_supported('diverging', sci)}}")
    cli::cli_text("{.strong format} {.val {vellum_formats_supported()}}")
    cli::cli_bullets(c(
      i = "Only {.val nature} ships a verified specification; see {.fn venue_spec}.",
      # The art palettes are listed apart rather than merged in, because a list
      # that mixes them with Okabe-Ito reads as a recommendation that they are
      # interchangeable with it, and they are not.
      i = "{length(vellum_palettes_supported(family = 'ltc'))} further art palettes: {.code vellum_palettes(family = 'ltc')}."
    ))
  }
  invisible(secs)
}

#' Write editor snippets for Cursor, VS Code, or Positron
#'
#' @description
#' Generates a `*.code-snippets` file so that typing a short prefix expands to a working
#' `vellum` skeleton with the arguments already in place. This is the mechanism that
#' actually helps someone who does not yet know the function names: `vfig` produces the
#' whole four-line workflow, `vaudit` produces a palette audit, and tab moves between
#' the placeholders.
#'
#' Editors pick these up automatically from a project's `.vscode/` directory. Restart or
#' reload the window after writing them for the first time.
#'
#' @param dir Directory to write into. The default writes to `.vscode/` in the current
#'   project, which is where Cursor, VS Code, and Positron look.
#' @param file File name.
#' @param overwrite Overwrite an existing file.
#'
#' @return Invisibly, the path written, or the JSON as a string when `dir = NULL`.
#' @export
#' @examples
#' # Get the JSON without writing anything:
#' cat(substr(vellum_snippets(dir = NULL), 1, 200))
vellum_snippets <- function(dir = ".vscode", file = "vellum.code-snippets",
                            overwrite = FALSE) {
  # Snippet bodies use single-quoted R strings with plain double quotes inside, and are
  # escaped once at JSON-generation time. Embedding backslash escapes in the literals
  # instead is how this file failed to parse the first time.
  snip <- list(
    list('vellum figure', 'vfig', c(
      'p <- ggplot(${1:data}, aes(${2:x}, ${3:y})) +',
      '  geom_point() +',
      '  labs(x = "${4:x label}", y = "${5:y label}") +',
      '  theme_venue("${6|nature,science,cell,pnas,plos,elife,thesis,slide,poster|}")',
      '',
      'p <- fit_width(p, "${6}", "${7|single,onehalf,double|}")',
      'venue_check(p, "${6}")',
      'save_figure(p, "${8:figure-1}", venue = "${6}")$0'),
      'Full vellum workflow: theme, exact size, pre-flight, export'),
    list('vellum theme', 'vtheme', c(
      'theme_venue("${1|nature,science,cell,pnas,plos,elife,thesis,slide,poster|}", grid = "${2|none,x,y,both,Yy,XYxy|}", legend = "${3|right,top,bottom,left,none|}")$0'),
      'A venue theme with grid and legend set'),
    list('vellum palette audit', 'vaudit', c(
      'palette_audit("${1|okabe_ito,bright,muted,high_contrast,flow_semantic,flow_jama,flow_lancet,structured_domains,structured_severity,atni,cameo_monochrome,abyss,ember,verdant,terra,bloom,ink,strata,tide,equinox,polarity|}", n = ${2:9})$0'),
      'Measure a palette under colour-vision deficiency and greyscale'),
    list('vellum palette compare', 'vcompare', c(
      'palette_compare(',
      '  `${1:name}` = ${2:c("#F8766D", "#00BA38", "#619CFF")},',
      '  `Okabe-Ito` = okabe_ito(${3:3})',
      ')$0'),
      'Show palettes as five readers see them'),
    list('vellum forest plot', 'vforest', c(
      'fig_forest(${1:data}, estimate = "${2:est}", lower = "${3:lo}", upper = "${4:hi}",',
      '           label = "${5:study}", null = ${6|1,0|}, xlab = "${7:Risk ratio}")$0'),
      'Forest plot of effect estimates'),
    list('vellum group comparison', 'vdotstrip', c(
      'fig_dotstrip(${1:data}, x = "${2:group}", y = "${3:outcome}",',
      '             comparisons = list(c("${4:A}", "${5:B}")), p_values = ${6:0.01})$0'),
      'Observations, mean, nested intervals, exact P'),
    list('vellum emphasis', 'vemph', c(
      'scale_colour_emphasis("${1:series to highlight}") +',
      '  label_series(${2:data}, "${3:x}", "${4:y}", "${5:group}")$0'),
      'One accent, rest muted, direct labels, no legend'),
    list('vellum banked line chart', 'vbank', c(
      'p <- ggplot(${1:data}, aes(${2:x}, ${3:y}, colour = ${4:group})) +',
      '  geom_line() + theme_venue("nature")',
      'h <- bank_height(p, ${1}, "${2}", "${3}", group = "${4}", venue = "nature")',
      'p <- fit_width(p, "nature", "single", height = h)$0'),
      'Line chart banked so the median slope is 45 degrees'),
    list('vellum save set', 'vsaveset', c(
      'save_figure_set(${1:plot}, "${2:figure-1}", venues = c("nature", "slide", "poster"))$0'),
      'Export the same figure for several destinations')
  )

  esc <- function(x) gsub('"', '\\"', gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE)
  body_json <- function(lines) paste0("[", paste0('"', esc(lines), '"', collapse = ", "), "]")
  entries <- vapply(snip, function(s) {
    sprintf('  "%s": {\n    "prefix": "%s",\n    "body": %s,\n    "description": "%s"\n  }',
            s[[1]], s[[2]], body_json(s[[3]]), esc(s[[4]]))
  }, character(1))
  json <- paste0("{\n", paste(entries, collapse = ",\n"), "\n}\n")

  if (is.null(dir)) return(invisible(json))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, file)
  if (file.exists(path) && !overwrite) {
    cli::cli_abort(c("{.path {path}} already exists.",
                     i = "Pass {.code overwrite = TRUE} to replace it."))
  }
  writeLines(json, path)
  cli::cli_inform(c(
    "v" = "Wrote {length(snip)} snippets to {.path {path}}.",
    i = "Reload the editor window, then type {.val vfig} and press Tab.",
    i = "Prefixes: {.val {vapply(snip, `[[`, character(1), 2)}}."
  ))
  invisible(path)
}
