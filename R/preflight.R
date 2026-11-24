# Pre-flight compliance checking.
#
# This is the point of the package. Every other tool makes a figure that looks
# like a journal figure. This answers a different question: does this plot satisfy
# the specification I have to submit against?
#
# The check reads the plot's fully resolved theme through ggplot2's own
# calc_element(), so it sees the sizes that will actually be drawn, including
# inherited and rel() sizes. It reports the measured value against the required
# one in every case, because "axis text too small" is not actionable and
# "axis.text.x is 4.4 pt, below Nature's 5 pt minimum" is.

# Theme elements that render text, with the role each plays in a venue's rules.
.text_elements <- c(
  "axis.text.x", "axis.text.y", "axis.title.x", "axis.title.y",
  "legend.text", "legend.title", "strip.text", "plot.title", "plot.subtitle",
  "plot.caption"
)

# Theme elements that render rules.
.line_elements <- c(
  "axis.line.x", "axis.line.y", "axis.ticks.x", "axis.ticks.y",
  "panel.grid.major.x", "panel.grid.major.y",
  "panel.grid.minor.x", "panel.grid.minor.y", "panel.border"
)

# Collect every theme that will actually be drawn, and say what kind each one is.
#
# A patchwork IS a ggplot -- its own `$theme` is the first panel's -- and the remaining
# panels live in `$patches$plots`, any of which may itself be a patchwork. Reading only
# the annotation theme, as this did at first, silently reported theme_grey() for a
# composed figure whose panels were all themed: a compliance checker giving the wrong
# answer on the commonest journal case, a multi-panel plate.
#
# The kinds have to be distinguished. A "panel" theme is what a plot is drawn with, so
# it is completed against the session default and judged in full. A "container" theme
# belongs to a patchwork or its annotation and only MODIFIES a few elements (the tag,
# the caption); completing one would invent 11 pt axis text that is never drawn, so only
# what it actually sets is judged.
resolved_themes <- function(plot) {
  entry <- function(x, kind) {
    t <- (tryCatch(x, error = function(e) NULL)) %||% ggplot2::theme()
    set <- names(t)[!vapply(t, is.null, logical(1))]
    list(complete = ggplot2::complete_theme(t), set = set, kind = kind)
  }
  gather <- function(x, depth = 0L) {
    if (depth > 10L || !inherits(x, "patchwork")) {
      return(list(entry(x$theme, "panel")))
    }
    kids <- tryCatch(x$patches$plots, error = function(e) list()) %||% list()
    own <- entry(x$theme, "container")
    # patchwork keeps one panel's theme on the container, so if it carries real text
    # elements it IS a panel and must be judged in full. Only an emptied container --
    # which is what plot_annotation() leaves behind -- is a true container.
    if (any(.text_elements %in% own$set)) own$kind <- "panel"
    c(list(own),
      unlist(lapply(kids, gather, depth = depth + 1L), recursive = FALSE),
      list(entry(x$patches$annotation$theme, "container")))
  }
  out <- gather(plot)
  names(out) <- if (length(out) == 1L) "plot" else paste("panel", seq_along(out))
  out
}

resolved_theme <- function(plot) resolved_themes(plot)[[1]]$complete

# Is this element, or anything it inherits from, explicitly set?
#
# theme_vellum() sets `axis.line` and `axis.ticks`, not their four positional variants,
# because the variants are meant to inherit. Testing only for a literal match therefore
# concluded that a fully specified theme drew no rules at all.
explicit_or_inherited <- function(element, set, tree) {
  seen <- character()
  cur <- element
  while (length(cur) == 1L && nzchar(cur) && !cur %in% seen) {
    if (cur %in% set) return(TRUE)
    seen <- c(seen, cur)
    cur <- tree[[cur]]$inherit %||% ""
  }
  FALSE
}

# Which elements of a theme entry should be judged.
judged_elements <- function(entry, elements, tree) {
  if (identical(entry$kind, "panel")) return(elements)
  elements[vapply(elements, explicit_or_inherited, logical(1),
                  set = entry$set, tree = tree)]
}

element_or_null <- function(name, theme) {
  el <- tryCatch(ggplot2::calc_element(name, theme), error = function(e) NULL)
  if (is.null(el) || inherits(el, "element_blank")) return(NULL)
  el
}

# Does this plot draw raster output, which brings a dpi requirement with it?
has_raster_layer <- function(plot) {
  layers <- tryCatch(plot$layers, error = function(e) NULL)
  if (!length(layers)) return(FALSE)
  cls <- unlist(lapply(layers, function(l) class(l$geom)))
  any(grepl("Raster|Tile|Bin2d|Hex", cls))
}

#' Check a figure against a venue's artwork specification
#'
#' @description
#' A pre-flight report. Reads the plot's fully resolved theme and layers and checks
#' them against the target venue's published requirements: text sizes within the
#' permitted range, rule weights at or above the minimum, typeface among those
#' allowed, panel-label style and size, physical size within the column and depth
#' limits, resolution where a raster layer is present, and whether a title is
#' sitting on the plot where the caption should carry it.
#'
#' Each result reports the measured value alongside the requirement. A `"fail"` is
#' something the venue's own document says is not acceptable; a `"warn"` is a
#' judgement this package makes, and the reasoning is given so it can be argued
#' with.
#'
#' @param plot A `ggplot` or `patchwork` object.
#' @param venue Target venue. See [venues()].
#' @param column Column name or a width in millimetres, used when `width` is not
#'   given.
#' @param width,height Intended physical size in millimetres. If the plot came from
#'   [fit_width()] its fitted size is used automatically.
#' @param content `"line"`, `"combination"`, or `"halftone"`, selecting the dpi
#'   requirement.
#' @param quiet Return the report without printing it.
#'
#' @return An object of class `vellum_preflight`, invisibly when `quiet = TRUE`.
#'
#' @examples
#' library(ggplot2)
#'
#' # A default ggplot2 figure fails Nature on every text element: base_size 11
#' # against a 7 pt ceiling.
#' venue_check(ggplot(mtcars, aes(wt, mpg)) + geom_point(), "nature")
#'
#' # Built for the venue, it passes.
#' venue_check(ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature"),
#'             "nature")
#' @export
venue_check <- function(plot, venue = "nature", column = "single",
                        width = NULL, height = NULL,
                        content = c("line", "combination", "halftone"),
                        quiet = FALSE) {
  content <- match.arg(content)
  spec <- venue_spec(venue)
  warn_unverified(spec)
  entries <- resolved_themes(plot)
  themes <- lapply(entries, `[[`, "complete")
  multi <- length(entries) > 1L
  th <- themes[[1]]
  el_tree <- ggplot2::get_element_tree()

  # Label an offending element with its panel only when there is more than one, so the
  # single-panel report stays as terse as it was.
  tag_panel <- function(panel, element) if (multi) paste0(panel, " ", element) else element

  gather_num <- function(elements, extract) {
    rows <- lapply(names(entries), function(nm) {
      t <- themes[[nm]]
      elements <- judged_elements(entries[[nm]], elements, el_tree)
      if (!length(elements)) return(NULL)
      v <- vapply(elements, function(e) {
        el <- element_or_null(e, t)
        val <- if (is.null(el)) NULL else extract(el)
        if (is.null(val) || !is.numeric(val) || !length(val)) NA_real_ else as.numeric(val[1])
      }, numeric(1))
      data.frame(panel = nm, element = elements, value = unname(v),
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
    if (is.null(out)) return(data.frame(panel = character(), element = character(),
                                        value = numeric(), label = character()))
    out <- out[!is.na(out$value), , drop = FALSE]
    out$label <- mapply(tag_panel, out$panel, out$element, USE.NAMES = FALSE)
    out
  }

  fit <- attr(plot, "vellum_fit")
  if (is.null(width) && !is.null(fit)) width <- fit$achieved[["width"]]
  if (is.null(height) && !is.null(fit)) height <- fit$achieved[["height"]]
  width <- width %||% venue_width(venue, column)
  height <- height %||% (width * 0.72)

  checks <- list()
  add <- function(id, label, verdict, detail) {
    checks[[id]] <<- list(id = id, label = label, verdict = verdict, detail = detail)
  }

  # -- text sizes -------------------------------------------------------------
  size_tbl <- gather_num(.text_elements, function(el) el$size)
  if (!nrow(size_tbl)) {
    add("text_size", "Text sizes", "warn", "no text elements could be resolved from the theme")
  } else {
    v <- stats::setNames(size_tbl$value, size_tbl$label)
    # Panel labels are exempt from the body-text ceiling: Nature specifies 7 pt
    # maximum for "all other text" and 8 pt for the labels themselves.
    too_small <- v[v < spec$text_min]
    too_big <- v[v > spec$text_max]
    if (length(too_small)) {
      add("text_min", "Minimum text size", "fail", sprintf(
        "%s below the %g pt minimum: %s",
        if (length(too_small) == 1) "1 element is" else paste(length(too_small), "elements are"),
        spec$text_min,
        paste(sprintf("%s %.2f pt", names(too_small), too_small), collapse = ", ")))
    } else {
      add("text_min", "Minimum text size", "pass", sprintf(
        "smallest element is %s at %.2f pt (minimum %g pt)",
        names(which.min(v)), min(v), spec$text_min))
    }
    if (length(too_big)) {
      add("text_max", "Maximum text size", "fail", sprintf(
        "%s above the %g pt maximum: %s",
        if (length(too_big) == 1) "1 element is" else paste(length(too_big), "elements are"),
        spec$text_max,
        paste(sprintf("%s %.2f pt", names(too_big), too_big), collapse = ", ")))
    } else {
      add("text_max", "Maximum text size", "pass", sprintf(
        "largest element is %s at %.2f pt (maximum %g pt)",
        names(which.max(v)), max(v), spec$text_max))
    }
  }

  # -- typeface ---------------------------------------------------------------
  fams <- unique(unlist(lapply(names(entries), function(nm) {
    if (!length(judged_elements(entries[[nm]], "text", el_tree))) return(NULL)
    el <- element_or_null("text", themes[[nm]])
    if (is.null(el)) NULL else (el$family %||% "")
  })))
  fams <- fams[nzchar(fams)]
  # Report the worst offender: an unpermitted family anywhere fails the figure.
  fam <- if (!length(fams)) "" else {
    bad <- setdiff(fams, spec$families)
    if (length(bad)) bad[1] else fams[1]
  }
  installed <- tryCatch(unique(systemfonts::system_fonts()$family), error = function(e) character())
  if (!nzchar(fam)) {
    add("family", "Typeface", "warn", sprintf(
      "no family is set, so the device default will be used; %s asks for %s",
      spec$journal, paste(spec$families, collapse = " or ")))
  } else if (!fam %in% spec$families) {
    add("family", "Typeface", "warn", sprintf(
      "family is %s; %s asks for %s", fam, spec$journal, paste(spec$families, collapse = " or ")))
  } else if (!fam %in% installed) {
    add("family", "Typeface", "fail", sprintf(
      "family %s is specified but is not installed, so it will be substituted silently", fam))
  } else {
    add("family", "Typeface", "pass", sprintf("%s, which is permitted and installed", fam))
  }

  # -- rule weights -----------------------------------------------------------
  # A rule below about a quarter of a point can disappear entirely in offset
  # printing, or reproduce inconsistently across a print run.
  min_rule_pt <- 0.25
  lw_tbl <- gather_num(.line_elements, function(el) el$linewidth)
  if (!nrow(lw_tbl)) {
    add("rules", "Rule weights", "warn", "no rules are drawn; check that axes are visible")
  } else {
    v <- stats::setNames(linewidth_pt(lw_tbl$value), lw_tbl$label)
    thin <- v[v < min_rule_pt]
    if (length(thin)) {
      add("rules", "Rule weights", "fail", sprintf(
        "%s below %.2f pt and may not print: %s", length(thin), min_rule_pt,
        paste(sprintf("%s %.3f pt", names(thin), thin), collapse = ", ")))
    } else {
      add("rules", "Rule weights", "pass", sprintf(
        "thinnest rule is %s at %.2f pt (floor %.2f pt)",
        names(which.min(v)), min(v), min_rule_pt))
    }
  }

  # -- physical size ----------------------------------------------------------
  widths <- spec$widths
  nearest <- names(widths)[which.min(abs(unname(widths) - width))]
  on_grid <- any(abs(unname(widths) - width) < 0.5)
  if (width > max(unname(widths)) + 0.5) {
    add("size_width", "Figure width", "fail", sprintf(
      "%.2f mm exceeds the widest column, %s at %g mm", width, nearest, max(unname(widths))))
  } else if (!on_grid) {
    add("size_width", "Figure width", "warn", sprintf(
      "%.2f mm is not one of the defined widths (%s); the nearest is %s at %g mm, and a figure scaled to fit will have its type scaled too",
      width, paste(sprintf("%g", unname(widths)), collapse = ", "), nearest, unname(widths[[nearest]])))
  } else {
    add("size_width", "Figure width", "pass", sprintf(
      "%.2f mm, matching the %s column", width, nearest))
  }
  if (height > spec$height_max) {
    add("size_height", "Figure height", "fail", sprintf(
      "%.2f mm exceeds the %g mm maximum depth", height, spec$height_max))
  } else {
    add("size_height", "Figure height", "pass", sprintf(
      "%.2f mm, within the %g mm maximum depth", height, spec$height_max))
  }

  # -- panel labels -----------------------------------------------------------
  tag <- element_or_null("plot.tag", th)
  if (is.null(tag)) {
    add("tag", "Panel labels", "skip", "no panel labels are set")
  } else {
    problems <- character()
    if (is.numeric(tag$size) && abs(tag$size - spec$tag_size) > 0.51) {
      problems <- c(problems, sprintf("size is %.1f pt, not %g pt", tag$size, spec$tag_size))
    }
    if (!identical(tag$face, spec$tag_face)) {
      problems <- c(problems, sprintf("face is %s, not %s", tag$face %||% "plain", spec$tag_face))
    }
    if (length(problems)) {
      add("tag", "Panel labels", "warn", paste(problems, collapse = "; "))
    } else {
      add("tag", "Panel labels", "pass", sprintf(
        "%g pt %s %s", spec$tag_size, spec$tag_face,
        if (spec$tag_case == "lower") "lowercase a, b, c" else "uppercase A, B, C"))
    }
  }

  # -- on-plot title ----------------------------------------------------------
  has_title <- !is.null(tryCatch(plot$labels$title, error = function(e) NULL))
  if (has_title && !spec$name %in% c("slide", "poster")) {
    add("title", "On-plot title", "warn", paste0(
      "a title is set on the plot. In a journal the title belongs in the caption, ",
      "where it can be typeset, indexed, and translated"))
  } else {
    add("title", "On-plot title", "pass",
        if (has_title) "present, which is appropriate for this destination"
        else "none, so the caption carries it")
  }

  # -- imported illustration ---------------------------------------------------
  #
  # An illustration is checked as artwork, not as a picture. Because the SVG was
  # parsed into real geometry rather than rasterised, its strokes have a weight at
  # the figure's final size and that weight can be compared with the publisher's
  # floor. This is the check that becomes impossible the moment a schematic is
  # flattened to a bitmap, which is the ordinary way of putting one in an R figure.
  art_all <- illustration_layers(plot)
  is_raster <- vapply(art_all, function(a) inherits(a$svg, "vellum_raster"), logical(1))
  art_raster <- art_all[is_raster]
  art <- art_all[!is_raster]

  # Raster artwork is held to the venue's resolution requirement, because that is
  # the requirement it actually has. Effective dpi falls as an image is enlarged,
  # which is the failure nobody notices until proof stage.
  if (length(art_raster)) {
    dpis <- vapply(art_raster, function(a) {
      raster_dpi(a$svg, a$width_mm %||% width)
    }, numeric(1))
    req <- unname(spec$dpi[[content]])
    add("art_raster", "Illustration resolution",
        if (min(dpis) < req) "fail" else "pass",
        sprintf("%d raster illustration%s at %.0f-%.0f dpi effective at this size (%s requires %g dpi for %s content)",
                length(dpis), if (length(dpis) == 1L) "" else "s",
                min(dpis), max(dpis), spec$journal, req, content))
    if (isTRUE(spec$vector_required)) {
      add("art_raster_vector", "Illustration vector requirement", "warn", sprintf(
        "%d illustration%s %s raster; %s requires line art and text as vector, so these must be genuinely photographic or painted artwork rather than rasterised diagrams",
        length(dpis), if (length(dpis) == 1L) "" else "s",
        if (length(dpis) == 1L) "is" else "are", spec$journal))
    }
  }

  if (length(art)) {
    # The same quarter-point floor the theme's own rules are held to.
    art_min_pt <- 0.25
    measures <- lapply(art, function(a) asset_measure(a$svg, a$width_mm %||% width))
    weights <- unlist(lapply(measures, `[[`, "linewidths_pt"))
    weights <- weights[is.finite(weights) & weights > 0]
    art_text <- unlist(lapply(measures, `[[`, "text_pt"))
    rasters <- sum(vapply(measures, function(m) as.integer(m$rasters %||% 0L), integer(1)))

    if (length(weights) && min(weights) < art_min_pt) {
      add("art_linewidth", "Illustration rule weight", "fail", sprintf(
        "%d rule%s inside the imported artwork fall%s below the %g pt minimum at this size; the thinnest is %.3f pt",
        sum(weights < art_min_pt), if (sum(weights < art_min_pt) == 1) "" else "s",
        if (sum(weights < art_min_pt) == 1) "s" else "", art_min_pt, min(weights)))
    } else if (length(weights)) {
      add("art_linewidth", "Illustration rule weight", "pass", sprintf(
        "thinnest imported rule is %.3f pt at this size (minimum %g pt)",
        min(weights), art_min_pt))
    }

    if (length(art_text)) {
      small <- art_text[art_text < spec$text_min]
      add("art_text", "Illustration text size",
          if (length(small)) "fail" else "pass",
          if (length(small)) sprintf(
            "%d text element%s inside the artwork %s below the %g pt minimum at this size; the smallest is %.2f pt",
            length(small), if (length(small) == 1) "" else "s",
            if (length(small) == 1) "is" else "are", spec$text_min, min(art_text))
          else sprintf("smallest text inside the artwork is %.2f pt (minimum %g pt)",
                       min(art_text), spec$text_min))
    }

    add("art_vector", "Illustration vector purity",
        if (rasters > 0L) "warn" else "pass",
        if (rasters > 0L) sprintf(
          "%d embedded raster image%s in the imported artwork could not be drawn as vector geometry",
          rasters, if (rasters == 1L) "" else "s")
        else sprintf("%d imported illustration%s %s vector geometry throughout",
                     length(art), if (length(art) == 1L) "" else "s",
                     if (length(art) == 1L) "is" else "are"))

    licences <- unlist(lapply(art, function(a) a$svg$provenance$licence_recorded))
    disputed <- vapply(art, function(a) isTRUE(a$svg$provenance$licence_disputed), logical(1))
    sa <- vapply(art, function(a) isTRUE(a$svg$provenance$share_alike), logical(1))
    if (is.null(licences)) {
      add("art_licence", "Illustration licence", "warn", paste0(
        "imported artwork carries no provenance; load it with vellum_asset() ",
        "rather than svg_read() so the licence travels with the figure"))
    } else {
      add("art_licence", "Illustration licence",
          if (any(disputed) || any(sa)) "warn" else "pass",
          if (any(disputed))
            sprintf("%d asset's recorded licence disagrees with the licence stated inside the file; resolve before submission",
                    sum(disputed))
          else if (any(sa))
            sprintf("%d asset is share-alike, so its terms propagate to this figure",
                    sum(sa))
          else sprintf("licence%s recorded: %s",
                       if (length(unique(licences)) == 1L) "" else "s",
                       paste(unique(licences), collapse = ", ")))
    }
  }

  # -- raster content and resolution -----------------------------------------
  raster <- has_raster_layer(plot)
  if (raster) {
    req <- unname(spec$dpi[[content]])
    add("raster", "Raster content", "warn", sprintf(
      "a raster layer is present, so export at %g dpi or convert it to vector; %s",
      req,
      if (isTRUE(spec$vector_required))
        "text and line art must still be supplied as editable vector"
      else "check the venue's resolution requirement"))
  } else {
    add("raster", "Raster content", "pass", "no raster layers; the figure is line art throughout")
  }

  # -- rejected formats -------------------------------------------------------
  if (length(spec$formats_rejected)) {
    add("formats", "Accepted formats", "pass", sprintf(
      "prefer %s; %s does not accept %s for final artwork",
      paste(spec$formats_preferred, collapse = ", "), spec$journal,
      paste(spec$formats_rejected, collapse = ", ")))
  }

  # -- composed-figure caveat -------------------------------------------------
  if (multi) {
    panels <- sum(vapply(entries, function(e) identical(e$kind, "panel"), logical(1)))
    container_bare <- any(vapply(entries, function(e) {
      identical(e$kind, "container") && !any(.text_elements %in% e$set)
    }, logical(1)))
    add("composed", "Composed figure", if (container_bare) "warn" else "pass",
        sprintf(paste0("%d panel theme%s inspected. %s"), panels,
                if (panels == 1) "" else "s",
                if (container_bare)
                  paste0("patchwork keeps one panel's theme on the container and ",
                         "plot_annotation() overwrites it, so that panel is not visible ",
                         "here. Use venue_check_panels() to check each panel directly.")
                else "every panel theme was recovered from the object"))
  }

  # -- provenance -------------------------------------------------------------
  add("provenance", "Specification provenance",
      if (isTRUE(spec$verified)) "pass" else "warn",
      if (isTRUE(spec$verified))
        sprintf("read from the publisher's artwork guide%s",
                if (!is.na(spec$retrieved)) paste0(", retrieved ", spec$retrieved) else "")
      else sprintf("this venue's numbers are an unverified default; confirm at %s",
                   spec$source %|na|% "no recorded source"))

  verdicts <- vapply(checks, `[[`, character(1), "verdict")
  verdicts <- verdicts[verdicts != "skip"]
  overall <- if (any(verdicts == "fail")) "fail" else if (any(verdicts == "warn")) "warn" else "pass"

  out <- structure(list(
    venue = spec$name, journal = spec$journal, spec = spec,
    width = width, height = height, content = content,
    checks = checks, verdict = overall
  ), class = "vellum_preflight")

  if (quiet) invisible(out) else out
}

`%|na|%` <- function(x, y) if (is.null(x) || length(x) != 1L || is.na(x)) y else x

#' @export
print.vellum_preflight <- function(x, ...) {
  head <- switch(x$verdict,
                 pass = "Pre-flight \u00b7 pass",
                 warn = "Pre-flight \u00b7 pass with warnings",
                 fail = "Pre-flight \u00b7 fail")
  cli::cli_h1("{head} \u00b7 {x$journal}")
  cli::cli_text("{round(x$width, 2)} x {round(x$height, 2)} mm \u00b7 {x$content} art")
  cli::cli_par()
  for (ck in x$checks) {
    if (identical(ck$verdict, "skip")) next
    fn <- switch(ck$verdict,
                 pass = cli::cli_alert_success,
                 warn = cli::cli_alert_warning,
                 fail = cli::cli_alert_danger)
    fn("{ck$label}: {ck$detail}")
  }
  cli::cli_end()
  if (x$verdict == "fail") {
    cli::cli_bullets(c(
      i = "Build the figure for the venue with {.code theme_venue('{x$venue}')}.",
      i = "Fix its physical size with {.code fit_width(plot, '{x$venue}', 'single')}.",
      i = "Then write it with {.fn save_figure}."
    ))
  }
  invisible(x)
}

#' Summarise a pre-flight report as a data frame
#'
#' Useful for logging compliance across a whole figure set, or for asserting it in
#' a test.
#'
#' @param object A `vellum_preflight` object.
#' @param ... Ignored.
#' @return A data frame with one row per check.
#' @export
#' @examples
#' library(ggplot2)
#' r <- venue_check(ggplot(mtcars, aes(wt, mpg)) + geom_point(), "nature", quiet = TRUE)
#' summary(r)
summary.vellum_preflight <- function(object, ...) {
  data.frame(
    check = vapply(object$checks, `[[`, character(1), "id"),
    label = vapply(object$checks, `[[`, character(1), "label"),
    verdict = vapply(object$checks, `[[`, character(1), "verdict"),
    detail = vapply(object$checks, `[[`, character(1), "detail"),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Check each panel of a composed figure directly
#'
#' @description
#' The reliable way to pre-flight a multi-panel plate.
#'
#' [venue_check()] does its best on a `patchwork` object, but patchwork stores one
#' panel's theme on the container and `plot_annotation()` overwrites it, so that panel's
#' theme is not recoverable from the assembled object. Rather than guess, pass the panels
#' themselves: every one is then checked in full, and the report names any that fail.
#'
#' @param plots A list of `ggplot` objects, ideally named. Bare plots are also accepted
#'   as separate arguments.
#' @param venue Target venue.
#' @param width,height Physical size of the whole figure, used for the size checks on the
#'   assembled plate rather than on each panel.
#' @param ... Passed to [venue_check()].
#'
#' @return An object of class `vellum_preflight_panels`, printing one line per panel plus
#'   the worst verdict.
#' @export
#' @examples
#' library(ggplot2)
#' a <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature")
#' b <- ggplot(mtcars, aes(wt, hp)) + geom_point()          # deliberately unthemed
#' venue_check_panels(list(a = a, b = b), "nature")
venue_check_panels <- function(plots, venue = "nature", width = NULL, height = NULL, ...) {
  if (inherits(plots, "ggplot")) plots <- list(plots)
  if (!is.list(plots) || !length(plots)) {
    cli::cli_abort("{.arg plots} must be a list of {.cls ggplot} objects.")
  }
  bad <- !vapply(plots, function(p) inherits(p, "ggplot"), logical(1))
  if (any(bad)) {
    cli::cli_abort("Element{?s} {.val {which(bad)}} of {.arg plots} {?is/are} not a {.cls ggplot}.")
  }
  nms <- names(plots) %none% paste("panel", seq_along(plots))
  nms[!nzchar(nms)] <- paste("panel", seq_along(plots))[!nzchar(nms)]

  reports <- lapply(seq_along(plots), function(i) {
    venue_check(plots[[i]], venue = venue, width = width, height = height,
                quiet = TRUE, ...)
  })
  names(reports) <- nms
  verdicts <- vapply(reports, `[[`, character(1), "verdict")
  overall <- if (any(verdicts == "fail")) "fail" else if (any(verdicts == "warn")) "warn" else "pass"
  structure(list(reports = reports, verdicts = verdicts, verdict = overall,
                 venue = venue_spec(venue)$name),
            class = "vellum_preflight_panels")
}

#' @export
print.vellum_preflight_panels <- function(x, ...) {
  head <- switch(x$verdict, pass = "Panel pre-flight \u00b7 pass",
                 warn = "Panel pre-flight \u00b7 pass with warnings",
                 fail = "Panel pre-flight \u00b7 fail")
  cli::cli_h1("{head} \u00b7 {x$venue}")
  for (nm in names(x$reports)) {
    r <- x$reports[[nm]]
    fn <- switch(r$verdict, pass = cli::cli_alert_success,
                 warn = cli::cli_alert_warning, fail = cli::cli_alert_danger)
    failed <- vapply(r$checks, function(c) c$verdict %in% c("warn", "fail"), logical(1))
    fn("{nm}: {r$verdict}{if (any(failed)) paste0(' \u2014 ', paste(vapply(r$checks[failed], `[[`, character(1), 'label'), collapse = '; ')) else ''}")
  }
  cli::cli_bullets(c(i = "Call {.fn print} on an element of {.code $reports} for the full report on one panel."))
  invisible(x)
}
