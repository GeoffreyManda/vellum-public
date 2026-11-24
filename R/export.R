# Export.
#
# Two things go wrong at this stage even when the figure itself is right.
#
# First, the device. R's classic `pdf()` and `png()` devices use a private font
# database that does not include most system fonts, so a figure specifying
# Helvetica or Arial is silently substituted and the warning is easy to miss. The
# cairo and ragg devices read system fonts properly and embed them. This function
# only uses those.
#
# Second, the size. Setting a device to 89 mm gives an 89 mm canvas; the plot then
# stretches to fill it. Combined with fit_width(), which fixes the figure's own
# geometry, the device size and the figure size finally agree.

.devices <- list(
  pdf   = list(vector = TRUE,  ext = "pdf"),
  eps   = list(vector = TRUE,  ext = "eps"),
  svg   = list(vector = TRUE,  ext = "svg"),
  png   = list(vector = FALSE, ext = "png"),
  tiff  = list(vector = FALSE, ext = "tiff"),
  jpeg  = list(vector = FALSE, ext = "jpg")
)

open_device <- function(format, path, width_in, height_in, dpi, bg, family) {
  switch(
    format,
    pdf = grDevices::cairo_pdf(filename = path, width = width_in, height = height_in,
                               bg = bg, family = family, onefile = TRUE),
    eps = grDevices::cairo_ps(filename = path, width = width_in, height = height_in,
                              bg = bg, family = family, fallback_resolution = dpi),
    svg = if (has_pkg("svglite")) {
      svglite::svglite(filename = path, width = width_in, height = height_in, bg = bg)
    } else {
      grDevices::svg(filename = path, width = width_in, height = height_in,
                     bg = bg, family = family)
    },
    png = if (has_pkg("ragg")) {
      ragg::agg_png(filename = path, width = width_in, height = height_in,
                    units = "in", res = dpi, background = bg)
    } else {
      grDevices::png(filename = path, width = width_in, height = height_in,
                     units = "in", res = dpi, bg = bg, type = "cairo")
    },
    tiff = if (has_pkg("ragg")) {
      ragg::agg_tiff(filename = path, width = width_in, height = height_in,
                     units = "in", res = dpi, background = bg, compression = "lzw")
    } else {
      grDevices::tiff(filename = path, width = width_in, height = height_in,
                      units = "in", res = dpi, bg = bg, type = "cairo",
                      compression = "lzw")
    },
    jpeg = if (has_pkg("ragg")) {
      ragg::agg_jpeg(filename = path, width = width_in, height = height_in,
                     units = "in", res = dpi, background = bg, quality = 95)
    } else {
      grDevices::jpeg(filename = path, width = width_in, height = height_in,
                      units = "in", res = dpi, bg = bg, type = "cairo", quality = 95)
    },
    cli::cli_abort("Unsupported format {.val {format}}.")
  )
}

# The size actually written to the file, as distinct from the size requested.
#
# No device can write an arbitrary physical size. Cairo's PDF and PostScript
# backends quantise the page box to whole points, so a 89 mm width becomes
# round(89/25.4*72) = 252 pt = 88.90 mm. Raster devices quantise to whole pixels
# at the chosen resolution. Both deviations are small -- at most half a point,
# 0.176 mm, for cairo, and at most half a pixel for raster -- and far inside any
# publisher's tolerance, but a package that claims exact geometry should report
# the number it actually achieved rather than the one it was asked for.
written_size <- function(width_mm, height_mm, format, dpi) {
  in_w <- width_mm / 25.4
  in_h <- height_mm / 25.4
  if (format %in% c("pdf", "eps")) {
    c(round(in_w * 72) / 72 * 25.4, round(in_h * 72) / 72 * 25.4)
  } else if (format == "svg") {
    c(width_mm, height_mm)   # SVG carries fractional dimensions exactly
  } else {
    c(round(in_w * dpi) / dpi * 25.4, round(in_h * dpi) / dpi * 25.4)
  }
}

#' Save a figure at exact physical size
#'
#' @description
#' Writes a figure at the target venue's own column width, using devices that can
#' actually see system fonts, and returns a manifest recording what was written and
#' whether each file is eligible for submission to that venue.
#'
#' Vector formats come first deliberately. Nature's artwork guide is explicit that
#' line art and text must not be rasterised and that JPEG, TIFF, and PNG are not
#' accepted for final artwork; the common advice to "export at 600 dpi PNG" fails
#' at the production stage. A raster file is a review copy, and the manifest labels
#' it as one.
#'
#' @param plot A `ggplot` or `patchwork` object. If it came from [fit_width()], its
#'   fitted size is used unless `width` and `height` say otherwise.
#' @param file Output path. An extension is added per format, and any extension
#'   given is stripped, so one call can write several formats.
#' @param venue Target venue. See [venues()].
#' @param column Column name (`"single"`, `"onehalf"`, `"double"`) or a width in
#'   millimetres.
#' @param width,height Explicit size in millimetres, overriding `venue`/`column`.
#' @param formats Formats to write. Defaults to a vector submission file plus a PNG
#'   review copy.
#' @param content `"line"` for charts and schematics, `"combination"` for a chart
#'   with a raster layer such as a heatmap or dense scatter, `"halftone"` for
#'   photographs and micrographs. Selects the venue's dpi requirement.
#' @param dpi Explicit resolution, overriding `content`.
#' @param bg Background. `"white"` by default; use `"transparent"` for slides.
#' @param check Run [venue_check()] and report any problems.
#' @param quiet Suppress the written-files message.
#'
#' @return Invisibly, a data frame manifest with one row per file, recording both
#'   the requested size and the size actually written. These differ slightly for
#'   PDF, EPS, and raster output because those devices quantise the page box to
#'   whole points or whole pixels; the deviation is at most 0.18 mm and is reported
#'   rather than concealed.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_venue("nature")
#' path <- file.path(tempdir(), "figure-1")
#' save_figure(p, path, venue = "nature", column = "single", check = FALSE)
#' @export
save_figure <- function(plot, file, venue = "nature", column = "single",
                        width = NULL, height = NULL, formats = NULL,
                        content = c("line", "combination", "halftone"),
                        dpi = NULL, bg = "white", check = TRUE, quiet = FALSE) {
  content <- match.arg(content)
  check_string(file, "file")
  spec <- venue_spec(venue)
  warn_unverified(spec)

  fit <- attr(plot, "vellum_fit")
  if (is.null(width) && is.null(height) && !is.null(fit)) {
    width <- fit$achieved[["width"]]
    height <- fit$achieved[["height"]]
  }
  width <- width %||% venue_width(venue, column)
  height <- height %||% (width * 0.72)
  check_number(width, "width", min = 1)
  check_number(height, "height", min = 1)

  if (height > spec$height_max) {
    cli::cli_warn("Height {round(height, 1)} mm exceeds {.val {spec$name}}'s maximum of {spec$height_max} mm.")
  }

  formats <- formats %||% c(if (length(spec$formats_preferred)) spec$formats_preferred[1] else "pdf", "png")
  formats <- unique(tolower(formats))
  unknown <- setdiff(formats, names(.devices))
  if (length(unknown)) {
    cli::cli_abort(c("Unsupported format{?s} {.val {unknown}}.",
                     i = "Available: {.val {names(.devices)}}."))
  }
  dpi <- dpi %||% unname(spec$dpi[[content]])
  family <- as.character(resolve_font(spec$families))
  stem <- sub("\\.[A-Za-z0-9]+$", "", file)
  dir <- dirname(stem)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  rows <- vector("list", length(formats))
  for (i in seq_along(formats)) {
    fmt <- formats[i]
    info <- .devices[[fmt]]
    path <- paste0(stem, ".", info$ext)
    open_device(fmt, path, width / 25.4, height / 25.4, dpi, bg, family)
    ok <- tryCatch({ print(plot); TRUE },
                   error = function(e) { grDevices::dev.off(); stop(e) })
    grDevices::dev.off()

    eligible <- !(fmt %in% tolower(spec$formats_rejected)) &&
      (!isTRUE(spec$vector_required) || isTRUE(info$vector) ||
         !identical(content, "line"))
    written <- written_size(width, height, fmt, dpi)
    rows[[i]] <- data.frame(
      file = path, format = fmt, vector = info$vector,
      width_mm = round(width, 2), height_mm = round(height, 2),
      written_width_mm = round(written[1], 3), written_height_mm = round(written[2], 3),
      dpi = if (info$vector) NA_integer_ else as.integer(dpi),
      submission_eligible = eligible,
      size_kb = round(file.size(path) / 1024, 1),
      stringsAsFactors = FALSE
    )
  }
  manifest <- do.call(rbind, rows)
  attr(manifest, "venue") <- spec$name

  if (!quiet) {
    cli::cli_inform(c(
      "Wrote {nrow(manifest)} file{?s} at {round(width, 2)} x {round(height, 2)} mm for {.val {spec$name}}:",
      stats::setNames(
        sprintf("%s  %s%s", basename(manifest$file),
                ifelse(manifest$vector, "vector", paste0(manifest$dpi, " dpi")),
                ifelse(manifest$submission_eligible, "", "  (review copy only)")),
        rep("*", nrow(manifest))
      )
    ))
    ineligible <- manifest[!manifest$submission_eligible, ]
    if (nrow(ineligible) && length(spec$formats_rejected)) {
      cli::cli_inform(c(i = "{.val {spec$name}} does not accept {.val {tolower(spec$formats_rejected)}} for final artwork."))
    }
  }

  if (check) {
    rep <- venue_check(plot, venue = venue, width = width, height = height, quiet = TRUE)
    if (rep$verdict != "pass") {
      # Skipped checks are not issues. `venue_check()` excludes them when it
      # decides its own verdict, and counting them here reported more problems
      # than the report itself then listed.
      verdicts <- vapply(rep$checks, function(chk) chk$verdict, character(1))
      n_issues <- sum(!verdicts %in% c("pass", "skip"))
      cli::cli_inform(c("!" = "Pre-flight found {n_issues} issue{?s}. Run {.code venue_check()} for the report."))
    }
  }
  invisible(manifest)
}

#' Save a figure for several destinations at once
#'
#' @description
#' The same figure usually has to exist at more than one size: the journal column,
#' a slide, a poster block. Rebuilding it by hand for each is how versions drift
#' apart. This writes them all from one plot, adjusting type sizes for each
#' destination rather than merely scaling the image, which would leave 7 pt text at
#' 7 pt on a poster.
#'
#' @param plot A `ggplot` object.
#' @param file Output path stem. The venue name is appended.
#' @param venues Venues to write for.
#' @param column Column name, resolved per venue.
#' @param ... Passed to [save_figure()].
#' @param retheme Rebuild the plot's theme for each venue. Leave `TRUE`; a poster
#'   needs bigger type, not a bigger picture.
#'
#' @return Invisibly, the combined manifest.
#' @export
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_vellum()
#' save_figure_set(p, file.path(tempdir(), "fig1"),
#'                 venues = c("nature", "slide"), check = FALSE, quiet = TRUE)
save_figure_set <- function(plot, file, venues = c("nature", "slide", "poster"),
                            column = "single", ..., retheme = TRUE) {
  out <- lapply(venues, function(v) {
    p <- plot
    if (retheme) p <- p + theme_venue(v)
    save_figure(p, paste0(sub("\\.[A-Za-z0-9]+$", "", file), "-", v),
                venue = v, column = column, ...)
  })
  invisible(do.call(rbind, out))
}
