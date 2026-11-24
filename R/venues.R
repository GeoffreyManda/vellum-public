# Venue artwork specifications.
#
# Every field a publisher states in millimetres, points, or dots per inch is
# stored here as data, together with where it came from and whether it has been
# read from the primary source. `verified = TRUE` means the numbers in that entry
# were read directly from the publisher's own artwork document. Anything else is
# a working default that the user should confirm, and `venue_check()` says so.

.venue_registry <- local({

  spec <- function(name, journal, publisher,
                   widths, height_max,
                   text_min, text_max, tag_size,
                   tag_case = c("lower", "upper"), tag_face = "bold",
                   families = c("Helvetica", "Arial"),
                   colour_space = "RGB",
                   dpi = c(line = 1200, combination = 600, halftone = 300),
                   formats_preferred = c("pdf", "eps", "svg"),
                   formats_rejected = character(),
                   vector_required = TRUE,
                   source = NA_character_, retrieved = NA_character_,
                   verified = FALSE, notes = character()) {
    tag_case <- match.arg(tag_case)
    structure(
      list(
        name = name, journal = journal, publisher = publisher,
        widths = widths, height_max = height_max,
        text_min = text_min, text_max = text_max,
        tag_size = tag_size, tag_case = tag_case, tag_face = tag_face,
        families = families, colour_space = colour_space, dpi = dpi,
        formats_preferred = formats_preferred, formats_rejected = formats_rejected,
        vector_required = vector_required,
        source = source, retrieved = retrieved, verified = verified,
        notes = notes
      ),
      class = c("vellum_venue", "list")
    )
  }

  list(

    # -- Nature Portfolio -----------------------------------------------------
    # Read in full from the publisher's own "Guide to Preparing Final Artwork".
    # Every number below is quoted from that document.
    nature = spec(
      name = "nature",
      journal = "Nature", publisher = "Nature Portfolio",
      widths = c(single = 89, onehalf = 120, onehalf_wide = 136, double = 183),
      height_max = 247,
      text_min = 5, text_max = 7, tag_size = 8,
      tag_case = "lower", tag_face = "bold",
      families = c("Helvetica", "Arial"),
      colour_space = "RGB",
      dpi = c(line = 1200, combination = 600, halftone = 300),
      formats_preferred = c("pdf", "eps", "ai", "ps"),
      formats_rejected = c("jpeg", "jpg", "tiff", "tif", "png"),
      vector_required = TRUE,
      source = "https://www.nature.com/documents/nature-final-artwork.pdf",
      retrieved = "2026-07-30", verified = TRUE,
      notes = c(
        "7 pt is a maximum for all text other than panel labels, not a target.",
        "Panel labels are 8 pt bold upright lowercase a, b, c.",
        "Line art and text must not be rasterised; supply editable vector.",
        "JPEG, TIFF and PNG are not accepted for final artwork.",
        "Amino-acid sequences use one-letter code set in Courier."
      )
    ),

    # -- Science / AAAS -------------------------------------------------------
    science = spec(
      name = "science",
      journal = "Science", publisher = "AAAS",
      widths = c(single = 57, double = 121, triple = 184),
      height_max = 240,
      text_min = 5, text_max = 9, tag_size = 9,
      tag_case = "upper", tag_face = "bold",
      families = c("Helvetica", "Arial"),
      colour_space = "RGB",
      dpi = c(line = 1200, combination = 600, halftone = 300),
      formats_preferred = c("pdf", "eps", "ai"),
      vector_required = TRUE,
      source = "https://www.science.org/content/page/science-information-authors-preparing-your-manuscript",
      retrieved = NA_character_, verified = FALSE,
      notes = c(
        "Science sets three text columns; figures are sized to 1, 2 or 3 of them.",
        "Panel labels are conventionally bold uppercase.",
        "UNVERIFIED: confirm widths and type sizes against the source before submission."
      )
    ),

    # -- Cell Press -----------------------------------------------------------
    cell = spec(
      name = "cell",
      journal = "Cell", publisher = "Cell Press (Elsevier)",
      widths = c(single = 85, onehalf = 114, double = 174),
      height_max = 234,
      text_min = 6, text_max = 10, tag_size = 10,
      tag_case = "upper", tag_face = "bold",
      families = c("Arial", "Helvetica"),
      colour_space = "RGB",
      dpi = c(line = 1000, combination = 500, halftone = 300),
      formats_preferred = c("pdf", "eps", "ai"),
      vector_required = TRUE,
      source = "https://www.cell.com/figureguidelines",
      retrieved = NA_character_, verified = FALSE,
      notes = c(
        "Cell Press figures are commonly assembled as tall multi-panel plates.",
        "Panel labels are bold uppercase A, B, C.",
        "UNVERIFIED: confirm widths, height ceiling and type sizes before submission."
      )
    ),

    # -- PNAS -----------------------------------------------------------------
    pnas = spec(
      name = "pnas",
      journal = "PNAS", publisher = "National Academy of Sciences",
      widths = c(single = 87, onehalf = 114, double = 178),
      height_max = 235,
      text_min = 6, text_max = 8, tag_size = 9,
      tag_case = "upper",
      source = "https://www.pnas.org/author-center/submitting-your-manuscript",
      verified = FALSE,
      notes = "UNVERIFIED: confirm against the PNAS author centre before submission."
    ),

    # -- PLOS -----------------------------------------------------------------
    plos = spec(
      name = "plos",
      journal = "PLOS journals", publisher = "Public Library of Science",
      widths = c(min = 66.8, single = 132, max = 190.5),
      height_max = 222.3,
      text_min = 8, text_max = 12, tag_size = 12,
      tag_case = "upper",
      families = c("Arial", "Helvetica", "Times New Roman", "Symbol"),
      dpi = c(line = 600, combination = 600, halftone = 300),
      formats_preferred = c("tiff", "eps"),
      vector_required = FALSE,
      source = "https://journals.plos.org/plosone/s/figures",
      retrieved = "2026-07-30", verified = TRUE,
      notes = c(
        "Figures are 66.8-190.5 mm wide; 132 mm aligns with the article text column.",
        "PLOS accepts TIFF or EPS at 300-600 dpi and caps height at 222.3 mm.",
        "PLOS is unusual in requiring 8-12 pt text, larger than Nature."
      )
    ),

    # -- eLife ----------------------------------------------------------------
    elife = spec(
      name = "elife",
      journal = "eLife", publisher = "eLife Sciences",
      widths = c(single = 85, double = 176),
      height_max = 225,
      text_min = 7, text_max = 10, tag_size = 10,
      tag_case = "upper",
      source = "https://elifesciences.org/author-guide",
      verified = FALSE,
      notes = "UNVERIFIED: eLife is screen-first; confirm current guidance."
    ),

    # -- Generic print --------------------------------------------------------
    generic = spec(
      name = "generic",
      journal = "Generic two-column print journal", publisher = NA_character_,
      widths = c(single = 89, onehalf = 136, double = 183),
      height_max = 240,
      text_min = 6, text_max = 9, tag_size = 9,
      tag_case = "lower",
      verified = FALSE,
      notes = "A conservative default that satisfies most two-column print journals."
    ),

    # -- Non-journal destinations ---------------------------------------------
    thesis = spec(
      name = "thesis",
      journal = "Thesis or dissertation (A4)", publisher = NA_character_,
      widths = c(single = 75, double = 160, full = 160),
      height_max = 220,
      text_min = 8, text_max = 11, tag_size = 11,
      tag_case = "lower",
      families = c("Helvetica", "Arial", "Times New Roman"),
      verified = FALSE,
      notes = "A4 with 25 mm margins gives a 160 mm text block; captions run fuller than in journals."
    ),

    slide = spec(
      name = "slide",
      journal = "Presentation slide (16:9)", publisher = NA_character_,
      widths = c(single = 120, double = 250, full = 250),
      height_max = 130,
      text_min = 12, text_max = 20, tag_size = 20,
      tag_case = "upper",
      colour_space = "RGB",
      dpi = c(line = 300, combination = 300, halftone = 200),
      formats_preferred = c("pdf", "png", "svg"),
      vector_required = FALSE,
      verified = FALSE,
      notes = "Read at distance: type is much larger and panel counts much lower than print."
    ),

    portfolio = spec(
      name = "portfolio",
      journal = "Visualisation consultancy portfolio", publisher = "Vellum",
      widths = c(single = 120, double = 250, full = 250),
      height_max = 160,
      text_min = 8, text_max = 11, tag_size = 11,
      tag_case = "upper",
      colour_space = "RGB",
      dpi = c(line = 600, combination = 300, halftone = 300),
      formats_preferred = c("pdf", "svg", "png"),
      vector_required = FALSE,
      source = "Vellum internal consultancy plate specification v0.2.0",
      retrieved = "2026-07-30", verified = TRUE,
      notes = c(
        "A 250 x 160 mm landscape plate for four-panel portfolio comparisons.",
        "Typography is optimized for screen review and printable A4 landscape reduction."
      )
    ),

    poster = spec(
      name = "poster",
      journal = "Conference poster (A0)", publisher = NA_character_,
      widths = c(single = 250, double = 520, full = 780),
      height_max = 1050,
      text_min = 18, text_max = 36, tag_size = 36,
      tag_case = "upper",
      dpi = c(line = 300, combination = 300, halftone = 200),
      formats_preferred = c("pdf", "svg"),
      vector_required = TRUE,
      verified = FALSE,
      notes = "A0 is 841 x 1189 mm; these are sensible figure blocks within a margined layout."
    )
  )
})

# nature_portfolio is an alias, not a copy, so the two can never drift apart.
.venue_aliases <- c(
  nature_portfolio = "nature", npg = "nature", nat = "nature",
  aaas = "science", cellpress = "cell", cell_press = "cell",
  plosone = "plos", plos_one = "plos",
  dissertation = "thesis", talk = "slide", presentation = "slide",
  showcase = "portfolio"
)

# User overrides live in their own environment so the shipped registry stays
# immutable and `venue_reset()` can always restore it.
.venue_user <- new.env(parent = emptyenv())

resolve_venue_name <- function(venue, call = rlang::caller_env()) {
  check_string(venue, "venue", call = call)
  key <- vellum_key(venue)
  if (!is.na(.venue_aliases[key]) && key %in% names(.venue_aliases)) {
    key <- unname(.venue_aliases[key])
  }
  known <- union(names(.venue_registry), ls(.venue_user))
  if (!key %in% known) {
    cli::cli_abort(c(
      "Unknown venue {.val {venue}}.",
      i = "Known venues: {.val {sort(known)}}.",
      i = "Add your own with {.fn venue_register}."
    ), call = call)
  }
  key
}

#' Journal artwork specifications
#'
#' @description
#' `vellum` stores each venue's published artwork specification as data, together
#' with the source it came from and whether that source has been read directly.
#' This is what makes a specification checkable rather than aspirational.
#'
#' `"nature"` and `"plos"` ship `verified = TRUE`: their numbers were read from
#' the publishers' current artwork guidance. The remaining journal entries are
#' working defaults; confirm them against the `source` URL and, if they differ,
#' correct them permanently with [venue_register()].
#'
#' @param venue A venue name, such as `"nature"`, `"science"`, `"cell"`,
#'   `"pnas"`, `"plos"`, `"elife"`, `"generic"`, `"thesis"`, `"slide"`, or
#'   `"poster"`. Common aliases such as `"nature_portfolio"` are accepted.
#' @param column Which column width to return: `"single"`, `"onehalf"`,
#'   `"double"`, or any other name present in the venue's `widths`. Also accepts
#'   a number, which is returned unchanged so that callers can take either.
#' @param venue_names Optional character vector of venue names or aliases to
#'   include in [venue_summary()]. `NULL` returns every registered venue.
#'
#' @return `venue_spec()` returns a `vellum_venue` object. `venue_width()`
#'   returns a width in millimetres. `venues()` returns a data frame of all
#'   registered venues. `venue_summary()` returns a compact classed data frame,
#'   optionally filtered to `venue_names`.
#'
#' @examples
#' venue_spec("nature")
#' venue_width("nature", "double")
#' venue_width("cell", "single")
#' venues()
#' @name venues
NULL

#' @rdname venues
#' @export
venue_spec <- function(venue = "nature") {
  key <- resolve_venue_name(venue)
  if (key %in% ls(.venue_user)) get(key, envir = .venue_user) else .venue_registry[[key]]
}

#' @rdname venues
#' @export
venue_width <- function(venue = "nature", column = "single") {
  if (is.numeric(column)) {
    check_number(column, "column", min = 1)
    return(column)
  }
  check_string(column, "column")
  spec <- venue_spec(venue)
  w <- spec$widths
  if (!column %in% names(w)) {
    cli::cli_abort(c(
      "{.val {column}} is not a defined width for venue {.val {spec$name}}.",
      i = "Available: {.val {names(w)}}."
    ))
  }
  unname(w[[column]])
}

#' @rdname venues
#' @export
venues <- function() {
  keys <- union(names(.venue_registry), ls(.venue_user))
  keys <- sort(keys)
  specs <- lapply(keys, venue_spec)
  data.frame(
    venue = vapply(specs, `[[`, character(1), "name"),
    journal = vapply(specs, function(s) s$journal %||% NA_character_, character(1)),
    single_mm = vapply(specs, function(s) unname(s$widths[["single"]]), numeric(1)),
    double_mm = vapply(specs, function(s) {
      w <- s$widths
      # Only `single` is required of a registered venue, so a venue can legally
      # have no wider column at all.
      wide <- intersect(c("double", "triple", "full", "max"), names(w))
      if (!length(wide)) NA_real_ else unname(as.numeric(w[[utils::tail(wide, 1)]]))
    }, numeric(1)),
    height_max_mm = vapply(specs, `[[`, numeric(1), "height_max"),
    text_min_pt = vapply(specs, `[[`, numeric(1), "text_min"),
    text_max_pt = vapply(specs, `[[`, numeric(1), "text_max"),
    verified = vapply(specs, `[[`, logical(1), "verified"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @rdname venues
#' @export
venue_summary <- function(venue_names = NULL) {
  df <- venues()
  if (!is.null(venue_names)) {
    keys <- vapply(venue_names, resolve_venue_name, character(1))
    df <- df[df$venue %in% keys, , drop = FALSE]
  }
  class(df) <- c("vellum_venue_summary", "data.frame")
  df
}


#' Register or override a venue specification
#'
#' @description
#' Publisher guidance changes, and this package ships some entries unverified on
#' purpose rather than pretending otherwise. Use `venue_register()` to correct a
#' shipped venue or to add a new one -- a lab template, a society journal, a
#' funder's report format -- and every theme, export, and compliance check will
#' use your numbers from then on.
#'
#' Overrides last for the session. Put the call in a project startup script, or
#' in a small internal package, to make it durable.
#'
#' @param name Name to register under. Overrides a shipped venue if it matches.
#' @param widths A named numeric vector of column widths in millimetres. Must
#'   include `single`.
#' @param height_max Maximum figure height in millimetres.
#' @param text_min,text_max Minimum and maximum permitted text size in points.
#' @param tag_size Panel-label size in points.
#' @param tag_case Panel-label case, `"lower"` or `"upper"`.
#' @param tag_face Panel-label font face, such as `"bold"` or `"plain"`.
#' @param families Permitted typeface families, most preferred first.
#' @param ... Any other field of a venue specification, such as `dpi`,
#'   `colour_space`, `formats_preferred`, `vector_required`, or `notes`.
#' @param source URL or citation for the specification.
#' @param retrieved The date the specification was read from `source`, as a
#'   string. Left `NA` -- meaning not stated -- rather than defaulted to today,
#'   because a stamp the caller did not supply is not provenance.
#' @param verified Set `TRUE` only when you have read the numbers from the
#'   publisher's own document. This flag is what silences the compliance
#'   warning, so setting it dishonestly defeats the purpose.
#' @param journal,publisher Human-readable labels used in reports.
#'
#' @return Invisibly, the registered `vellum_venue` object.
#' @export
#' @examples
#' venue_register(
#'   "my_society",
#'   widths = c(single = 80, double = 168),
#'   height_max = 230, text_min = 7, text_max = 9, tag_size = 9,
#'   journal = "Journal of My Society",
#'   source = "internal style sheet v3", verified = TRUE
#' )
#' venue_width("my_society", "double")
venue_register <- function(name, widths, height_max,
                           text_min = 6, text_max = 9, tag_size = 9,
                           tag_case = c("lower", "upper"), tag_face = "bold",
                           families = c("Helvetica", "Arial"),
                           ...,
                           source = NA_character_, retrieved = NA_character_,
                           verified = FALSE,
                           journal = NULL, publisher = NA_character_) {
  check_string(name, "name")
  tag_case <- match.arg(tag_case)
  check_string(tag_face, "tag_face")
  if (!is.numeric(widths) || is.null(names(widths)) || !"single" %in% names(widths)) {
    cli::cli_abort("{.arg widths} must be a named numeric vector including {.val single}.")
  }
  base <- .venue_registry$generic
  new <- base
  new$name <- name
  new$journal <- journal %||% name
  new$publisher <- publisher
  new$widths <- widths
  new$height_max <- height_max
  new$text_min <- text_min
  new$text_max <- text_max
  new$tag_size <- tag_size
  new$tag_case <- tag_case
  new$tag_face <- tag_face
  new$families <- families
  new$source <- source
  new$verified <- verified
  new$retrieved <- retrieved
  extra <- list(...)
  for (nm in names(extra)) new[[nm]] <- extra[[nm]]
  key <- vellum_key(name)
  assign(key, new, envir = .venue_user)
  cli::cli_inform("Registered venue {.val {name}}{if (verified) '' else ' (unverified)'}.")
  invisible(new)
}

#' @rdname venue_register
#' @export
venue_reset <- function() {
  rm(list = ls(.venue_user), envir = .venue_user)
  invisible(TRUE)
}

# Emit the unverified-spec warning exactly once per venue per session.
warn_unverified <- function(spec) {
  if (isTRUE(spec$verified)) return(invisible(FALSE))
  warn_once(
    paste0("unverified-venue-", spec$name),
    c(
      "Venue {.val {spec$name}} ships an {.strong unverified} specification.",
      i = if (!is.na(spec$source)) "Confirm against {.url {spec$source}}." else
        "No source URL is recorded for this venue.",
      i = "Correct it permanently with {.fn venue_register}.",
      i = "{.val nature} and {.val plos} are verified against current publisher guidance."
    )
  )
}

#' @export
print.vellum_venue <- function(x, ...) {
  cli::cli_h1("{x$journal}{if (!is.na(x$publisher)) paste0('  \u00b7  ', x$publisher) else ''}")

  cli::cli_text("{.strong Geometry}")
  w <- x$widths
  cli::cli_bullets(c("*" = paste0(
    paste(sprintf("%s %g mm", names(w), unname(w)), collapse = "   \u00b7   "),
    "   \u00b7   max height ", x$height_max, " mm"
  )))

  cli::cli_text("{.strong Typography}")
  cli::cli_bullets(c("*" = sprintf(
    "text %g\u2013%g pt   \u00b7   panel labels %g pt %s %s   \u00b7   %s",
    x$text_min, x$text_max, x$tag_size, x$tag_face,
    if (x$tag_case == "lower") "a, b, c" else "A, B, C",
    paste(x$families, collapse = " / ")
  )))

  cli::cli_text("{.strong Output}")
  cli::cli_bullets(c("*" = sprintf(
    "%s   \u00b7   dpi line %g / combination %g / halftone %g   \u00b7   prefer %s%s",
    x$colour_space, x$dpi[["line"]], x$dpi[["combination"]], x$dpi[["halftone"]],
    paste(x$formats_preferred, collapse = ", "),
    if (length(x$formats_rejected))
      paste0("   \u00b7   rejects ", paste(x$formats_rejected, collapse = ", ")) else ""
  )))

  cli::cli_text("{.strong Provenance}")
  if (isTRUE(x$verified)) {
    cli::cli_alert_success("Verified from the publisher's artwork guide{if (!is.na(x$retrieved)) paste0(', retrieved ', x$retrieved) else ''}.")
  } else {
    cli::cli_alert_warning("Unverified working default \u2014 confirm before submission.")
  }
  if (!is.na(x$source)) cli::cli_bullets(c(" " = "{.url {x$source}}"))

  if (length(x$notes)) {
    cli::cli_text("{.strong Notes}")
    cli::cli_bullets(stats::setNames(x$notes, rep("*", length(x$notes))))
  }
  invisible(x)
}
