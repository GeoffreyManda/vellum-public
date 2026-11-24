# Anatomical and biological colour convention.
#
# Medical illustration has conventions a reader arrives already knowing. Arteries
# are red and veins are blue, nerves are yellow, lymphatics are green, bone is a
# warm off-white. A figure that inverts them is not adventurous, it is misread.
#
# What follows is a statement of convention, not of standard. There is no
# published normative specification assigning hex values to anatomical structures
# the way a journal specifies a column width, and this package will not manufacture
# the authority of one. Every entry therefore carries `basis = "convention"`.
#
# This is a lexicon, not a categorical palette, and the difference is not
# pedantic. Seventeen colours cannot all be mutually distinguishable under
# deuteranopia -- that is arithmetic, not a defect -- and `anatomy_audit()` on the
# whole set duly fails. Audit the subset a figure actually uses.
#
# Nor is every member readable on its own. Bone, adipose, connective tissue,
# urinary structures and serous fluid are pale by convention and reach less than
# 2:1 against white, which is correct for a large area fill and useless for a thin
# one. Rather than darken them into something a reader would not recognise, each
# carries `needs_outline = TRUE`, so a figure can be told programmatically which
# fills cannot carry their own boundary.
#
# What the values do guarantee: structures that co-occur in a figure are
# separated, every colour not flagged `needs_outline` clears 3:1 against white,
# and arterial and venous carry a greyscale separation of 21 dE00, because the
# pair most likely to be printed in black and white is exactly the pair a reader
# most needs to tell apart.
#
# Use `identity_*` and the venue palettes for data. This is for the parts of an
# illustration that depict a thing rather than encode a number.

.anatomy_conventions <- list(
  arterial = list(
    colour = "#CE3B3B", label = "Arterial blood, systemic arteries",
    note = "Oxygenated blood and arterial vessels. Lightened from a deep arterial red so that it separates from venous blue in greyscale."),
  venous = list(
    colour = "#14335C", label = "Venous blood, systemic veins",
    note = "Deoxygenated blood and venous vessels. Darkened for the same reason."),
  capillary = list(
    colour = "#8E3A6B", label = "Capillary bed",
    note = "Drawn between the arterial and venous hues where a bed is shown as a transition."),
  nerve = list(
    colour = "#E0A800", label = "Nerve, neural tissue",
    note = "Conventional yellow. Darkened from a pure yellow to stay readable against white."),
  lymphatic = list(
    colour = "#3F8F5B", label = "Lymphatic vessel, lymphoid tissue",
    note = "Conventional green."),
  bone = list(
    colour = "#E8DFC8", label = "Bone, cartilage matrix",
    note = "A warm off-white. Requires an outline; it cannot carry a boundary on its own."),
  muscle = list(
    colour = "#A34B4B", label = "Skeletal and smooth muscle",
    note = "Deliberately distinct from arterial red, which it is otherwise confused with."),
  connective = list(
    colour = "#C9B79C", label = "Connective tissue, fascia",
    note = "A neutral tan that recedes behind the structures it supports."),
  epithelium = list(
    colour = "#D9A38C", label = "Epithelium, mucosa",
    note = "A muted flesh tone; see `anatomy_skin()` for depicted human skin."),
  adipose = list(
    colour = "#EBC96A", label = "Adipose tissue",
    note = "Distinguished from nerve yellow by lower chroma and higher lightness."),
  respiratory = list(
    colour = "#7FB2D4", label = "Airway, alveolar tissue",
    note = "A lighter blue than venous, so a lung is not read as a vein."),
  digestive = list(
    colour = "#C98A4B", label = "Gastrointestinal tract",
    note = ""),
  urinary = list(
    colour = "#D4C24A", label = "Renal and urinary structures",
    note = ""),
  csf = list(
    colour = "#9FC6D8", label = "Cerebrospinal fluid, serous fluid",
    note = "Pale and low-chroma, because a fluid space should not out-weigh tissue."),
  pathogen = list(
    colour = "#6E4C9B", label = "Pathogen, virus, bacterium",
    note = "Purple by convention in infection figures, and separable from host tissue."),
  immune = list(
    colour = "#2F8F8F", label = "Immune cell",
    note = "Teal, kept clear of the lymphatic green it would otherwise merge with."),
  tumour = list(
    colour = "#7A3E8F", label = "Neoplastic tissue",
    note = "Distinct from pathogen purple in lightness so the two can co-occur.")
)

#' Conventional colours for anatomical and biological structures
#'
#' @description
#' The colours a reader of a medical illustration already expects: arterial red,
#' venous blue, nerve yellow, lymphatic green. Supplying them as a named,
#' documented set means an illustration recoloured for a figure keeps the meanings
#' its reader brings to it.
#'
#' These are conventions, not a standard, and the package says so. No normative
#' specification assigns hex values to anatomical structures, so what is offered
#' here is conventional usage resolved into specific values.
#'
#' It is a lexicon rather than a categorical palette. Seventeen colours cannot all
#' be mutually separable under colour-vision deficiency, so [anatomy_audit()] on
#' the whole set fails by construction; audit the subset a given figure uses.
#'
#' Nor is every colour readable alone. The pale tissue fills -- bone, adipose,
#' connective, urinary, serous fluid -- reach under 2:1 against white, which is
#' right for a large area and wrong for a thin one, so each is flagged
#' `needs_outline` rather than darkened into a colour a reader would not
#' recognise. Every colour not so flagged clears 3:1, and arterial and venous
#' carry a greyscale separation of 21 dE00 so the pair survives monochrome
#' printing.
#'
#' @param structure One or more structure names. `NULL` returns the whole table.
#'   See the `Structures` section.
#'
#' @section Structures:
#' `arterial`, `venous`, `capillary`, `nerve`, `lymphatic`, `bone`, `muscle`,
#' `connective`, `epithelium`, `adipose`, `respiratory`, `digestive`, `urinary`,
#' `csf`, `pathogen`, `immune`, `tumour`.
#'
#' @return With `structure`, a named character vector of colours. Without it, a
#'   data frame of every structure with its colour, label, note, `needs_outline`
#'   flag and basis.
#'
#' @seealso [anatomy_recolour()] to apply these to an illustration,
#'   [anatomy_audit()] to check the set under colour-vision deficiency, and
#'   [palette_audit()] for the general case.
#' @export
#' @examples
#' anatomy_palette()
#' anatomy_palette(c("arterial", "venous"))
anatomy_palette <- function(structure = NULL) {
  if (is.null(structure)) {
    return(data.frame(
      structure = names(.anatomy_conventions),
      colour = vapply(.anatomy_conventions, `[[`, character(1), "colour"),
      label = vapply(.anatomy_conventions, `[[`, character(1), "label"),
      note = vapply(.anatomy_conventions, `[[`, character(1), "note"),
      needs_outline = vapply(.anatomy_conventions, function(e) {
        contrast_ratio(e$colour, "#FFFFFF") < 3
      }, logical(1)),
      basis = "convention",
      stringsAsFactors = FALSE, row.names = NULL))
  }
  if (!is.character(structure)) cli::cli_abort("{.arg structure} must be character.")
  unknown <- setdiff(structure, names(.anatomy_conventions))
  if (length(unknown)) {
    cli::cli_abort(c("Unknown structure{?s} {.val {unknown}}.",
                     i = "Available: {.val {names(.anatomy_conventions)}}."))
  }
  stats::setNames(
    vapply(.anatomy_conventions[structure], `[[`, character(1), "colour"),
    structure)
}

#' Assign anatomical convention colours to an illustration
#'
#' @description
#' Maps the colours an illustration already uses onto conventional anatomical
#' colours. Where an illustration's own colours already follow the convention this
#' changes little; where it does not, this is what makes a borrowed drawing read
#' correctly beside the rest of a figure.
#'
#' The mapping is explicit by design. Automatic inference of which path in an SVG
#' is an artery would be guesswork dressed as anatomy, and the corpora carry no
#' semantic layer to infer it from -- their groups and identifiers are drawing-tool
#' artefacts, not labelled structures. So the caller states the mapping and the
#' package records it.
#'
#' @param x A `vellum_svg` illustration.
#' @param map A named character vector of `"#RRGGBB" = "structure"`, mapping a
#'   colour in the drawing to a structure name. Use [svg_palette()] to see what
#'   colours the drawing contains.
#' @param preserve Colours left alone. Defaults to white.
#'
#' @return A `vellum_svg` object, with the mapping recorded in its
#'   `anatomy` component so a figure can report how it was coloured.
#' @export
#' @examples
#' svg <- svg_read(paste0(
#'   '<svg viewBox="0 0 10 10">',
#'   '<rect width="4" height="10" fill="#CC0000"/>',
#'   '<rect x="5" width="4" height="10" fill="#0000CC"/></svg>'))
#' svg_palette(svg)
#' out <- anatomy_recolour(svg, c("#CC0000" = "arterial", "#0000CC" = "venous"))
#' svg_palette(out)
anatomy_recolour <- function(x, map, preserve = "#FFFFFF") {
  if (!inherits(x, "vellum_svg")) {
    cli::cli_abort("{.arg x} must come from {.fn svg_read} or {.fn vellum_asset}.")
  }
  if (!is.character(map) || is.null(names(map)) || !length(map)) {
    cli::cli_abort(c("{.arg map} must be a named character vector.",
                     i = 'Each entry maps a colour to a structure, as {.code "#CC0000" = "arterial"}.'))
  }
  unknown <- setdiff(unname(map), names(.anatomy_conventions))
  if (length(unknown)) {
    cli::cli_abort(c("Unknown structure{?s} {.val {unknown}}.",
                     i = "See {.fn anatomy_palette} for the available structures."))
  }
  present <- toupper(svg_palette(x)$colour)
  absent <- setdiff(toupper(names(map)), present)
  if (length(absent)) {
    cli::cli_warn(c(
      "{length(absent)} mapped colour{?s} {?is/are} not in the drawing: {.val {absent}}.",
      i = "Run {.fn svg_palette} to see the colours it actually uses."))
  }
  target <- stats::setNames(unname(anatomy_palette(unname(map))), names(map))
  out <- asset_recolour(x, map = target, preserve = preserve)
  out$anatomy <- data.frame(
    from = toupper(names(map)), structure = unname(map),
    to = unname(target), stringsAsFactors = FALSE, row.names = NULL)
  out
}

#' Audit the anatomical convention colours
#'
#' @description
#' Runs the package's own palette audit over the convention set, so the claim that
#' these colours survive colour-vision deficiency and greyscale is checked rather
#' than asserted. The arterial and venous pair is reported separately, because it
#' is the pair a reader most needs to distinguish and the one most often printed
#' in black and white.
#'
#' @param structure Structures to audit. Defaults to the whole set.
#' @return A `vellum_audit` object, with the arterial-venous separation in the
#'   `arteriovenous` attribute.
#' @export
#' @examples
#' anatomy_audit(c("arterial", "venous", "nerve", "lymphatic"))
anatomy_audit <- function(structure = NULL) {
  cols <- if (is.null(structure)) {
    stats::setNames(anatomy_palette()$colour, anatomy_palette()$structure)
  } else {
    anatomy_palette(structure)
  }
  out <- palette_audit(unname(cols), type = "qualitative")
  attr(out, "arteriovenous") <- c(
    colour = delta_e(anatomy_palette("arterial"), anatomy_palette("venous")),
    greyscale = delta_e(cvd(anatomy_palette("arterial"), "grey"),
                        cvd(anatomy_palette("venous"), "grey")))
  out
}

#' Skin tones for depicted people
#'
#' @description
#' A graded set for the human figures that appear in clinical, public-health and
#' patient-facing illustration.
#'
#' Two points are not cosmetic. A figure set that shows only light skin makes an
#' implicit and usually false claim about who a study or a service is about, and
#' in dermatological or clinical-sign illustration it is a straightforward
#' accuracy failure, because a sign presents differently across skin tones. And a
#' skin tone is a depiction, never a category: never map a study variable onto
#' this scale, which is why it is a plain vector rather than a registered
#' palette or a scale function.
#'
#' The values are an even progression in perceptual lightness at constant
#' moderate chroma, so a figure using several of them reads as one drawing.
#'
#' @param n Number of tones, from 2 to 10.
#' @return A character vector of colours, light to dark.
#' @export
#' @examples
#' anatomy_skin(6)
anatomy_skin <- function(n = 6) {
  check_number(n, "n", min = 2, max = 10)
  # An even walk in lightness rather than a set of sampled photographs: the point
  # is that consecutive tones are equally distinguishable, which sampling does
  # not give you.
  L <- seq(0.86, 0.30, length.out = as.integer(n))
  C <- 0.055 + 0.030 * sin(seq(0, pi, length.out = as.integer(n)))
  from_polar(cbind(L = L, C = C, h = rep(58, as.integer(n))))
}
