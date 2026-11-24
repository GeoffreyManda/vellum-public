# Numeric and textual typography.
#
# Axis labels are typeset text, and the conventions are not decorative. A
# hyphen-minus (U+002D) is not a minus sign (U+2212): it is shorter and sits at a
# different height, and at 6 pt that difference is visible on the page. A comma
# as a thousands separator is ambiguous internationally, which is why SI
# recommends a thin space. A unit belongs a thin space away from its value, and
# must not break across a line.
#
# All non-ASCII characters here are written as \u escapes so the package is
# encoding-independent.

MINUS  <- "\u2212"  # U+2212 MINUS SIGN
THIN   <- "\u2009"  # U+2009 THIN SPACE
NBTHIN <- "\u202f"  # U+202F NARROW NO-BREAK SPACE
TIMES  <- "\u00d7"  # U+00D7 MULTIPLICATION SIGN
ENDASH <- "\u2013"  # U+2013 EN DASH

#' Typographically correct number labels
#'
#' @description
#' A drop-in axis labeller that fixes the three things `scales::label_number()`
#' leaves wrong for print: it uses a true minus sign rather than a hyphen, a thin
#' space rather than a comma for digit grouping, and it never emits `-0`.
#'
#' @param digits Decimal places. `NULL` chooses the smallest number that
#'   distinguishes the supplied breaks, which avoids both `1.0, 1.5, 2.0` being
#'   printed as `1, 2, 2` and `1, 2, 3` being printed as `1.00, 2.00, 3.00`.
#' @param big_mark Digit-group separator. Defaults to a thin space, per SI.
#'   Pass `""` for none, which is preferable below five digits.
#' @param prefix,suffix Strings placed before and after the number.
#' @param minus Use a true minus sign. Leave `TRUE`.
#'
#' @return A labelling function for a ggplot2 scale's `labels` argument.
#' @export
#' @examples
#' f <- label_vellum()
#' f(c(-1500, 0, 1500, 25000))
label_vellum <- function(digits = NULL, big_mark = THIN, prefix = "", suffix = "",
                         minus = TRUE) {
  function(x) {
    x <- as.numeric(x)
    d <- digits %||% auto_digits(x)
    # The SI Brochure applies the group separator only when the integer part has more
    # than four digits, which is why a year is 2024 and not 2 024. Applying it at four
    # digits is the commonest way a correct-looking axis reads wrong.
    mark <- if (max(abs(x[is.finite(x)]), 0) < 1e5) "" else big_mark
    out <- formatC(abs(x), format = "f", digits = d, big.mark = mark, mode = "double")
    out <- trimws(out)
    sign_str <- ifelse(!is.na(x) & x < 0 & round(abs(x), d) > 0,
                       if (minus) MINUS else "-", "")
    out <- paste0(sign_str, prefix, out, suffix)
    out[is.na(x)] <- NA_character_
    out
  }
}

# The smallest number of decimals that is both DISTINCT and FAITHFUL, capped so a
# floating-point artefact cannot produce a nine-decimal axis.
#
# Distinctness alone is not enough, and getting this wrong is subtle. Breaks of
# -5, -2.5, 0, 2.5, 5 round at zero decimals to -5, -2, 0, 2, 5: all distinct, so a
# duplicate test accepts them, and the axis then claims a tick sits at 2 when it
# sits at 2.5. Faithfulness has to be tested too.
auto_digits <- function(x, max_digits = 6L) {
  x <- x[is.finite(x)]
  if (!length(x)) return(0L)
  tol <- 1e-9 * max(1, max(abs(x)))
  for (d in 0:max_digits) {
    r <- round(x, d)
    if (!anyDuplicated(r) && max(abs(r - x)) <= tol) return(d)
  }
  # No exact representation within the cap, as for a genuinely irrational break.
  # Fall back to distinctness, which at least keeps the labels different.
  for (d in 0:max_digits) if (!anyDuplicated(round(x, d))) return(d)
  max_digits
}

#' Labels with SI prefixes and correctly spaced units
#'
#' @description
#' Formats large and small magnitudes with SI prefixes, placing a narrow
#' no-break space between value and unit as BIPM requires, so a unit can never be
#' orphaned onto the next line.
#'
#' @param unit Unit symbol, for example `"g"`, `"m"`, `"s"`. Note that SI symbols
#'   are not abbreviations and take no full stop.
#' @param digits Significant digits after scaling.
#' @return A labelling function.
#' @export
#' @examples
#' label_si("g")(c(0.002, 1, 1500, 2.4e6))
label_si <- function(unit = "", digits = 3) {
  prefixes <- c("y", "z", "a", "f", "p", "n", "\u00b5", "m", "",
                "k", "M", "G", "T", "P", "E", "Z", "Y")
  powers <- seq(-24, 24, by = 3)
  function(x) {
    x <- as.numeric(x)
    out <- character(length(x))
    for (i in seq_along(x)) {
      v <- x[i]
      if (is.na(v)) { out[i] <- NA_character_; next }
      if (v == 0) {
        out[i] <- paste0("0", if (nzchar(unit)) paste0(NBTHIN, unit) else "")
        next
      }
      k <- clamp(3 * floor(log10(abs(v)) / 3), min(powers), max(powers))
      p <- prefixes[match(k, powers)]
      scaled <- v / 10^k
      txt <- formatC(abs(scaled), format = "fg", digits = digits, flag = "#")
      txt <- sub("\\.$", "", trimws(txt))
      out[i] <- paste0(if (v < 0) MINUS else "", txt,
                       if (nzchar(paste0(p, unit))) paste0(NBTHIN, p, unit) else "")
    }
    out
  }
}

#' Powers-of-ten labels for log axes
#'
#' Renders `10^n` as an expression rather than as `1e+05`, which is the single
#' most common blemish on an otherwise finished log axis.
#'
#' @param base Log base. `10` or `2`.
#' @return A labelling function returning an expression vector.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point() +
#'   scale_y_log10(labels = label_log())
label_log <- function(base = 10) {
  function(x) {
    exps <- log(as.numeric(x), base = base)
    lab <- vapply(exps, function(e) {
      if (is.na(e)) return("NA")
      e <- round(e, 8)
      sprintf("%g^%s", base, format(e))
    }, character(1))
    parse(text = lab)
  }
}

#' Exact P-value labels
#'
#' @description
#' Reports P values as numbers, in the form journals ask for. A cloud of
#' asterisks compresses a continuous quantity into three bins, discards the
#' direction and magnitude of the effect, and cannot be re-analysed; it is not an
#' acceptable primary presentation of a result.
#'
#' Below the smallest reportable value the label becomes `P < 0.001` rather than
#' `P = 0`, since a P value is never exactly zero.
#'
#' @param p A numeric vector of P values. Omit it to get a labelling function
#'   suitable for a scale's `labels` argument.
#' @param digits Decimal places for values at or above `floor`.
#' @param floor Smallest value reported exactly.
#' @param italic_p Set the `P` in italic, per most style guides. Returns an
#'   expression when `TRUE`.
#' @param prefix Text before the value, usually `"P"`.
#' @return A character vector, or an expression vector when `italic_p = TRUE`.
#' @export
#' @examples
#' label_pval(c(0.4823, 0.032, 0.0004))
#' label_pval(0.032, italic_p = TRUE)
label_pval <- function(p, digits = 3, floor = 0.001, italic_p = FALSE, prefix = "P") {
  if (missing(p)) {
    return(function(x) label_pval(x, digits = digits, floor = floor,
                                  italic_p = italic_p, prefix = prefix))
  }
  p <- as.numeric(p)
  txt <- ifelse(
    is.na(p), NA_character_,
    ifelse(p < floor,
           paste0("< ", formatC(floor, format = "f", digits = digits)),
           paste0("= ", formatC(p, format = "f", digits = digits)))
  )
  # `paste()` would render a missing P value as the literal "P NA", which reads as
  # a measurement rather than as its absence.
  if (!italic_p) return(ifelse(is.na(txt), NA_character_, paste(prefix, txt)))
  # An expression vector cannot hold NA_character_, so a missing value becomes the
  # `NA` literal, which is what ggplot2 drops from a set of labels.
  code <- ifelse(is.na(txt), "NA", sprintf("italic(%s)*' %s'", prefix, txt))
  parse(text = code)
}

#' Format an estimate with its uncertainty interval
#'
#' @description
#' Produces the standard journal form for an effect estimate. The separator is
#' the word `to` rather than an en dash, because with negative bounds a dash-joined
#' range collapses into a run of dashes that cannot be parsed by eye, whereas
#' `-0.72 to -0.08` can.
#'
#' @param estimate,lower,upper Numeric vectors.
#' @param digits Decimal places.
#' @param level Interval level, used only for the `attr` label.
#' @param sep Separator between bounds.
#' @return A character vector.
#' @export
#' @examples
#' fmt_estimate(0.82, 0.71, 0.95)
#' fmt_estimate(-0.4, -0.72, -0.08)
fmt_estimate <- function(estimate, lower, upper, digits = 2, level = 0.95, sep = " to ") {
  f <- label_vellum(digits = digits, big_mark = "")
  out <- paste0(f(estimate), " (", f(lower), sep, f(upper), ")")
  attr(out, "interval") <- sprintf("%g%% interval", 100 * level)
  out
}

#' Italicise scientific names for plot labels
#'
#' Binomial names are set in italic by convention in every biological journal.
#' This returns an expression suitable for a scale's `labels`, leaving any
#' non-binomial text upright.
#'
#' @param x A character vector, for example `"Escherichia coli"`.
#' @param upright Words to leave upright, such as `"sp."`, `"spp."`, `"cf."`.
#' @return An expression vector.
#' @export
#' @examples
#' italicise(c("Escherichia coli", "Vibrio cholerae"))
italicise <- function(x, upright = c("sp.", "spp.", "cf.", "aff.", "var.", "subsp.")) {
  exprs <- vapply(x, function(s) {
    words <- strsplit(s, "\\s+")[[1]]
    parts <- vapply(words, function(w) {
      # An apostrophe in a name -- and every backslash -- would otherwise close or
      # break the plotmath string literal and make the whole label unparseable.
      lit <- encodeString(w, quote = "'")
      if (w %in% upright) sprintf("plain(%s)", lit) else sprintf("italic(%s)", lit)
    }, character(1))
    paste(parts, collapse = "*' '*")
  }, character(1))
  parse(text = exprs)
}

#' Wrap long axis or facet labels
#'
#' Rotating a label is the worst available fix for overlong text: rotated text is
#' measurably slower to read. Wrapping keeps it horizontal.
#'
#' @param width Target characters per line.
#' @return A labelling function.
#' @export
#' @examples
#' label_wrap(12)("Adjusted mean difference in weight-for-height z score")
label_wrap <- function(width = 20) {
  function(x) vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

#' Wrap labels to a physical width
#'
#' Character counts are a poor proxy for label width: ten `W`s and ten `i`s do
#' not occupy the same space. This labeller measures candidate lines in the
#' selected font and wraps them to a width in millimetres, so labels remain
#' predictable when a figure is exported at final size.
#'
#' @param width_mm Maximum line width in millimetres.
#' @param size Text size in points.
#' @param family Font family. Defaults to Vellum's resolved sans-serif family.
#' @return A labelling function suitable for a discrete scale.
#' @export
#' @examples
#' wrap <- label_wrap_mm(22, size = 7)
#' wrap(c("A short label", "A substantially longer scientific category"))
label_wrap_mm <- function(width_mm, size = 7, family = NULL) {
  check_number(width_mm, "width_mm", min = 1)
  check_number(size, "size", min = 1)
  family <- family %||% as.character(resolve_font())
  check_string(family, "family")
  width_pt <- mm_pt(width_mm)

  one <- function(s) {
    if (is.na(s)) return(NA_character_)
    words <- strsplit(as.character(s), "\\s+")[[1]]
    if (!length(words)) return("")
    lines <- character()
    current <- words[1]
    if (length(words) > 1L) {
      for (word in words[-1]) {
        candidate <- paste(current, word)
        measured <- try(
          systemfonts::string_width(candidate, family = family, size = size),
          silent = TRUE
        )
        if (inherits(measured, "try-error")) {
          measured <- nchar(candidate) * size * 0.52
        }
        if (measured <= width_pt) {
          current <- candidate
        } else {
          lines <- c(lines, current)
          current <- word
        }
      }
    }
    paste(c(lines, current), collapse = "\n")
  }
  function(x) vapply(x, one, character(1), USE.NAMES = FALSE)
}
