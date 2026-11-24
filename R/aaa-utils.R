#' @keywords internal
#' @importFrom rlang %||%
NULL

# ---- argument checking -------------------------------------------------------

check_number <- function(x, arg, min = NULL, max = NULL, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number, not {.obj_type_friendly {x}}.",
                   call = call)
  }
  if (!is.null(min) && x < min) {
    cli::cli_abort("{.arg {arg}} must be at least {min}, not {x}.", call = call)
  }
  if (!is.null(max) && x > max) {
    cli::cli_abort("{.arg {arg}} must be at most {max}, not {x}.", call = call)
  }
  invisible(x)
}

check_numeric <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || !length(x) || any(!is.finite(x))) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector of finite values, not {.obj_type_friendly {x}}.",
                   call = call)
  }
  invisible(x)
}

# A physical measurement, which may be a vector and may be missing.
#
# Distinct from `check_number()` because the unit converters are used both on a
# single theme constant and on a whole vector of measured weights, and distinct
# from `check_numeric()` because a measurement that does not exist -- a shape
# with no stroke -- is legitimately NA and should convert to NA rather than
# abort.
check_measure <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be numeric, not {.obj_type_friendly {x}}.", call = call)
  }
  if (any(is.infinite(x))) {
    cli::cli_abort("{.arg {arg}} must be finite.", call = call)
  }
  invisible(x)
}

check_string <- function(x, arg, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}.",
                   call = call)
  }
  invisible(x)
}

check_bool <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be {.code TRUE} or {.code FALSE}.", call = call)
  }
  invisible(x)
}

check_colour <- function(x, arg, call = rlang::caller_env()) {
  if (!is.character(x) || !length(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector of colours.", call = call)
  }
  bad <- vapply(x, function(cl) inherits(try(grDevices::col2rgb(cl), silent = TRUE), "try-error"),
                logical(1))
  if (any(bad)) {
    cli::cli_abort("{.arg {arg}} contains values that are not colours: {.val {x[bad]}}.", call = call)
  }
  invisible(x)
}

check_installed <- function(pkg, why, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("The {.pkg {pkg}} package is required {why}.", call = call)
  }
  invisible(TRUE)
}

has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# ---- once-per-session messaging ---------------------------------------------

the <- new.env(parent = emptyenv())
the$warned <- character()
the$ramp_cache <- list()

warn_once <- function(id, ..., .envir = parent.frame()) {
  if (id %in% the$warned) return(invisible(FALSE))
  the$warned <- c(the$warned, id)
  cli::cli_warn(..., .envir = .envir)
  invisible(TRUE)
}

inform_once <- function(id, ..., .envir = parent.frame()) {
  if (id %in% the$warned) return(invisible(FALSE))
  the$warned <- c(the$warned, id)
  cli::cli_inform(..., .envir = .envir)
  invisible(TRUE)
}

#' Reset vellum's once-per-session notices
#'
#' `vellum` deliberately warns only once per session about unverified venue
#' specifications and about chart forms with known perceptual problems, so that
#' scripted use is not noisy. Call this to hear them again, which is useful in
#' tests and in teaching.
#'
#' @return Invisibly, the identifiers that were cleared.
#' @export
#' @examples
#' vellum_reset_notices()
vellum_reset_notices <- function() {
  old <- the$warned
  the$warned <- character()
  invisible(old)
}

# ---- small helpers -----------------------------------------------------------

`%none%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x

seq_between <- function(from, to, n) {
  if (n == 1L) return(to)
  from + (to - from) * seq(0, 1, length.out = n)
}

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# Recycle a value to length n with a clear error rather than silent truncation.
recycle_to <- function(x, n, arg, call = rlang::caller_env()) {
  if (length(x) == n) return(x)
  if (length(x) == 1L) return(rep(x, n))
  cli::cli_abort("{.arg {arg}} must have length 1 or {n}, not {length(x)}.", call = call)
}
