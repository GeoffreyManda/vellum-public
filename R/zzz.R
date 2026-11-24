.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    # Silence the one-off note about chart forms with known perceptual problems.
    vellum.quiet_chart_advice = FALSE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}

.onUnload <- function(libpath) {
  the$ramp_cache <- list()
  invisible()
}
