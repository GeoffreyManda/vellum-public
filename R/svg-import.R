# SVG import.
#
# A scientific illustration arrives as SVG, and there are two ways to put one in
# an R figure. The common way is to rasterise it and draw the pixels. That is a
# quiet betrayal: the panel next to it is vector, the journal asks for vector line
# art, and the illustration is the one element that will pixelate in production
# and be sent back at proof stage.
#
# This file does the other thing. It reads the SVG, resolves the cascade the way
# a renderer would, flattens curves to a tolerance expressed in device units, and
# emits grid grobs. What comes out is real geometry: it goes into a cairo PDF as
# paths, scales without loss, and can be measured. Because the geometry is real,
# the stroke weights are real too, which is what lets `venue_check()` say that a
# hairline inside an imported illustration falls below a publisher's minimum --
# a check that is impossible once the artwork has become a bitmap.
#
# The subset of SVG implemented here is the subset these corpora actually use,
# established by profiling the assets rather than by reading the specification and
# guessing: paths, the four primitive shapes, groups and transforms, CSS class
# selectors in a `<style>` block, presentation attributes, inline styles,
# gradients, clipping paths, and `<use>`. Raster `<image>` elements are recorded
# rather than drawn, because an embedded bitmap is exactly the thing the caller
# needs to be told about.

# ---- CSS ---------------------------------------------------------------------

# A minimal CSS parser. It handles what an illustration tool writes -- rules keyed
# by class, id, or element name, several selectors sharing one block -- and
# nothing more. Anything it does not understand is dropped rather than guessed at.
svg_parse_css <- function(text) {
  if (!length(text) || !any(nzchar(text))) return(list())
  css <- paste(text, collapse = "\n")
  css <- gsub("/\\*.*?\\*/", " ", css)
  blocks <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
  rules <- list()
  for (b in blocks) {
    sel <- trimws(sub("\\{.*$", "", b))
    body <- sub("^[^{]*\\{", "", sub("\\}\\s*$", "", b))
    decls <- svg_parse_declarations(body)
    if (!length(decls)) next
    for (s in trimws(strsplit(sel, ",")[[1]])) {
      if (!nzchar(s)) next
      rules[[s]] <- utils::modifyList(rules[[s]] %||% list(), decls)
    }
  }
  rules
}

svg_parse_declarations <- function(text) {
  if (!length(text) || !nzchar(trimws(text))) return(list())
  parts <- trimws(strsplit(text, ";")[[1]])
  parts <- parts[nzchar(parts)]
  out <- list()
  for (p in parts) {
    kv <- regmatches(p, regexec("^\\s*([A-Za-z-]+)\\s*:\\s*(.*)$", p))[[1]]
    if (length(kv) == 3L) out[[tolower(kv[2])]] <- trimws(kv[3])
  }
  out
}

# ---- colour ------------------------------------------------------------------

# Returns NA for "none" so that a missing fill and an absent fill stay
# distinguishable, and returns the gradient reference untouched so the caller can
# resolve it against the document's defs.
svg_colour <- function(value, current = "#000000") {
  if (is.null(value)) return(NULL)
  v <- trimws(tolower(value))
  if (!nzchar(v) || v %in% c("none", "transparent")) return(NA_character_)
  if (v == "currentcolor") return(current)
  if (grepl("^url\\(", v)) return(v)
  if (grepl("^#[0-9a-f]{3}$", v)) {
    ch <- strsplit(substring(v, 2), "")[[1]]
    return(toupper(paste0("#", paste0(ch, ch, collapse = ""))))
  }
  if (grepl("^#[0-9a-f]{6}$", v)) return(toupper(v))
  if (grepl("^#[0-9a-f]{8}$", v)) return(toupper(substring(v, 1, 7)))
  if (grepl("^rgba?\\(", v)) {
    nums <- as.numeric(regmatches(v, gregexpr("[0-9.]+%?", v))[[1]])
    pct <- grepl("%", regmatches(v, gregexpr("[0-9.]+%?", v))[[1]])
    if (length(nums) >= 3L) {
      rgb <- ifelse(pct[1:3], nums[1:3] / 100 * 255, nums[1:3])
      return(grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255))
    }
    return(NA_character_)
  }
  ok <- !inherits(try(grDevices::col2rgb(v), silent = TRUE), "try-error")
  if (ok) toupper(v) else NA_character_
}

svg_number <- function(value, default = NA_real_) {
  if (is.null(value) || !length(value)) return(default)
  v <- suppressWarnings(as.numeric(sub("(px|pt|mm|cm|in|%)$", "", trimws(value))))
  if (length(v) != 1L || is.na(v)) default else v
}

# ---- transforms --------------------------------------------------------------

# Transformation matrices are carried as c(a, b, c, d, e, f), the SVG ordering,
# so that a matrix() attribute is a straight copy and mistakes have one fewer
# place to hide.
svg_mat_identity <- function() c(1, 0, 0, 1, 0, 0)

svg_mat_multiply <- function(m, n) {
  c(m[1] * n[1] + m[3] * n[2],
    m[2] * n[1] + m[4] * n[2],
    m[1] * n[3] + m[3] * n[4],
    m[2] * n[3] + m[4] * n[4],
    m[1] * n[5] + m[3] * n[6] + m[5],
    m[2] * n[5] + m[4] * n[6] + m[6])
}

svg_mat_apply <- function(m, x, y) {
  list(x = m[1] * x + m[3] * y + m[5],
       y = m[2] * x + m[4] * y + m[6])
}

# The factor by which the matrix changes length. For an anisotropic transform no
# single factor is correct, and SVG itself defines stroke width against the
# geometric mean, so that is what is used.
svg_mat_scale <- function(m) {
  det <- abs(m[1] * m[4] - m[2] * m[3])
  if (!is.finite(det) || det <= 0) return(1)
  sqrt(det)
}

svg_parse_transform <- function(text) {
  if (is.null(text) || !length(text) || is.na(text) || !nzchar(trimws(text))) {
    return(svg_mat_identity())
  }
  ops <- regmatches(text, gregexpr("[A-Za-z]+\\s*\\([^)]*\\)", text))[[1]]
  m <- svg_mat_identity()
  for (op in ops) {
    name <- tolower(trimws(sub("\\(.*$", "", op)))
    args <- suppressWarnings(as.numeric(regmatches(
      op, gregexpr("[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)(?:[eE][+-]?[0-9]+)?", op))[[1]]))
    args <- args[is.finite(args)]
    step <- switch(
      name,
      matrix    = if (length(args) >= 6L) args[1:6] else NULL,
      translate = c(1, 0, 0, 1, args[1] %||% 0, if (length(args) >= 2L) args[2] else 0),
      scale     = c(args[1] %||% 1, 0, 0, if (length(args) >= 2L) args[2] else args[1] %||% 1, 0, 0),
      rotate    = svg_rotate_matrix(args),
      skewx     = c(1, 0, tan(args[1] * pi / 180), 1, 0, 0),
      skewy     = c(1, tan(args[1] * pi / 180), 0, 1, 0, 0),
      NULL
    )
    if (!is.null(step) && length(step) == 6L && all(is.finite(step))) {
      m <- svg_mat_multiply(m, step)
    }
  }
  m
}

svg_rotate_matrix <- function(args) {
  if (!length(args)) return(NULL)
  a <- args[1] * pi / 180
  rot <- c(cos(a), sin(a), -sin(a), cos(a), 0, 0)
  if (length(args) >= 3L) {
    to <- c(1, 0, 0, 1, args[2], args[3])
    back <- c(1, 0, 0, 1, -args[2], -args[3])
    svg_mat_multiply(svg_mat_multiply(to, rot), back)
  } else {
    rot
  }
}

# ---- path data ---------------------------------------------------------------

# The tokeniser is one regular expression because SVG path data is deliberately
# terse: "1.5.5" is two numbers, "1-2" is two numbers, and splitting on
# whitespace loses both. The one place a single expression is not enough is the
# elliptical arc, whose two flags may be written with no separator at all
# ("a1 1 0 0110 10"), so the cursor below can split a digit back off a token.
svg_tokenise_path <- function(d) {
  pattern <- "[MmZzLlHhVvCcSsQqTtAa]|[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)(?:[eE][+-]?[0-9]+)?"
  regmatches(d, gregexpr(pattern, d, perl = TRUE))[[1]]
}

svg_path_cursor <- function(tokens) {
  i <- 1L
  n <- length(tokens)
  list(
    done = function() i > n,
    peek = function() if (i > n) NA_character_ else tokens[i],
    is_command = function() i <= n && grepl("^[A-Za-z]$", tokens[i]),
    take = function() { tok <- tokens[i]; i <<- i + 1L; tok },
    number = function() {
      if (i > n) return(NA_real_)
      val <- suppressWarnings(as.numeric(tokens[i]))
      i <<- i + 1L
      val
    },
    # An arc flag is a single character. If the tokeniser glued it to the number
    # that followed, split it here and leave the remainder in place.
    flag = function() {
      if (i > n) return(NA_real_)
      tok <- tokens[i]
      if (nchar(tok) > 1L && substring(tok, 1, 1) %in% c("0", "1")) {
        tokens[i] <<- substring(tok, 2)
        return(as.numeric(substring(tok, 1, 1)))
      }
      i <<- i + 1L
      suppressWarnings(as.numeric(tok))
    }
  )
}

# Curve flattening.
#
# `tol` is a chord tolerance in user units, chosen by the caller from the size the
# illustration will actually be drawn at, so a 3 mm icon is not carrying the
# vertex count of a full-page plate. The segment count is derived from the control
# polygon's length, which bounds the curve's arc length, so tight curves get more
# points than slack ones without measuring curvature.
svg_curve_steps <- function(len, tol) {
  if (!is.finite(len) || len <= 0) return(2L)
  as.integer(min(96, max(3, ceiling(sqrt(len / max(tol, 1e-6)) * 1.6))))
}

svg_cubic <- function(p0, p1, p2, p3, tol) {
  len <- sum(abs(diff(c(p0[1], p1[1], p2[1], p3[1])))) +
    sum(abs(diff(c(p0[2], p1[2], p2[2], p3[2]))))
  n <- svg_curve_steps(len, tol)
  t <- seq(0, 1, length.out = n + 1L)[-1]
  mt <- 1 - t
  cbind(mt^3 * p0[1] + 3 * mt^2 * t * p1[1] + 3 * mt * t^2 * p2[1] + t^3 * p3[1],
        mt^3 * p0[2] + 3 * mt^2 * t * p1[2] + 3 * mt * t^2 * p2[2] + t^3 * p3[2])
}

svg_quadratic <- function(p0, p1, p2, tol) {
  len <- sum(abs(diff(c(p0[1], p1[1], p2[1])))) + sum(abs(diff(c(p0[2], p1[2], p2[2]))))
  n <- svg_curve_steps(len, tol)
  t <- seq(0, 1, length.out = n + 1L)[-1]
  mt <- 1 - t
  cbind(mt^2 * p0[1] + 2 * mt * t * p1[1] + t^2 * p2[1],
        mt^2 * p0[2] + 2 * mt * t * p1[2] + t^2 * p2[2])
}

# Endpoint to centre parameterisation, SVG 1.1 appendix F.6.5. The out-of-range
# radius correction in F.6.6 is applied rather than skipped, because real files
# do contain arcs whose radii are too small for their endpoints.
svg_arc <- function(p0, rx, ry, phi, large_arc, sweep, p1, tol) {
  if (!is.finite(rx) || !is.finite(ry) || rx == 0 || ry == 0) {
    return(matrix(p1, nrow = 1L))
  }
  rx <- abs(rx); ry <- abs(ry)
  phi <- phi * pi / 180
  dx2 <- (p0[1] - p1[1]) / 2
  dy2 <- (p0[2] - p1[2]) / 2
  x1p <- cos(phi) * dx2 + sin(phi) * dy2
  y1p <- -sin(phi) * dx2 + cos(phi) * dy2

  lambda <- x1p^2 / rx^2 + y1p^2 / ry^2
  if (lambda > 1) {
    s <- sqrt(lambda)
    rx <- rx * s
    ry <- ry * s
  }
  num <- rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2
  den <- rx^2 * y1p^2 + ry^2 * x1p^2
  coef <- if (den <= 0 || num <= 0) 0 else sqrt(num / den)
  if (large_arc == sweep) coef <- -coef
  cxp <- coef * rx * y1p / ry
  cyp <- -coef * ry * x1p / rx
  cx <- cos(phi) * cxp - sin(phi) * cyp + (p0[1] + p1[1]) / 2
  cy <- sin(phi) * cxp + cos(phi) * cyp + (p0[2] + p1[2]) / 2

  ang <- function(ux, uy, vx, vy) {
    dot <- ux * vx + uy * vy
    len <- sqrt(ux^2 + uy^2) * sqrt(vx^2 + vy^2)
    if (len <= 0) return(0)
    a <- acos(clamp(dot / len, -1, 1))
    if (ux * vy - uy * vx < 0) -a else a
  }
  theta1 <- ang(1, 0, (x1p - cxp) / rx, (y1p - cyp) / ry)
  dtheta <- ang((x1p - cxp) / rx, (y1p - cyp) / ry, (-x1p - cxp) / rx, (-y1p - cyp) / ry)
  if (sweep == 0 && dtheta > 0) dtheta <- dtheta - 2 * pi
  if (sweep == 1 && dtheta < 0) dtheta <- dtheta + 2 * pi

  n <- svg_curve_steps(abs(dtheta) * max(rx, ry), tol)
  t <- seq(theta1, theta1 + dtheta, length.out = n + 1L)[-1]
  cbind(cx + rx * cos(t) * cos(phi) - ry * sin(t) * sin(phi),
        cy + rx * cos(t) * sin(phi) + ry * sin(t) * cos(phi))
}

# Returns a list of subpaths, each a matrix of points, plus a logical vector
# recording which subpaths the path data itself closed. Closure matters: an
# unclosed subpath that is filled is closed implicitly for the fill but must not
# gain a stroke along the closing edge.
svg_parse_path <- function(d, tol = 0.05) {
  tokens <- svg_tokenise_path(d)
  if (!length(tokens)) return(list(subpaths = list(), closed = logical()))
  cur <- svg_path_cursor(tokens)

  subpaths <- list()
  closed <- logical()
  pts <- NULL
  start <- c(0, 0)
  pos <- c(0, 0)
  cmd <- ""
  prev_cubic <- NULL
  prev_quad <- NULL

  flush <- function(is_closed) {
    if (!is.null(pts) && nrow(pts) >= 2L) {
      subpaths[[length(subpaths) + 1L]] <<- pts
      closed[length(closed) + 1L] <<- is_closed
    }
    pts <<- NULL
  }
  push <- function(m) {
    if (is.null(m) || !length(m)) return(invisible(NULL))
    m <- m[stats::complete.cases(m), , drop = FALSE]
    if (nrow(m)) pts <<- rbind(pts, m)
    invisible(NULL)
  }

  while (!cur$done()) {
    if (cur$is_command()) {
      cmd <- cur$take()
    } else if (!nzchar(cmd)) {
      break
    } else if (cmd == "M") {
      cmd <- "L"
    } else if (cmd == "m") {
      cmd <- "l"
    }
    rel <- cmd %in% c("m", "l", "h", "v", "c", "s", "q", "t", "a")
    base <- if (rel) pos else c(0, 0)
    up <- toupper(cmd)

    if (up == "Z") {
      if (!is.null(pts)) {
        push(matrix(start, nrow = 1L))
        flush(TRUE)
      }
      pos <- start
      prev_cubic <- prev_quad <- NULL
      next
    }

    if (up == "M") {
      flush(FALSE)
      x <- cur$number() + base[1]
      y <- cur$number() + base[2]
      if (!is.finite(x) || !is.finite(y)) break
      pos <- c(x, y)
      start <- pos
      pts <- matrix(pos, nrow = 1L)
      prev_cubic <- prev_quad <- NULL
      next
    }

    if (is.null(pts)) pts <- matrix(pos, nrow = 1L)

    if (up == "L") {
      x <- cur$number() + base[1]; y <- cur$number() + base[2]
      if (!is.finite(x) || !is.finite(y)) break
      push(matrix(c(x, y), nrow = 1L)); pos <- c(x, y)
      prev_cubic <- prev_quad <- NULL
    } else if (up == "H") {
      x <- cur$number() + base[1]
      if (!is.finite(x)) break
      push(matrix(c(x, pos[2]), nrow = 1L)); pos <- c(x, pos[2])
      prev_cubic <- prev_quad <- NULL
    } else if (up == "V") {
      y <- cur$number() + base[2]
      if (!is.finite(y)) break
      push(matrix(c(pos[1], y), nrow = 1L)); pos <- c(pos[1], y)
      prev_cubic <- prev_quad <- NULL
    } else if (up %in% c("C", "S")) {
      if (up == "C") {
        c1 <- c(cur$number() + base[1], cur$number() + base[2])
      } else {
        c1 <- if (is.null(prev_cubic)) pos else 2 * pos - prev_cubic
      }
      c2 <- c(cur$number() + base[1], cur$number() + base[2])
      p <- c(cur$number() + base[1], cur$number() + base[2])
      if (!all(is.finite(c(c1, c2, p)))) break
      push(svg_cubic(pos, c1, c2, p, tol))
      prev_cubic <- c2; prev_quad <- NULL; pos <- p
    } else if (up %in% c("Q", "T")) {
      if (up == "Q") {
        c1 <- c(cur$number() + base[1], cur$number() + base[2])
      } else {
        c1 <- if (is.null(prev_quad)) pos else 2 * pos - prev_quad
      }
      p <- c(cur$number() + base[1], cur$number() + base[2])
      if (!all(is.finite(c(c1, p)))) break
      push(svg_quadratic(pos, c1, p, tol))
      prev_quad <- c1; prev_cubic <- NULL; pos <- p
    } else if (up == "A") {
      rx <- cur$number(); ry <- cur$number(); rot <- cur$number()
      laf <- cur$flag(); swf <- cur$flag()
      p <- c(cur$number() + base[1], cur$number() + base[2])
      if (!all(is.finite(c(rx, ry, rot, laf, swf, p)))) break
      push(svg_arc(pos, rx, ry, rot, laf, swf, p, tol))
      prev_cubic <- prev_quad <- NULL; pos <- p
    } else {
      break
    }
  }
  flush(FALSE)
  list(subpaths = subpaths, closed = closed)
}

# ---- shapes ------------------------------------------------------------------

svg_points_attr <- function(text) {
  nums <- suppressWarnings(as.numeric(regmatches(
    text, gregexpr("[+-]?(?:[0-9]*\\.[0-9]+|[0-9]+)(?:[eE][+-]?[0-9]+)?", text))[[1]]))
  nums <- nums[is.finite(nums)]
  if (length(nums) < 4L) return(NULL)
  n <- length(nums) %/% 2L
  matrix(nums[seq_len(2L * n)], ncol = 2L, byrow = TRUE)
}

svg_ellipse_points <- function(cx, cy, rx, ry, tol) {
  n <- svg_curve_steps(2 * pi * max(rx, ry), tol) * 4L
  n <- as.integer(min(360L, max(24L, n)))
  t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  cbind(cx + rx * cos(t), cy + ry * sin(t))
}

# A rounded rectangle is written out rather than approximated by a plain
# rectangle, because rounded corners are load-bearing in icon sets: squaring them
# changes the drawing.
svg_rect_points <- function(x, y, w, h, rx, ry, tol) {
  if (!is.finite(rx) || rx <= 0) rx <- 0
  if (!is.finite(ry) || ry <= 0) ry <- rx
  rx <- min(rx, w / 2); ry <- min(ry, h / 2)
  if (rx <= 0 || ry <= 0) {
    return(cbind(c(x, x + w, x + w, x, x), c(y, y, y + h, y + h, y)))
  }
  k <- svg_curve_steps(2 * pi * max(rx, ry) / 4, tol)
  corner <- function(cx, cy, a0, a1) {
    t <- seq(a0, a1, length.out = k)
    cbind(cx + rx * cos(t), cy + ry * sin(t))
  }
  rbind(
    cbind(x + rx, y),
    corner(x + w - rx, y + ry, -pi / 2, 0),
    cbind(x + w, y + h - ry),
    corner(x + w - rx, y + h - ry, 0, pi / 2),
    cbind(x + rx, y + h),
    corner(x + rx, y + h - ry, pi / 2, pi),
    cbind(x, y + ry),
    corner(x + rx, y + ry, pi, 3 * pi / 2)
  )
}

# ---- document traversal ------------------------------------------------------

svg_local_name <- function(node) {
  sub("^.*:", "", xml2::xml_name(node))
}

svg_attr <- function(node, name) {
  v <- xml2::xml_attr(node, name)
  if (length(v) != 1L || is.na(v)) NULL else v
}

# The inherited properties. Anything not in this list resets at each element,
# which is what the specification says and what stops a stroke width from one
# group silently governing the next.
.svg_inherited <- c("fill", "stroke", "stroke-width", "stroke-linecap",
                    "stroke-linejoin", "stroke-dasharray", "stroke-opacity",
                    "fill-opacity", "fill-rule", "color", "font-family",
                    "font-size", "font-weight", "font-style", "text-anchor",
                    "visibility")

.svg_presentation <- c(.svg_inherited, "opacity", "display", "clip-path",
                       "stroke-miterlimit")

# Resolve one element's computed style. Order is the CSS cascade as these files
# rely on it: presentation attributes lose to any stylesheet rule, stylesheet
# rules are ordered element then class then id, and the inline `style` attribute
# beats all of them.
svg_computed_style <- function(node, inherited, css) {
  style <- inherited[intersect(names(inherited), .svg_inherited)]

  for (p in .svg_presentation) {
    v <- svg_attr(node, p)
    if (!is.null(v)) style[[p]] <- v
  }
  if (length(css)) {
    el <- svg_local_name(node)
    if (!is.null(css[[el]])) style <- utils::modifyList(style, css[[el]])
    cls <- svg_attr(node, "class")
    if (!is.null(cls)) {
      for (c1 in strsplit(trimws(cls), "\\s+")[[1]]) {
        rule <- css[[paste0(".", c1)]]
        if (!is.null(rule)) style <- utils::modifyList(style, rule)
      }
    }
    id <- svg_attr(node, "id")
    if (!is.null(id) && !is.null(css[[paste0("#", id)]])) {
      style <- utils::modifyList(style, css[[paste0("#", id)]])
    }
  }
  inline <- svg_attr(node, "style")
  if (!is.null(inline)) style <- utils::modifyList(style, svg_parse_declarations(inline))
  style
}

# The parse state carried down the tree: the current transformation matrix, the
# computed style, the document's stylesheet and definitions, the flattening
# tolerance, and the accumulators for shapes, warnings, and the raster elements
# that were found but not drawn.
svg_new_state <- function(css, defs, tol, current_colour) {
  env <- new.env(parent = emptyenv())
  env$css <- css
  env$defs <- defs
  env$tol <- tol
  env$colour <- current_colour
  env$shapes <- list()
  env$rasters <- character()
  env$texts <- list()
  env$unsupported <- character()
  env$depth <- 0L
  env
}

svg_record_shape <- function(state, subpaths, closed, style, ctm) {
  if (!length(subpaths)) return(invisible(NULL))
  fill <- svg_colour(style$fill %||% "#000000", state$colour)
  stroke <- svg_colour(style$stroke, state$colour)
  if (is.null(fill)) fill <- "#000000"
  if (is.null(stroke)) stroke <- NA_character_

  # An element with neither paint contributes nothing but vertices.
  if ((is.na(fill) || identical(fill, NA_character_)) && is.na(stroke)) return(invisible(NULL))
  if (identical(style$display, "none") || identical(style$visibility, "hidden")) {
    return(invisible(NULL))
  }

  width_user <- svg_number(style[["stroke-width"]], 1)
  state$shapes[[length(state$shapes) + 1L]] <- list(
    subpaths = subpaths,
    closed = closed,
    fill = fill,
    stroke = stroke,
    ctm = ctm,
    stroke_width_user = width_user * svg_mat_scale(ctm),
    fill_opacity = svg_number(style[["fill-opacity"]], 1),
    stroke_opacity = svg_number(style[["stroke-opacity"]], 1),
    opacity = svg_number(style$opacity, 1),
    rule = if (identical(tolower(style[["fill-rule"]] %||% "nonzero"), "evenodd")) {
      "evenodd"
    } else {
      "winding"
    },
    linecap = style[["stroke-linecap"]] %||% "butt",
    linejoin = style[["stroke-linejoin"]] %||% "miter",
    dash = style[["stroke-dasharray"]],
    clip = style[["clip-path"]]
  )
  invisible(NULL)
}

svg_transform_subpaths <- function(subpaths, ctm) {
  lapply(subpaths, function(m) {
    p <- svg_mat_apply(ctm, m[, 1], m[, 2])
    cbind(p$x, p$y)
  })
}

svg_walk <- function(node, state, ctm, style) {
  name <- svg_local_name(node)
  if (name %in% c("defs", "metadata", "title", "desc", "style", "clipPath",
                  "linearGradient", "radialGradient", "mask", "pattern",
                  "filter", "symbol", "font", "script", "switch")) {
    return(invisible(NULL))
  }

  style <- svg_computed_style(node, style, state$css)
  ctm <- svg_mat_multiply(ctm, svg_parse_transform(svg_attr(node, "transform")))
  tol <- state$tol / max(svg_mat_scale(ctm), 1e-6)

  if (identical(style$display, "none")) return(invisible(NULL))

  if (name == "g" || name == "svg" || name == "a") {
    for (child in xml2::xml_children(node)) svg_walk(child, state, ctm, style)
    return(invisible(NULL))
  }

  if (name == "use") {
    if (state$depth > 12L) return(invisible(NULL))
    href <- svg_attr(node, "href") %||% svg_attr(node, "xlink:href")
    target <- if (!is.null(href)) state$defs[[sub("^#", "", href)]] else NULL
    if (!is.null(target)) {
      shift <- c(1, 0, 0, 1, svg_number(svg_attr(node, "x"), 0), svg_number(svg_attr(node, "y"), 0))
      state$depth <- state$depth + 1L
      svg_walk(target, state, svg_mat_multiply(ctm, shift), style)
      state$depth <- state$depth - 1L
    }
    return(invisible(NULL))
  }

  if (name == "image") {
    state$rasters <- c(state$rasters, svg_attr(node, "id") %||% "image")
    return(invisible(NULL))
  }

  if (name == "text" || name == "tspan") {
    txt <- trimws(xml2::xml_text(node))
    if (nzchar(txt)) {
      p <- svg_mat_apply(ctm, svg_number(svg_attr(node, "x"), 0), svg_number(svg_attr(node, "y"), 0))
      state$texts[[length(state$texts) + 1L]] <- list(
        label = txt, x = p$x, y = p$y,
        size_user = svg_number(style[["font-size"]], 12) * svg_mat_scale(ctm),
        colour = svg_colour(style$fill %||% "#000000", state$colour) %none% "#000000",
        anchor = style[["text-anchor"]] %||% "start",
        family = style[["font-family"]]
      )
    }
    return(invisible(NULL))
  }

  geom <- svg_shape_geometry(node, name, tol, state)
  if (!is.null(geom) && length(geom$subpaths)) {
    closed <- rep_len(geom$closed, length(geom$subpaths))
    svg_record_shape(state, svg_transform_subpaths(geom$subpaths, ctm), closed, style, ctm)
  }
  for (child in xml2::xml_children(node)) svg_walk(child, state, ctm, style)
  invisible(NULL)
}

# The geometry of a single shape element, independent of paint. Shared by the
# painting walker and the clipping-path walker so that the two cannot drift
# apart in what they consider a rectangle.
svg_shape_geometry <- function(node, name, tol, state = NULL) {
  switch(
    name,
    path = {
      d <- svg_attr(node, "d")
      if (is.null(d)) NULL else svg_parse_path(d, tol)
    },
    polygon = {
      m <- svg_points_attr(svg_attr(node, "points") %||% "")
      if (is.null(m)) NULL else list(subpaths = list(rbind(m, m[1, ])), closed = TRUE)
    },
    polyline = {
      m <- svg_points_attr(svg_attr(node, "points") %||% "")
      if (is.null(m)) NULL else list(subpaths = list(m), closed = FALSE)
    },
    line = {
      m <- matrix(c(svg_number(svg_attr(node, "x1"), 0), svg_number(svg_attr(node, "y1"), 0),
                    svg_number(svg_attr(node, "x2"), 0), svg_number(svg_attr(node, "y2"), 0)),
                  ncol = 2L, byrow = TRUE)
      list(subpaths = list(m), closed = FALSE)
    },
    rect = {
      w <- svg_number(svg_attr(node, "width"), 0)
      h <- svg_number(svg_attr(node, "height"), 0)
      if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) NULL else list(
        subpaths = list(svg_rect_points(
          svg_number(svg_attr(node, "x"), 0), svg_number(svg_attr(node, "y"), 0), w, h,
          svg_number(svg_attr(node, "rx"), 0), svg_number(svg_attr(node, "ry"), NA_real_), tol)),
        closed = TRUE)
    },
    circle = {
      r <- svg_number(svg_attr(node, "r"), 0)
      if (!is.finite(r) || r <= 0) NULL else list(
        subpaths = list(svg_ellipse_points(svg_number(svg_attr(node, "cx"), 0),
                                           svg_number(svg_attr(node, "cy"), 0), r, r, tol)),
        closed = TRUE)
    },
    ellipse = {
      rx <- svg_number(svg_attr(node, "rx"), 0)
      ry <- svg_number(svg_attr(node, "ry"), 0)
      if (!is.finite(rx) || !is.finite(ry) || rx <= 0 || ry <= 0) NULL else list(
        subpaths = list(svg_ellipse_points(svg_number(svg_attr(node, "cx"), 0),
                                           svg_number(svg_attr(node, "cy"), 0), rx, ry, tol)),
        closed = TRUE)
    },
    {
      if (!is.null(state)) state$unsupported <- unique(c(state$unsupported, name))
      NULL
    }
  )
}

# ---- gradients and clipping paths --------------------------------------------

# Gradients are resolved here rather than at draw time so that a document with a
# gradient it never uses costs nothing, and so that `href` inheritance -- one
# gradient borrowing another's stops, which is how illustration tools write out
# a recoloured copy -- is followed once.
svg_parse_gradients <- function(root) {
  nodes <- xml2::xml_find_all(
    root, ".//*[local-name()='linearGradient' or local-name()='radialGradient']")
  raw <- list()
  for (nd in nodes) {
    id <- svg_attr(nd, "id")
    if (is.null(id)) next
    stops <- xml2::xml_find_all(nd, "./*[local-name()='stop']")
    offsets <- vapply(stops, function(s) {
      v <- svg_attr(s, "offset") %||% "0"
      out <- if (grepl("%$", v)) svg_number(sub("%$", "", v), 0) / 100 else svg_number(v, 0)
      if (is.finite(out)) out else 0
    }, numeric(1))
    cols <- vapply(stops, function(s) {
      decl <- svg_parse_declarations(svg_attr(s, "style") %||% "")
      col <- decl[["stop-color"]] %||% svg_attr(s, "stop-color") %||% "#000000"
      svg_colour(col, "#000000") %none% "#000000"
    }, character(1))
    alpha <- vapply(stops, function(s) {
      decl <- svg_parse_declarations(svg_attr(s, "style") %||% "")
      svg_number(decl[["stop-opacity"]] %||% svg_attr(s, "stop-opacity"), 1)
    }, numeric(1))
    raw[[id]] <- list(
      type = if (identical(svg_local_name(nd), "radialGradient")) "radial" else "linear",
      href = sub("^#", "", svg_attr(nd, "href") %||% svg_attr(nd, "xlink:href") %||% ""),
      units = svg_attr(nd, "gradientUnits") %||% "objectBoundingBox",
      transform = svg_parse_transform(svg_attr(nd, "gradientTransform")),
      offsets = offsets, colours = cols, alpha = alpha,
      coords = list(
        x1 = svg_attr(nd, "x1"), y1 = svg_attr(nd, "y1"),
        x2 = svg_attr(nd, "x2"), y2 = svg_attr(nd, "y2"),
        cx = svg_attr(nd, "cx"), cy = svg_attr(nd, "cy"), r = svg_attr(nd, "r")
      )
    )
  }
  # Follow `href` once each, which is all these files need and which cannot loop.
  for (id in names(raw)) {
    g <- raw[[id]]
    if (!length(g$offsets) && nzchar(g$href) && !is.null(raw[[g$href]])) {
      parent <- raw[[g$href]]
      raw[[id]]$offsets <- parent$offsets
      raw[[id]]$colours <- parent$colours
      raw[[id]]$alpha <- parent$alpha
    }
  }
  raw
}

# The geometry of each clipping path, in the clip path's own user space. The
# referencing element's transform is applied when the shape is drawn, which is
# what `clipPathUnits="userSpaceOnUse"` means and what every file here uses.
svg_parse_clips <- function(root, css, defs, tol, colour) {
  out <- list()
  for (nd in xml2::xml_find_all(root, ".//*[local-name()='clipPath']")) {
    id <- svg_attr(nd, "id")
    if (is.null(id)) next
    st <- svg_new_state(css, defs, tol, colour)
    for (child in xml2::xml_children(nd)) {
      svg_walk_clip(child, st, svg_mat_identity())
    }
    if (length(st$shapes)) out[[id]] <- st$shapes
  }
  out
}

# A clipping path contributes only outlines, so paint is irrelevant and the
# ordinary walker's "no paint, no shape" rule would discard the whole thing.
svg_walk_clip <- function(node, state, ctm) {
  name <- svg_local_name(node)
  ctm <- svg_mat_multiply(ctm, svg_parse_transform(svg_attr(node, "transform")))
  tol <- state$tol / max(svg_mat_scale(ctm), 1e-6)
  if (name %in% c("g", "svg")) {
    for (child in xml2::xml_children(node)) svg_walk_clip(child, state, ctm)
    return(invisible(NULL))
  }
  geom <- svg_shape_geometry(node, name, tol)
  if (!is.null(geom) && length(geom$subpaths)) {
    state$shapes[[length(state$shapes) + 1L]] <- list(
      subpaths = svg_transform_subpaths(geom$subpaths, ctm),
      closed = rep_len(geom$closed, length(geom$subpaths))
    )
  }
  invisible(NULL)
}

svg_clip_ref <- function(value) {
  if (is.null(value) || is.na(value)) return(NULL)
  m <- regmatches(value, regexec("url\\(\\s*#([^)\\s]+)\\s*\\)", value))[[1]]
  if (length(m) == 2L) m[2] else NULL
}

# ---- viewBox -----------------------------------------------------------------

svg_viewbox <- function(root) {
  vb <- svg_attr(root, "viewBox")
  if (!is.null(vb)) {
    # POSIX character classes, not Perl escapes: inside brackets R's default
    # engine reads "\\s" as a literal backslash or "s", so "[,\\s]+" silently
    # fails to split "0 0 400 232" and every viewBox falls back to a default.
    nums <- suppressWarnings(as.numeric(strsplit(trimws(vb), "[,[:space:]]+")[[1]]))
    nums <- nums[is.finite(nums)]
    if (length(nums) == 4L && nums[3] > 0 && nums[4] > 0) {
      return(c(x = nums[1], y = nums[2], width = nums[3], height = nums[4]))
    }
  }
  w <- svg_number(svg_attr(root, "width"), NA_real_)
  h <- svg_number(svg_attr(root, "height"), NA_real_)
  if (is.finite(w) && is.finite(h) && w > 0 && h > 0) {
    return(c(x = 0, y = 0, width = w, height = h))
  }
  c(x = 0, y = 0, width = 100, height = 100)
}

# ---- public parse ------------------------------------------------------------

#' Read an SVG illustration as vector geometry
#'
#' @description
#' Parses an SVG file into resolved vector geometry: subpath coordinates, fills,
#' strokes, stroke widths, and the document's own provenance metadata. The result
#' is drawn by [asset_grob()] as real grid paths, so an illustration placed in a
#' figure stays vector through PDF, EPS, and SVG export and can be measured by
#' [venue_check()].
#'
#' Rasterising an illustration and drawing the pixels is the usual approach and
#' it is the wrong one for submission artwork: the surrounding panel is vector,
#' most publishers require line art to be vector, and once the drawing is a
#' bitmap no tool can tell you that a rule inside it is below the minimum weight.
#'
#' @param file Path to an SVG file, or a character string of SVG source.
#' @param tolerance Chord tolerance for flattening curves, in the illustration's
#'   own user units. `NULL`, the default, derives it from the canvas as
#'   `width / 1500`, which is the right thing: a drawing's user units are an
#'   arbitrary internal choice, and a fixed tolerance therefore samples a
#'   1920-unit canvas forty times more finely than a 48-unit one for no visible
#'   gain and a very large vertex count. Give a number to override, smaller for
#'   a poster-scale enlargement.
#' @param colour Colour to substitute for `currentColor`, which icon sets use to
#'   defer the decision to the caller.
#'
#' @return An object of class `vellum_svg`: a list with `shapes`, `texts`,
#'   `viewbox`, `meta`, and `notes` describing anything that could not be drawn
#'   as vector geometry.
#'
#' @seealso [asset_grob()] to draw it, [asset_recolour()] to restyle it, and
#'   [vellum_asset()] to load one from a registered corpus.
#' @export
#' @examples
#' svg <- svg_read(
#'   '<svg viewBox="0 0 10 10"><circle cx="5" cy="5" r="4" fill="#3B6EA5"/></svg>'
#' )
#' svg
svg_read <- function(file, tolerance = NULL, colour = "#000000") {
  if (!is.null(tolerance)) check_number(tolerance, "tolerance", min = 1e-6)
  check_string(colour, "colour")

  doc <- if (length(file) == 1L && !grepl("<", file, fixed = TRUE)) {
    if (!file.exists(file)) cli::cli_abort("No SVG file at {.path {file}}.")
    xml2::read_xml(file)
  } else {
    xml2::read_xml(paste(file, collapse = "\n"))
  }
  root <- xml2::xml_root(doc)
  if (!identical(svg_local_name(root), "svg")) {
    cli::cli_abort("The document root is {.val {svg_local_name(root)}}, not {.val svg}.")
  }

  css <- svg_parse_css(xml2::xml_text(xml2::xml_find_all(root, ".//*[local-name()='style']")))
  defs <- list()
  for (nd in xml2::xml_find_all(root, ".//*[@id]")) {
    id <- svg_attr(nd, "id")
    if (!is.null(id) && is.null(defs[[id]])) defs[[id]] <- nd
  }

  vb <- svg_viewbox(root)
  tolerance <- tolerance %||% (vb[["width"]] / 1500)
  state <- svg_new_state(css, defs, tolerance, colour)
  # The root element carries style too. Icon sets in particular set `fill` on
  # `<svg>` and let every path inherit it, so starting the walk with an empty
  # style loses the drawing's only paint declaration.
  root_style <- svg_computed_style(root, list(), css)
  root_ctm <- svg_parse_transform(svg_attr(root, "transform"))
  for (child in xml2::xml_children(root)) {
    svg_walk(child, state, root_ctm, root_style)
  }
  gradients <- svg_parse_gradients(root)
  clips <- svg_parse_clips(root, css, defs, tolerance, colour)

  notes <- character()
  if (length(state$rasters)) {
    notes <- c(notes, sprintf(
      "%d embedded raster image%s could not be drawn as vector geometry",
      length(state$rasters), if (length(state$rasters) == 1L) "" else "s"))
  }
  if (length(state$unsupported)) {
    notes <- c(notes, paste0("unsupported element", if (length(state$unsupported) > 1L) "s" else "",
                             ": ", paste(state$unsupported, collapse = ", ")))
  }

  structure(list(
    shapes = state$shapes,
    texts = state$texts,
    viewbox = vb,
    gradients = gradients,
    clips = clips,
    meta = svg_metadata(root),
    notes = notes,
    rasters = length(state$rasters),
    tolerance = tolerance
  ), class = "vellum_svg")
}

# The drawing's own account of itself. NIAID's BioArt files carry a metadata
# block naming the illustrator and the terms of use, which is better evidence
# than any catalogue entry about the same file, and is kept so that
# `asset_licence()` can report the two side by side when they disagree.
svg_metadata <- function(root) {
  fields <- c("title", "description", "license", "creator", "credit",
              "creationDate", "caption")
  out <- list()
  for (f in fields) {
    nd <- xml2::xml_find_first(root, sprintf(".//*[local-name()='%s']", f))
    if (!inherits(nd, "xml_missing")) {
      v <- trimws(xml2::xml_text(nd))
      if (nzchar(v)) out[[f]] <- v
    }
  }
  out
}

#' @export
print.vellum_svg <- function(x, ...) {
  vb <- x$viewbox
  cli::cli_h2("SVG illustration")
  cli::cli_text("{.strong Geometry} {length(x$shapes)} shape{?s} \u00b7 {sum(vapply(x$shapes, function(s) length(s$subpaths), integer(1)))} subpath{?s} \u00b7 {svg_vertex_count(x)} vertices")
  cli::cli_text("{.strong Canvas} {round(vb[['width']], 2)} \u00d7 {round(vb[['height']], 2)} user units")
  if (length(x$texts)) cli::cli_text("{.strong Text} {length(x$texts)} live text element{?s}")
  if (length(x$meta$title)) cli::cli_text("{.strong Title} {x$meta$title}")
  if (length(x$meta$creator)) cli::cli_text("{.strong Creator} {x$meta$creator}")
  if (length(x$meta$license)) cli::cli_text("{.strong Stated terms} {x$meta$license}")
  if (length(x$notes)) cli::cli_bullets(stats::setNames(x$notes, rep("!", length(x$notes))))
  invisible(x)
}

svg_vertex_count <- function(x) {
  sum(vapply(x$shapes, function(s) sum(vapply(s$subpaths, nrow, integer(1))), integer(1)))
}

#' The distinct colours an illustration uses
#'
#' @description
#' Every fill and stroke in the drawing, with the number of shapes painted in
#' each. This is the input to [asset_recolour()] and to any judgement about
#' whether an illustration can join a figure's palette without fighting it.
#'
#' @param x A `vellum_svg` object from [svg_read()] or [vellum_asset()].
#' @return A data frame with `colour`, `role` (`"fill"` or `"stroke"`), and `n`.
#' @export
#' @examples
#' svg <- svg_read('<svg viewBox="0 0 4 4"><rect width="4" height="4" fill="#AA3377"/></svg>')
#' svg_palette(svg)
svg_palette <- function(x) {
  if (!inherits(x, "vellum_svg")) cli::cli_abort("{.arg x} must be a {.cls vellum_svg} object.")
  fills <- vapply(x$shapes, function(s) s$fill %||% NA_character_, character(1))
  strokes <- vapply(x$shapes, function(s) s$stroke %||% NA_character_, character(1))
  rows <- rbind(
    if (any(!is.na(fills))) as.data.frame(table(colour = fills[!is.na(fills)]),
                                          stringsAsFactors = FALSE) else NULL,
    NULL
  )
  out <- rbind(
    svg_colour_table(fills, "fill"),
    svg_colour_table(strokes, "stroke")
  )
  if (is.null(out)) {
    return(data.frame(colour = character(), role = character(), n = integer(),
                      stringsAsFactors = FALSE))
  }
  out[order(-out$n), , drop = FALSE]
}

svg_colour_table <- function(values, role) {
  values <- values[!is.na(values) & !grepl("^url\\(", values)]
  if (!length(values)) return(NULL)
  tab <- table(values)
  data.frame(colour = names(tab), role = role, n = as.integer(tab),
             stringsAsFactors = FALSE, row.names = NULL)
}
