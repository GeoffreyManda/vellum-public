# Asset corpora, and the licence question.
#
# Vellum does not ship illustrations. It ships the reader, the registry, and the
# ledger, and points at a corpus that lives on disk. There are three reasons, and
# only the first is about disk space.
#
# The corpora are licence-heterogeneous. BioIcons alone spans CC0, CC BY, CC BY-SA,
# MIT and BSD, keyed off the upstream directory an asset happens to sit in.
# Share-alike is not a footnote: it propagates into the figure that uses the asset,
# and therefore into the paper. A package that bundled all of it would be handing
# every user an obligation they did not read.
#
# And at least one corpus disagrees with itself. The NIAID BioArt manifest records
# CC-BY-4.0; the files' own embedded metadata says "Public Domain, Courtesy of
# NIAID"; about a third carry no licence metadata at all. Vellum is not in a
# position to adjudicate that and does not try. It reports both claims, marks the
# asset disputed, and leaves the decision with the person whose name goes on the
# paper.

.asset_env <- new.env(parent = emptyenv())
.asset_env$corpora <- list()

# The manifest schema the open corpora write. Recorded as a constant so that a
# corpus which does not match it fails loudly at registration rather than
# producing empty search results later.
.asset_required <- c("id", "name", "license", "source")

#' Register a corpus of illustration assets
#'
#' @description
#' Points vellum at a directory of SVG assets described by a manifest. The
#' manifest is the JSON written by open scientific icon corpora: an object with
#' `source`, `sourceUrl` and an `assets` array whose entries carry `id`, `name`,
#' `license`, `author`, `sourceUrl` and `svgPath`.
#'
#' Registration is per session and holds only paths and metadata; nothing is
#' copied and no asset is read until it is asked for.
#'
#' @param name Short name for the corpus, used to qualify asset identifiers.
#' @param path Directory containing the manifest.
#' @param manifest Manifest file name within `path`.
#' @param licence_override Optional single licence identifier to record for every
#'   asset, for a corpus whose manifest omits it. Use only with evidence; the
#'   ledger reports it as an override rather than as the corpus's own claim.
#'
#' @return Invisibly, a one-row summary of the registered corpus.
#' @seealso [asset_corpora()], [asset_search()], [vellum_asset()],
#'   [asset_licence()].
#' @export
#' @examples
#' dir <- tempfile()
#' dir.create(file.path(dir, "icons"), recursive = TRUE)
#' writeLines('<svg viewBox="0 0 8 8"><circle cx="4" cy="4" r="3" fill="#333"/></svg>',
#'            file.path(dir, "icons", "dot.svg"))
#' writeLines(paste0('{"source":"Demo","assets":[{"id":"demo:dot","name":"Dot",',
#'                   '"license":"CC0-1.0","source":"Demo","svgPath":"icons/dot.svg"}]}'),
#'            file.path(dir, "manifest.json"))
#' asset_corpus_register("demo", dir)
#' asset_search("dot")
asset_corpus_register <- function(name, path, manifest = "manifest.json",
                                  licence_override = NULL) {
  check_string(name, "name")
  check_string(path, "path")
  check_string(manifest, "manifest")
  check_installed("jsonlite", "to read asset corpus manifests")
  if (!is.null(licence_override)) check_string(licence_override, "licence_override")

  file <- file.path(path, manifest)
  if (!file.exists(file)) {
    cli::cli_abort(c("No manifest at {.path {file}}.",
                     i = "A corpus directory must contain a manifest describing its assets."))
  }
  parsed <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
  assets <- parsed$assets
  if (is.null(assets) || !NROW(assets)) {
    cli::cli_abort("The manifest at {.path {file}} lists no assets.")
  }
  if (!is.data.frame(assets)) assets <- as.data.frame(assets, stringsAsFactors = FALSE)

  missing <- setdiff(.asset_required, names(assets))
  if (length(missing)) {
    required <- .asset_required
    cli::cli_abort(c("The manifest is missing required field{?s} {.field {missing}}.",
                     i = "Expected at least {.field {required}} per asset."))
  }
  # `tags` arrives as a list column and is flattened to one searchable string,
  # because a list column cannot be grepped and search is what this is for.
  tags <- if ("tags" %in% names(assets)) {
    vapply(assets$tags, function(t) paste(unlist(t), collapse = " "), character(1))
  } else {
    rep("", nrow(assets))
  }
  # A corpus may be vector, raster, or mixed. BioIcons and Health Icons publish
  # `svgPath`; image banks of painted or photographic artwork publish `pngPath`
  # with `assetFormat`. Reading only the first would silently drop every raster
  # asset and report a corpus that is quietly empty.
  svg_col <- if ("svgPath" %in% names(assets)) assets$svgPath else rep(NA_character_, nrow(assets))
  png_col <- if ("pngPath" %in% names(assets)) assets$pngPath else rep(NA_character_, nrow(assets))
  path_col <- ifelse(is.na(svg_col) | !nzchar(svg_col), png_col, svg_col)

  index <- data.frame(
    corpus = name,
    id = as.character(assets$id),
    name = as.character(assets$name),
    category = if ("category" %in% names(assets)) as.character(assets$category) else NA_character_,
    tags = tags,
    licence = if (is.null(licence_override)) as.character(assets$license) else licence_override,
    licence_source = if (is.null(licence_override)) "manifest" else "override",
    author = if ("author" %in% names(assets)) as.character(assets$author) else NA_character_,
    source = as.character(assets$source),
    source_url = if ("sourceUrl" %in% names(assets)) as.character(assets$sourceUrl) else NA_character_,
    file = ifelse(is.na(path_col), NA_character_, file.path(path, path_col)),
    format = ifelse(grepl("\\.svg$", path_col %none% "", ignore.case = TRUE),
                    "vector", "raster"),
    stringsAsFactors = FALSE
  )
  index <- index[!is.na(index$file), , drop = FALSE]
  if (!nrow(index)) cli::cli_abort("No asset in the manifest has a usable file path.")

  present <- file.exists(index$file)
  if (!any(present)) {
    cli::cli_abort(c("None of the {nrow(index)} manifest entries resolve to a file under {.path {path}}.",
                     i = "The manifest may describe a corpus that has not been downloaded."))
  }
  if (!all(present)) {
    cli::cli_warn("{sum(!present)} of {nrow(index)} manifest entries have no file on disk and were skipped.")
    index <- index[present, , drop = FALSE]
  }

  .asset_env$corpora[[name]] <- list(
    name = name, path = path,
    source = parsed$source %||% name,
    source_url = parsed$sourceUrl %||% NA_character_,
    index = index
  )
  out <- data.frame(corpus = name, assets = nrow(index),
                    vector = sum(index$format == "vector"),
                    raster = sum(index$format == "raster"),
                    licences = length(unique(index$licence)),
                    path = path, stringsAsFactors = FALSE)
  cli::cli_inform(c(v = "Registered {.val {name}}: {nrow(index)} asset{?s} ({sum(index$format == 'vector')} vector, {sum(index$format == 'raster')} raster), {length(unique(index$licence))} licence{?s}."))
  invisible(out)
}

#' Registered asset corpora
#'
#' @return A data frame of registered corpora, one row each, with asset and
#'   licence counts. Zero rows if none is registered.
#' @export
#' @examples
#' asset_corpora()
asset_corpora <- function() {
  cor <- .asset_env$corpora
  if (!length(cor)) {
    return(data.frame(corpus = character(), source = character(), assets = integer(),
                      licences = integer(), path = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(cor, function(c1) data.frame(
    corpus = c1$name, source = c1$source, assets = nrow(c1$index),
    licences = length(unique(c1$index$licence)), path = c1$path,
    stringsAsFactors = FALSE)))
}

#' Forget a registered corpus
#'
#' @param name Corpus name, or `NULL` to clear all of them.
#' @return Invisibly, the names that were removed.
#' @export
#' @examples
#' asset_corpus_remove()
asset_corpus_remove <- function(name = NULL) {
  if (is.null(name)) {
    old <- names(.asset_env$corpora)
    .asset_env$corpora <- list()
    return(invisible(old))
  }
  check_string(name, "name")
  .asset_env$corpora[[name]] <- NULL
  invisible(name)
}

asset_index <- function(corpus = NULL) {
  cor <- .asset_env$corpora
  if (!length(cor)) {
    cli::cli_abort(c("No asset corpus is registered.",
                     i = "Point vellum at one with {.fn asset_corpus_register}."))
  }
  if (!is.null(corpus)) {
    check_string(corpus, "corpus")
    if (is.null(cor[[corpus]])) {
      cli::cli_abort("No corpus named {.val {corpus}}. Registered: {.val {names(cor)}}.")
    }
    return(cor[[corpus]]$index)
  }
  do.call(rbind, lapply(cor, `[[`, "index"))
}

#' Search registered assets
#'
#' @description
#' Matches a query against asset names, tags and categories. Search quality
#' depends on the corpus: some hydrated manifests carry degenerate categories and
#' boilerplate tags, in which case the asset name is doing all the work, and this
#' is visible in the results rather than hidden by them.
#'
#' @param query Words to look for. All of them must match, in any field.
#' @param corpus Restrict to one registered corpus.
#' @param licence Restrict to one or more licence identifiers, for instance
#'   `"CC0-1.0"` to avoid attribution obligations entirely.
#' @param exclude_share_alike Drop share-alike assets. `FALSE` by default so that
#'   nothing is silently hidden, but see [asset_licence()]: a share-alike asset
#'   propagates its terms into the figure that uses it.
#' @param n Maximum rows to return.
#'
#' @return A data frame of matching assets.
#' @export
#' @examples
#' asset_corpus_remove()
#' try(asset_search("cell"))
asset_search <- function(query, corpus = NULL, licence = NULL,
                         exclude_share_alike = FALSE, n = 25) {
  check_string(query, "query")
  check_bool(exclude_share_alike, "exclude_share_alike")
  check_number(n, "n", min = 1)
  idx <- asset_index(corpus)

  hay <- tolower(paste(idx$name, idx$tags, idx$category %none% ""))
  words <- strsplit(tolower(trimws(query)), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  keep <- rep(TRUE, nrow(idx))
  for (w in words) keep <- keep & grepl(w, hay, fixed = TRUE)
  out <- idx[keep, , drop = FALSE]

  if (!is.null(licence)) {
    if (!is.character(licence)) cli::cli_abort("{.arg licence} must be character.")
    out <- out[out$licence %in% licence, , drop = FALSE]
  }
  if (exclude_share_alike) out <- out[!asset_is_share_alike(out$licence), , drop = FALSE]

  # Shorter names first: a query for "cell" should surface "Cell" before
  # "Cell Culture Flask With Adherent Cells".
  out <- out[order(nchar(out$name), out$name), , drop = FALSE]
  utils::head(out, as.integer(n))
}

asset_is_share_alike <- function(licence) {
  grepl("-SA-|share.?alike", licence %none% "", ignore.case = TRUE)
}

#' Load a registered asset
#'
#' @description
#' Reads a registered asset into vector geometry, attaching the registry's
#' provenance and the file's own embedded metadata. Where those two disagree
#' about the licence, both are kept and the asset is marked disputed; see
#' [asset_licence()].
#'
#' @param id Asset identifier, or a row from [asset_search()].
#' @param ... Passed to [svg_read()], notably `colour` for icon sets that defer
#'   their ink to the caller and `tolerance` for the flattening resolution.
#'
#' @return A `vellum_svg` object for vector artwork, or a `vellum_raster` object
#'   for a raster asset, each with a `provenance` component.
#' @export
#' @examples
#' asset_corpus_remove()
#' try(vellum_asset("demo:dot"))
vellum_asset <- function(id, ...) {
  if (is.data.frame(id)) {
    if (nrow(id) != 1L) cli::cli_abort("Give one asset, not {nrow(id)}.")
    row <- id
  } else {
    check_string(id, "id")
    idx <- asset_index()
    hit <- which(idx$id == id)
    if (!length(hit)) {
      near <- idx$id[grepl(id, idx$id, fixed = TRUE)]
      cli::cli_abort(c("No registered asset with id {.val {id}}.",
                       i = if (length(near)) "Did you mean {.val {utils::head(near, 3)}}?" else
                         "Search with {.fn asset_search}."))
    }
    row <- idx[hit[1], , drop = FALSE]
  }
  if (!file.exists(row$file)) {
    cli::cli_abort("The asset file {.path {row$file}} is no longer on disk.")
  }
  # Dispatch on what the file actually is. A raster asset loaded through the SVG
  # reader would fail with a parse error that says nothing useful about why.
  if (identical(row$format %none% "vector", "raster")) {
    out <- raster_read(row$file, name = row$name, source = row$source,
                       licence = row$licence, author = row$author)
    out$provenance <- utils::modifyList(out$provenance, asset_provenance(row, list()))
    return(out)
  }
  svg <- svg_read(row$file, ...)
  svg$provenance <- asset_provenance(row, svg$meta)
  svg
}

# Reconcile what the catalogue says about an asset with what the asset says
# about itself. Two independent claims about the same file are more informative
# than one, and the disagreement is the interesting part.
asset_provenance <- function(row, meta) {
  stated <- meta$license
  recorded <- row$licence
  disputed <- !is.null(stated) && nzchar(stated) &&
    !identical(asset_licence_key(stated), asset_licence_key(recorded))
  list(
    id = row$id,
    name = row$name,
    corpus = row$corpus,
    licence_recorded = recorded,
    licence_recorded_from = row$licence_source,
    licence_stated_in_file = stated %||% NA_character_,
    licence_disputed = disputed,
    share_alike = asset_is_share_alike(recorded) ||
      asset_is_share_alike(stated %||% ""),
    author = meta$creator %||% row$author,
    credit = meta$credit %||% NA_character_,
    source = row$source,
    source_url = row$source_url,
    file = row$file
  )
}

# Compare licences by meaning rather than by spelling, so that "CC0-1.0" and
# "CC0 1.0" do not read as a dispute while "Public Domain" and "CC-BY-4.0" do.
asset_licence_key <- function(x) {
  x <- tolower(x %none% "")
  x <- gsub("[^a-z0-9]+", "", x)
  x <- sub("^cc0.*", "cc0", x)
  x <- sub("^publicdomain.*|^pd$|^cc0$", "publicdomain", x)
  x <- sub("^ccby(sa)?([0-9]+)?.*", "ccby\\1", x)
  x
}

#' The licence position of a set of assets
#'
#' @description
#' A ledger for the figure's methods section and for the person who has to answer
#' a publisher's rights query. It reports, per asset, what the catalogue claims,
#' what the file claims, whether those disagree, and whether the terms propagate
#' into the figure.
#'
#' Three verdicts:
#'
#' * `"clear"` -- public domain or CC0. No obligation.
#' * `"attribution"` -- credit is required and [asset_attribution()] generates it.
#' * `"review"` -- share-alike, unknown, custom, or a licence the file and the
#'   catalogue disagree about. Not a refusal to proceed; a refusal to pretend the
#'   question is settled.
#'
#' @param ... `vellum_svg` objects loaded by [vellum_asset()], or a data frame
#'   from [asset_search()].
#' @return A data frame with one row per asset and a `verdict` column, with the
#'   overall position in the `verdict` attribute.
#' @export
#' @examples
#' asset_corpus_remove()
#' try(asset_licence())
asset_licence <- function(...) {
  items <- list(...)
  if (!length(items)) cli::cli_abort("Give at least one asset.")
  rows <- list()
  for (it in items) {
    if (inherits(it, "vellum_svg")) {
      p <- it$provenance
      if (is.null(p)) {
        cli::cli_abort(c("This illustration carries no provenance.",
                         i = "Load it with {.fn vellum_asset} rather than {.fn svg_read} to get one."))
      }
      rows[[length(rows) + 1L]] <- as.data.frame(p[c(
        "id", "name", "corpus", "licence_recorded", "licence_stated_in_file",
        "licence_disputed", "share_alike", "author", "source_url")],
        stringsAsFactors = FALSE)
    } else if (inherits(it, "vellum_raster")) {
      p <- it$provenance
      rows[[length(rows) + 1L]] <- as.data.frame(p[c(
        "id", "name", "corpus", "licence_recorded", "licence_stated_in_file",
        "licence_disputed", "share_alike", "author", "source_url")],
        stringsAsFactors = FALSE)
    } else if (is.data.frame(it)) {
      rows[[length(rows) + 1L]] <- data.frame(
        id = it$id, name = it$name, corpus = it$corpus,
        licence_recorded = it$licence, licence_stated_in_file = NA_character_,
        licence_disputed = FALSE, share_alike = asset_is_share_alike(it$licence),
        author = it$author, source_url = it$source_url, stringsAsFactors = FALSE)
    } else {
      cli::cli_abort("Each argument must be a {.cls vellum_svg}, a {.cls vellum_raster}, or a search result.")
    }
  }
  out <- do.call(rbind, rows)
  key <- asset_licence_key(out$licence_recorded)
  out$verdict <- ifelse(
    out$licence_disputed | out$share_alike |
      !key %in% c("publicdomain", "cc0", "ccby", "mit", "bsd"),
    "review",
    ifelse(key %in% c("publicdomain", "cc0"), "clear", "attribution"))
  overall <- if (any(out$verdict == "review")) "review" else
    if (any(out$verdict == "attribution")) "attribution" else "clear"
  structure(out, class = c("vellum_licence", "data.frame"), verdict = overall)
}

#' @export
print.vellum_licence <- function(x, ...) {
  verdict <- attr(x, "verdict")
  cli::cli_h2("Asset licence ledger")
  cli::cli_text("{nrow(x)} asset{?s} \u00b7 overall position {.strong {verdict}}")
  for (i in seq_len(nrow(x))) {
    sym <- switch(x$verdict[i], clear = "v", attribution = "i", "!")
    cli::cli_bullets(stats::setNames(
      sprintf("%s \u2014 %s (%s)", x$name[i], x$licence_recorded[i], x$verdict[i]), sym))
    if (isTRUE(x$licence_disputed[i])) {
      cli::cli_bullets(c(" " = "the file itself states {.val {x$licence_stated_in_file[i]}}"))
    }
    if (isTRUE(x$share_alike[i])) {
      cli::cli_bullets(c(" " = "share-alike: these terms propagate to the figure"))
    }
  }
  if (verdict != "clear") {
    cli::cli_bullets(c(i = "Generate the credit line with {.fn asset_attribution}."))
  }
  invisible(x)
}

#' A credit line for the assets a figure uses
#'
#' @description
#' Produces the attribution sentence a figure legend or methods section needs.
#' Assets whose terms require no credit are omitted unless `all = TRUE`, because
#' a legend padded with unnecessary attributions is how the necessary ones stop
#' being read.
#'
#' @param ... `vellum_svg` objects loaded by [vellum_asset()].
#' @param all Include assets that carry no attribution obligation.
#' @param style `"sentence"` for prose, `"list"` for one entry per line.
#' @return A single string, or `character(0)` if nothing needs crediting.
#' @export
#' @examples
#' asset_corpus_remove()
#' try(asset_attribution())
asset_attribution <- function(..., all = FALSE, style = c("sentence", "list")) {
  style <- match.arg(style)
  check_bool(all, "all")
  led <- asset_licence(...)
  keep <- if (all) rep(TRUE, nrow(led)) else led$verdict != "clear"
  led <- led[keep, , drop = FALSE]
  if (!nrow(led)) return(character(0))

  entries <- vapply(seq_len(nrow(led)), function(i) {
    who <- led$author[i]
    who <- if (is.na(who) || !nzchar(who)) led$corpus[i] else who
    paste0(led$name[i], ", ", who, " (", led$licence_recorded[i], ")")
  }, character(1))
  entries <- unique(entries)

  if (style == "list") return(paste(entries, collapse = "\n"))
  paste0("Illustration assets: ", paste(entries, collapse = "; "), ".")
}
