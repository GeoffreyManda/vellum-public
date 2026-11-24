# Cytoscape Bridge for Vellum
#
# Cytoscape (https://cytoscape.org) is the established open platform for molecular
# and network diagramming. Its Cytoscape Exchange (CX) JSON format records nodes,
# directed edges, data attributes, visual styles, and spatial layouts.
#
# This bridge reads Cytoscape CX / Cytoscape JSON payloads and converts them into
# Vellum's diagram grammar (`diagram()`, `dg_node()`, `dg_edge()`), mapping generic
# network interactions to Vellum's typed edge relation vocabulary (`activation`,
# `inhibition`, `binding`, `translocation`, `catalysis`, `conversion`, `association`),
# enforcing publication venue pre-flight rules (Nature, Cell, PNAS, Science) and
# Oklab colorblind-safe palettes.

#' Read Cytoscape CX / Cytoscape JSON payload
#'
#' @param input Path to a JSON file, or a JSON string.
#' @return A structured list with `nodes`, `edges`, `node_attrs`, `edge_attrs`, `visual_properties`, and `layout`.
#' @export
read_cytoscape_cx <- function(input) {
  if (is.character(input) && length(input) == 1 && file.exists(input)) {
    raw_json <- readLines(input, warn = FALSE)
    raw_json <- paste(raw_json, collapse = "\n")
  } else if (is.character(input)) {
    raw_json <- input
  } else {
    cli::cli_abort("{.arg input} must be a valid file path or JSON string.")
  }

  parsed <- tryCatch({
    jsonlite::fromJSON(raw_json, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_abort("Failed to parse Cytoscape CX JSON: {e$message}")
  })

  nodes <- list()
  edges <- list()
  node_attrs <- list()
  edge_attrs <- list()
  layout_coords <- list()
  visual_props <- list()

  # Handle CX array format vs Cytoscape.js object format
  if (is.list(parsed) && is.null(names(parsed))) {
    # Standard CX aspect array
    for (aspect in parsed) {
      aspect_name <- names(aspect)[1]
      aspect_data <- aspect[[1]]
      if (identical(aspect_name, "nodes")) {
        nodes <- aspect_data
      } else if (identical(aspect_name, "edges")) {
        edges <- aspect_data
      } else if (identical(aspect_name, "nodeAttributes")) {
        node_attrs <- aspect_data
      } else if (identical(aspect_name, "edgeAttributes")) {
        edge_attrs <- aspect_data
      } else if (identical(aspect_name, "cartesianLayout")) {
        layout_coords <- aspect_data
      } else if (identical(aspect_name, "cyVisualProperties")) {
        visual_props <- aspect_data
      }
    }
  } else if (is.list(parsed) && !is.null(parsed$elements)) {
    # Cytoscape.js JSON format ({ elements: { nodes: [...], edges: [...] } })
    if (!is.null(parsed$elements$nodes)) {
      for (n in parsed$elements$nodes) {
        data_obj <- n$data %||% list()
        id_str <- as.character(data_obj$id %||% n$id)
        name_str <- as.character(data_obj$name %||% data_obj$label %||% id_str)
        nodes[[length(nodes) + 1]] <- list(`@id` = id_str, n = name_str)
        if (!is.null(n$position)) {
          layout_coords[[length(layout_coords) + 1]] <- list(
            node = id_str,
            x = n$position$x,
            y = n$position$y
          )
        }
      }
    }
    if (!is.null(parsed$elements$edges)) {
      for (e in parsed$elements$edges) {
        data_obj <- e$data %||% list()
        id_str <- as.character(data_obj$id %||% e$id)
        src_str <- as.character(data_obj$source %||% e$source)
        tgt_str <- as.character(data_obj$target %||% e$target)
        interaction_str <- as.character(data_obj$interaction %||% data_obj$type %||% "flow")
        edges[[length(edges) + 1]] <- list(
          `@id` = id_str,
          s = src_str,
          t = tgt_str,
          i = interaction_str
        )
      }
    }
  }

  structure(list(
    nodes = nodes,
    edges = edges,
    node_attrs = node_attrs,
    edge_attrs = edge_attrs,
    layout = layout_coords,
    visual_props = visual_props
  ), class = "vellum_cytoscape_cx")
}

#' Convert Cytoscape network to Vellum diagram
#'
#' @param cx Cytoscape network object from [read_cytoscape_cx()].
#' @param palette Palette name or colors for Vellum diagram. Defaults to `"okabe_ito"`.
#' @param venue Publication venue target (`"nature"`, `"cell"`, `"pnas"`, `"science"`).
#' @param width Figure width in millimetres.
#' @return A `vellum_diagram` ggplot object ready for rendering or saving.
#' @export
cytoscape_to_vellum <- function(cx, palette = "okabe_ito", venue = "nature", width = 89) {
  if (!inherits(cx, "vellum_cytoscape_cx")) {
    cli::cli_abort("{.arg cx} must be an object returned by {.fn read_cytoscape_cx}.")
  }

  # Build map of node IDs to positions & names
  node_map <- list()
  for (n in cx$nodes) {
    nid <- as.character(n$`@id` %||% n$id)
    nlabel <- as.character(n$n %||% n$label %||% nid)
    node_map[[nid]] <- list(id = nid, label = nlabel, x = NA_real_, y = NA_real_)
  }

  for (l in cx$layout) {
    nid <- as.character(l$node %||% l$`node@id`)
    if (!is.null(node_map[[nid]])) {
      node_map[[nid]]$x <- as.numeric(l$x)
      node_map[[nid]]$y <- as.numeric(l$y)
    }
  }

  # Map interaction types to Vellum edge relation vocabulary
  map_edge_type <- function(interaction) {
    if (is.null(interaction) || is.na(interaction)) return("flow")
    itr <- tolower(trimws(as.character(interaction)))
    if (grepl("activate|positive|stimulate|upreg", itr)) return("activation")
    if (grepl("inhibit|negative|suppress|downreg|block", itr)) return("inhibition")
    if (grepl("bind|complex|interact|ppi", itr)) return("binding")
    if (grepl("translocat|import|export|move", itr)) return("translocation")
    if (grepl("catalys|enzym|phosphorylat", itr)) return("catalysis")
    if (grepl("convert|metabol|transform", itr)) return("conversion")
    if (grepl("associat|correlat", itr)) return("association")
    "flow"
  }

  # Construct Vellum dg_node objects
  node_objs <- lapply(node_map, function(nm) {
    dg_node(
      id = nm$id,
      label = nm$label,
      shape = "round",
      x = nm$x,
      y = nm$y
    )
  })
  nodes_df <- do.call(rbind, node_objs)

  # Construct Vellum dg_edge objects
  edge_objs <- lapply(cx$edges, function(e) {
    src <- as.character(e$s %||% e$source)
    tgt <- as.character(e$t %||% e$target)
    itr <- as.character(e$i %||% e$interaction %||% "flow")
    dg_edge(
      from = src,
      to = tgt,
      type = map_edge_type(itr)
    )
  })
  edges_df <- do.call(rbind, edge_objs)

  # Compile diagram using Vellum engine
  diagram(
    nodes = nodes_df,
    edges = edges_df,
    palette = palette,
    venue = venue,
    width = width
  )
}

#' Export Vellum diagram to Cytoscape CX JSON
#'
#' @param dg A Vellum diagram object (or node/edge specification list).
#' @return A JSON string in Cytoscape CX format.
#' @export
export_cytoscape_cx <- function(dg) {
  # Extract node and edge data from diagram object or list
  if (inherits(dg, "vellum_diagram") || is.list(dg)) {
    nodes_df <- dg$nodes %||% dg$data$nodes
    edges_df <- dg$edges %||% dg$data$edges
  } else {
    cli::cli_abort("Unsupported object type for export_cytoscape_cx.")
  }

  cx_nodes <- list()
  cx_coords <- list()
  if (!is.null(nodes_df)) {
    for (i in seq_len(nrow(nodes_df))) {
      row <- nodes_df[i, ]
      nid <- as.character(row$id)
      lbl <- as.character(row$label %||% nid)
      cx_nodes[[length(cx_nodes) + 1]] <- list(`@id` = nid, n = lbl)
      if (!is.na(row$x) && !is.na(row$y)) {
        cx_coords[[length(cx_coords) + 1]] <- list(node = nid, x = row$x, y = row$y)
      }
    }
  }

  cx_edges <- list()
  if (!is.null(edges_df)) {
    for (i in seq_len(nrow(edges_df))) {
      row <- edges_df[i, ]
      cx_edges[[length(cx_edges) + 1]] <- list(
        `@id` = paste0("e", i),
        s = as.character(row$from),
        t = as.character(row$to),
        i = as.character(row$type %||% "flow")
      )
    }
  }

  cx_structure <- list(
    list(numberVerification = list(list(longNumber = 281474976710655))),
    list(metaData = list(
      list(name = "nodes", version = "1.0"),
      list(name = "edges", version = "1.0"),
      list(name = "cartesianLayout", version = "1.0")
    )),
    list(nodes = cx_nodes),
    list(edges = cx_edges),
    list(cartesianLayout = cx_coords),
    list(status = list(list(error = "", isSuccess = TRUE)))
  )

  jsonlite::toJSON(cx_structure, auto_unbox = TRUE, pretty = TRUE)
}
