# Model Context Protocol (MCP) Bridge for Vellum
#
# Model Context Protocol (MCP) connects AI models and autonomous agents to tools,
# databases, and execution environments. This module parses MCP server manifests
# (JSON-RPC tool definitions) and builds publication-grade Vellum relationship graphs,
# mapping MCP servers to compartments (`dg_compartment()`), tools to nodes (`dg_node()`),
# and parameter/execution dependencies to typed relations (`dg_edge()`).

#' Read MCP Server Manifest / Tool List Payload
#'
#' @param input Path to a JSON file, or a JSON string representing an MCP manifest.
#' @return A structured list of class `vellum_mcp_manifest` containing servers, tools, and relations.
#' @export
read_mcp_manifest <- function(input) {
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
    cli::cli_abort("Failed to parse MCP JSON manifest: {e$message}")
  })

  servers <- parsed$servers %||% list(list(id = "default_mcp", name = "Default MCP Server"))
  tools <- parsed$tools %||% list()
  relations <- parsed$relations %||% list()

  structure(list(
    servers = servers,
    tools = tools,
    relations = relations
  ), class = "vellum_mcp_manifest")
}

#' Convert MCP Tool Manifest to Vellum Relationship Graph
#'
#' @param manifest MCP manifest object from [read_mcp_manifest()].
#' @param palette Palette name or colors for Vellum diagram. Defaults to `"okabe_ito"`.
#' @param venue Publication venue target (`"nature"`, `"cell"`, `"pnas"`, `"science"`).
#' @param width Figure width in millimetres.
#' @return A `vellum_diagram` ggplot object ready for rendering or saving.
#' @export
mcp_to_vellum <- function(manifest, palette = "okabe_ito", venue = "nature", width = 183) {
  if (!inherits(manifest, "vellum_mcp_manifest")) {
    cli::cli_abort("{.arg manifest} must be an object returned by {.fn read_mcp_manifest}.")
  }

  # Build dg_node definitions for MCP tools
  node_objs <- lapply(manifest$tools, function(t) {
    tid <- as.character(t$name %||% t$id)
    tlabel <- as.character(t$label %||% tid)
    trole <- as.character(t$role %||% "tool")
    shape_type <- if (identical(trole, "subagent")) "hexagon" else if (identical(trole, "resource")) "cylinder" else "round"
    
    dg_node(
      id = tid,
      label = tlabel,
      shape = shape_type,
      role = trole,
      x = as.numeric(t$x %||% NA_real_),
      y = as.numeric(t$y %||% NA_real_)
    )
  })
  nodes_df <- do.call(rbind, node_objs)

  # Map relation types to Vellum edge vocabulary
  map_mcp_relation <- function(rel_type) {
    rt <- tolower(trimws(as.character(rel_type %||% "flow")))
    if (grepl("invoke|call|trigger", rt)) return("activation")
    if (grepl("param|bind|input", rt)) return("binding")
    if (grepl("delegate|subagent", rt)) return("translocation")
    if (grepl("inhibit|block", rt)) return("inhibition")
    "flow"
  }

  # Build dg_edge definitions for MCP tool interactions
  edge_objs <- lapply(manifest$relations, function(r) {
    src <- as.character(r$from %||% r$source)
    tgt <- as.character(r$to %||% r$target)
    rtype <- as.character(r$type %||% "flow")
    lbl <- as.character(r$label %||% NA_character_)

    dg_edge(
      from = src,
      to = tgt,
      type = map_mcp_relation(rtype),
      label = lbl
    )
  })
  edges_df <- do.call(rbind, edge_objs)

  # Build compartments for MCP servers
  comp_objs <- lapply(manifest$servers, function(s) {
    sid <- as.character(s$id)
    slabel <- as.character(s$name %||% sid)
    scontains <- unlist(s$contains %||% list())

    dg_compartment(
      id = sid,
      label = slabel,
      contains = scontains
    )
  })

  # Compile diagram using Vellum engine
  diagram(
    nodes = nodes_df,
    edges = edges_df,
    compartments = comp_objs,
    palette = palette,
    venue = venue,
    width = width
  )
}
