# Medical and scientific schematic recipes.
#
# Each recipe here corresponds to a figure that has a *correctness* criterion, not
# merely a style. A participant flow diagram whose counts do not reconcile is
# wrong. A causal diagram with a cycle in it is wrong. A compartmental model whose
# transitions do not name their rates is incomplete. These are the checks a
# reviewer will apply, so the recipe applies them first and refuses to draw a
# figure that fails them, rather than producing a handsome picture of an error.

#' Participant flow diagram
#'
#' @description
#' A CONSORT-style flow of participants through a study. The counts are checked
#' before anything is drawn: at every stage the number continuing plus the numbers
#' excluded must equal the number that entered. A flow diagram that does not
#' reconcile is the single most common reviewable defect in a trial figure, and it
#' is arithmetic, so it should never survive to review.
#'
#' @param stages A data frame with `id`, `label` and `n`, in order from the top.
#'   An optional `arm` column splits the flow into parallel columns from the point
#'   the arms diverge.
#' @param exclusions A data frame with `after` naming the stage the exclusion
#'   follows, `label` giving the reason, and `n`. Several rows may share `after`.
#' @param venue Venue whose tokens govern type and rules.
#' @param check Verify that the counts reconcile. Leave `TRUE`.
#' @param node_width,spacing_y Geometry in millimetres.
#' @param ... Passed to [diagram()].
#'
#' @return A ggplot. The reconciliation table is attached as the
#'   `vellum_reconciliation` attribute.
#'
#' @references Schulz KF, Altman DG, Moher D (2010). CONSORT 2010 Statement.
#'   *BMJ* 340:c332.
#' @export
#' @examples
#' stages <- data.frame(
#'   id = c("assessed", "randomised", "analysed"),
#'   label = c("Assessed for eligibility", "Randomised", "Analysed"),
#'   n = c(430, 318, 302)
#' )
#' exclusions <- data.frame(
#'   after = c("assessed", "assessed", "randomised"),
#'   label = c("Did not meet criteria", "Declined", "Lost to follow-up"),
#'   n = c(84, 28, 16)
#' )
#' fig_consort(stages, exclusions)
fig_consort <- function(stages, exclusions = NULL, venue = "nature", check = TRUE,
                        node_width = 46, spacing_y = 20, ...) {
  if (!is.data.frame(stages) || !all(c("id", "label", "n") %in% names(stages))) {
    cli::cli_abort("{.arg stages} must be a data frame with {.field id}, {.field label} and {.field n}.")
  }
  if (nrow(stages) < 2L) cli::cli_abort("A flow diagram needs at least two stages.")
  check_bool(check, "check")
  check_numeric(stages$n, "stages$n")

  if (!is.null(exclusions)) {
    if (!all(c("after", "label", "n") %in% names(exclusions))) {
      cli::cli_abort("{.arg exclusions} needs {.field after}, {.field label} and {.field n}.")
    }
    bad <- setdiff(exclusions$after, stages$id)
    if (length(bad)) {
      cli::cli_abort("{.arg exclusions$after} names stage{?s} that do not exist: {.val {bad}}.")
    }
  }

  recon <- consort_reconcile(stages, exclusions)
  if (check && any(!recon$reconciles)) {
    bad <- recon[!recon$reconciles, , drop = FALSE]
    cli::cli_abort(c(
      "The participant counts do not reconcile.",
      stats::setNames(sprintf(
        "%s: %s entered, %s excluded, %s continued, leaving %s unaccounted for.",
        bad$from, bad$entered, bad$excluded, bad$continued, bad$difference),
        rep("x", nrow(bad))),
      i = "Every participant leaving a stage must either continue or be excluded with a reason.",
      i = "Set {.code check = FALSE} to draw it anyway, but the figure will be wrong."))
  }

  # A CONSORT diagram is drawn in plain rules in every journal that asks for one.
  # Colouring the boxes by role would imply the stages differ in kind, which they
  # do not: they are one cohort losing members.
  tk <- vellum_tokens(venue)
  nodes <- lapply(seq_len(nrow(stages)), function(i) {
    dg_node(stages$id[i], stages$label[i], shape = "rect", width = node_width,
            height = 11, count = stages$n[i], role = "stage",
            fill = "#FFFFFF", colour = tk$ink$text, text_colour = tk$ink$text)
  })
  edges <- lapply(seq_len(nrow(stages) - 1L), function(i) {
    dg_edge(stages$id[i], stages$id[i + 1L], "flow")
  })

  # Exclusion boxes hang to the right of the spine at the midpoint of the step
  # they follow, which is the CONSORT convention and keeps the main flow readable
  # as a single vertical line.
  if (!is.null(exclusions) && nrow(exclusions)) {
    grouped <- split(exclusions, exclusions$after)
    for (after in names(grouped)) {
      g <- grouped[[after]]
      idx <- match(after, stages$id)
      if (is.na(idx) || idx >= nrow(stages)) next
      id <- paste0("excl_", after)
      label <- paste(sprintf("%s (n = %s)", g$label,
                             format(g$n, big.mark = " ", trim = TRUE)),
                     collapse = "\n")
      excl_width <- node_width * 0.92
      nodes[[length(nodes) + 1L]] <- dg_node(
        id, paste0("Excluded (n = ", format(sum(g$n), big.mark = " ", trim = TRUE), ")\n", label),
        shape = "rect", width = excl_width, height = 11 + 4.4 * nrow(g),
        role = "excluded", fill = "#FFFFFF", colour = tk$ink$weak,
        text_colour = tk$ink$text,
        # Clear of the spine by an explicit gap rather than by a fraction that
        # happens to work at one node width: the two boxes must not touch.
        x = node_width / 2 + 8 + excl_width / 2,
        y = -(idx - 0.5) * spacing_y)
      edges[[length(edges) + 1L]] <- dg_edge(stages$id[idx], id, "flow", route = "elbow")
    }
  }

  p <- diagram(nodes, edges, layout = "flow", direction = "down", venue = venue,
               node_width = node_width, spacing_y = spacing_y, ...)
  attr(p, "vellum_reconciliation") <- recon
  p
}

# Each step is checked on its own rather than only end to end, because a total
# that happens to come out right can still conceal two compensating errors.
consort_reconcile <- function(stages, exclusions) {
  n <- nrow(stages)
  rows <- lapply(seq_len(n - 1L), function(i) {
    ex <- if (is.null(exclusions)) 0 else sum(exclusions$n[exclusions$after == stages$id[i]])
    entered <- stages$n[i]
    continued <- stages$n[i + 1L]
    diff <- entered - ex - continued
    data.frame(from = stages$id[i], to = stages$id[i + 1L],
               entered = entered, excluded = ex, continued = continued,
               difference = diff, reconciles = isTRUE(all.equal(diff, 0)),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Causal directed acyclic graph
#'
#' @description
#' A DAG in the epidemiological sense: variables with causal roles, directed
#' edges, and an adjustment set. Roles drive the drawing, so a confounder does not
#' look like a mediator, and the graph is checked for cycles, because a cycle in a
#' causal DAG is not a stylistic choice but a contradiction.
#'
#' Adjusted variables are drawn boxed, the convention that marks conditioning.
#' This matters most for a collider: conditioning on one *opens* a path rather
#' than closing it, and a reader has to be able to see that it was conditioned on.
#'
#' @param variables A data frame with `id`, `label` and `role`. Roles are
#'   `"exposure"`, `"outcome"`, `"confounder"`, `"mediator"`, `"collider"`,
#'   `"instrument"`, or free text for anything else.
#' @param arcs A data frame with `from`, `to`, and an optional logical `biasing`
#'   marking the edges that carry a path the analysis must close.
#' @param adjusted Identifiers of variables conditioned on in the analysis.
#' @param venue,palette Passed through.
#' @param ... Passed to [diagram()].
#'
#' @return A ggplot.
#' @export
#' @examples
#' variables <- data.frame(
#'   id = c("wash", "diarr", "ses", "stunt"),
#'   label = c("WASH access", "Diarrhoea", "Household wealth", "Stunting"),
#'   role = c("exposure", "mediator", "confounder", "outcome")
#' )
#' arcs <- data.frame(
#'   from = c("wash", "diarr", "ses", "ses"),
#'   to   = c("diarr", "stunt", "wash", "stunt")
#' )
#' fig_dag(variables, arcs, adjusted = "ses")
fig_dag <- function(variables, arcs, adjusted = character(), venue = "nature",
                    palette = "okabe_ito", ...) {
  if (!is.data.frame(variables) || !all(c("id", "label") %in% names(variables))) {
    cli::cli_abort("{.arg variables} must have {.field id} and {.field label}.")
  }
  if (!is.data.frame(arcs) || !all(c("from", "to") %in% names(arcs))) {
    cli::cli_abort("{.arg arcs} must have {.field from} and {.field to}.")
  }
  if (is.null(variables$role)) variables$role <- "variable"
  unknown <- setdiff(adjusted, variables$id)
  if (length(unknown)) {
    cli::cli_abort("{.arg adjusted} names variable{?s} that do not exist: {.val {unknown}}.")
  }

  # A boxed node is the conventional mark for conditioning; an unconditioned
  # variable stays an ellipse. Using one glyph for both would make the adjustment
  # set invisible, which is the one thing the figure has to communicate.
  nodes <- lapply(seq_len(nrow(variables)), function(i) {
    is_adj <- variables$id[i] %in% adjusted
    dg_node(variables$id[i], variables$label[i],
            shape = if (is_adj) "rect" else "ellipse",
            role = variables$role[i], width = 28, height = 12)
  })
  biasing <- if (!is.null(arcs$biasing)) as.logical(arcs$biasing) else rep(FALSE, nrow(arcs))
  edges <- lapply(seq_len(nrow(arcs)), function(i) {
    dg_edge(arcs$from[i], arcs$to[i], if (isTRUE(biasing[i])) "biasing" else "causal")
  })

  p <- diagram(nodes, edges, layout = "layered", direction = "right",
               venue = venue, palette = palette, acyclic = TRUE,
               node_width = 28, node_height = 12, spacing_x = 40, spacing_y = 20, ...)
  attr(p, "vellum_dag") <- list(adjusted = adjusted, roles = variables$role)
  p
}

#' Compartmental transmission model
#'
#' @description
#' A state-transition diagram of the SIR family. Every transition must name its
#' rate: a diagram showing S going to I without saying at what rate is a picture
#' of a model rather than a statement of one, and cannot be re-implemented from
#' the figure.
#'
#' @param states A character vector of state names, or a data frame with `id` and
#'   `label`.
#' @param transitions A data frame with `from`, `to` and `rate`, where `rate` is
#'   the parameter label such as `"beta S I / N"`.
#' @param venue Venue.
#' @param direction Reading direction.
#' @param ... Passed to [diagram()].
#'
#' @return A ggplot.
#' @export
#' @examples
#' fig_compartmental(
#'   states = c(S = "Susceptible", E = "Exposed", I = "Infectious", R = "Recovered"),
#'   transitions = data.frame(
#'     from = c("S", "E", "I"),
#'     to   = c("E", "I", "R"),
#'     rate = c("beta S I / N", "sigma", "gamma")
#'   )
#' )
fig_compartmental <- function(states, transitions, venue = "nature",
                              direction = "right", ...) {
  if (is.character(states)) {
    ids <- names(states) %||% states
    labels <- unname(states)
  } else if (is.data.frame(states) && all(c("id", "label") %in% names(states))) {
    ids <- states$id
    labels <- states$label
  } else {
    cli::cli_abort("{.arg states} must be a named character vector or a data frame with {.field id} and {.field label}.")
  }
  if (!is.data.frame(transitions) || !all(c("from", "to") %in% names(transitions))) {
    cli::cli_abort("{.arg transitions} must have {.field from} and {.field to}.")
  }
  if (is.null(transitions$rate) || any(is.na(transitions$rate)) ||
        !all(nzchar(as.character(transitions$rate)))) {
    cli::cli_abort(c("Every transition must name its rate.",
                     i = "A transition without a rate cannot be re-implemented from the figure.",
                     i = "Add a {.field rate} column to {.arg transitions}."))
  }

  nodes <- lapply(seq_along(ids), function(i) {
    dg_node(ids[i], labels[i], shape = "round", role = "state",
            width = 22, height = 14)
  })
  edges <- lapply(seq_len(nrow(transitions)), function(i) {
    dg_edge(transitions$from[i], transitions$to[i], "flow",
            label = as.character(transitions$rate[i]))
  })
  diagram(nodes, edges, layout = "layered", direction = direction, venue = venue,
          node_width = 22, node_height = 14, spacing_x = 36, spacing_y = 24, ...)
}

#' Molecular pathway diagram
#'
#' @description
#' A signalling or metabolic pathway with typed relations and optional
#' compartments. The relation type is required rather than defaulted, because
#' "A relates to B" is not a mechanism and the whole content of a pathway figure
#' is which of activation, inhibition, binding, translocation, catalysis or
#' conversion is being claimed.
#'
#' @param entities A data frame with `id`, `label`, an optional `kind` among
#'   `"ligand"`, `"receptor"`, `"kinase"`, `"tf"`, `"gene"`, `"metabolite"`,
#'   `"drug"`, `"complex"`, and an optional `compartment`.
#' @param relations A data frame with `from`, `to`, `type` and an optional
#'   `label` for a rate, site or constant.
#' @param compartments Optional named character vector, `id = label`, giving the
#'   drawing order and titles of the compartments named in `entities`.
#' @param venue,palette Passed through.
#' @param ... Passed to [diagram()].
#'
#' @return A ggplot.
#' @export
#' @examples
#' entities <- data.frame(
#'   id = c("lig", "rec", "kin", "tf"),
#'   label = c("Ligand", "Receptor", "Kinase", "Transcription factor"),
#'   kind = c("ligand", "receptor", "kinase", "tf")
#' )
#' relations <- data.frame(
#'   from = c("lig", "rec", "kin"),
#'   to = c("rec", "kin", "tf"),
#'   type = c("binding", "activation", "activation")
#' )
#' fig_pathway(entities, relations)
fig_pathway <- function(entities, relations, compartments = NULL,
                        venue = "nature", palette = "okabe_ito", ...) {
  if (!is.data.frame(entities) || !all(c("id", "label") %in% names(entities))) {
    cli::cli_abort("{.arg entities} must have {.field id} and {.field label}.")
  }
  if (!is.data.frame(relations) || !all(c("from", "to", "type") %in% names(relations))) {
    cli::cli_abort(c("{.arg relations} must have {.field from}, {.field to} and {.field type}.",
                     i = "The type decides the arrowhead, and the arrowhead is the mechanism."))
  }
  kind <- if (is.null(entities$kind)) rep("entity", nrow(entities)) else entities$kind
  shape <- .pathway_shapes[kind]
  shape[is.na(shape)] <- "round"

  nodes <- lapply(seq_len(nrow(entities)), function(i) {
    dg_node(entities$id[i], entities$label[i], shape = unname(shape[i]),
            role = kind[i], width = 24, height = 11)
  })
  edges <- lapply(seq_len(nrow(relations)), function(i) {
    dg_edge(relations$from[i], relations$to[i], relations$type[i],
            label = if (is.null(relations$label)) NA_character_ else relations$label[i])
  })

  comps <- NULL
  if (!is.null(entities$compartment) && any(!is.na(entities$compartment))) {
    ids <- unique(stats::na.omit(entities$compartment))
    labels <- if (is.null(compartments)) stats::setNames(ids, ids) else compartments
    comps <- lapply(ids, function(cid) {
      dg_compartment(cid, unname(labels[cid] %none% cid),
                     contains = entities$id[which(entities$compartment == cid)])
    })
  }
  diagram(nodes, edges, comps, layout = "layered", direction = "down",
          venue = venue, palette = palette, node_width = 24, node_height = 11, ...)
}

# Shape carries the kind of molecule, so a reader can tell a receptor from a gene
# without reading every label. The assignments follow the conventions of pathway
# illustration rather than inventing new ones.
.pathway_shapes <- c(
  ligand = "ellipse", receptor = "round", kinase = "round", tf = "hexagon",
  gene = "parallelogram", metabolite = "diamond", drug = "stadium",
  complex = "hexagon", entity = "round"
)

#' Staged workflow or analysis pipeline
#'
#' @description
#' A laboratory protocol or bioinformatics pipeline as a sequence of phases, each
#' with its own steps. Used for the figure that has to let a reader reproduce a
#' method, so the parameters belong on the figure rather than only in the text.
#'
#' @param steps A data frame with `id`, `label`, an optional `phase` grouping
#'   consecutive steps, and an optional `detail` line for parameters such as
#'   `"1:1000, 4 C overnight"`.
#' @param direction `"right"` or `"down"`.
#' @param venue,palette Passed through.
#' @param ... Passed to [diagram()].
#'
#' @return A ggplot.
#' @export
#' @examples
#' steps <- data.frame(
#'   id = c("collect", "extract", "amplify", "sequence", "call"),
#'   label = c("Sample collection", "DNA extraction", "PCR", "Sequencing", "Variant calling"),
#'   phase = c("Wet lab", "Wet lab", "Wet lab", "Wet lab", "Analysis"),
#'   detail = c("-80 C within 2 h", "column kit", "35 cycles", "2 x 150 bp", "GATK 4.5")
#' )
#' fig_pipeline(steps)
fig_pipeline <- function(steps, direction = c("right", "down"), venue = "nature",
                         palette = "okabe_ito", ...) {
  direction <- match.arg(direction)
  if (!is.data.frame(steps) || !all(c("id", "label") %in% names(steps))) {
    cli::cli_abort("{.arg steps} must have {.field id} and {.field label}.")
  }
  label <- steps$label
  if (!is.null(steps$detail)) {
    has <- !is.na(steps$detail) & nzchar(steps$detail)
    label[has] <- paste0(label[has], "\n", steps$detail[has])
  }
  phase <- if (is.null(steps$phase)) rep("step", nrow(steps)) else steps$phase

  nodes <- lapply(seq_len(nrow(steps)), function(i) {
    dg_node(steps$id[i], label[i], shape = "round", role = phase[i],
            width = 30, height = 14)
  })
  edges <- if (nrow(steps) > 1L) {
    lapply(seq_len(nrow(steps) - 1L), function(i) dg_edge(steps$id[i], steps$id[i + 1L], "flow"))
  } else {
    NULL
  }
  comps <- if (!is.null(steps$phase)) {
    lapply(unique(phase), function(ph) {
      dg_compartment(ph, ph, contains = steps$id[phase == ph], label_position = "top")
    })
  } else {
    NULL
  }
  diagram(nodes, edges, comps, layout = "layered", direction = direction,
          venue = venue, palette = palette, node_width = 30, node_height = 14,
          spacing_x = 40, spacing_y = 24, ...)
}

#' Study timeline
#'
#' @description
#' A horizontal timeline of study periods and assessment points, drawn on a real
#' numeric axis. Timelines drawn as evenly spaced boxes misrepresent the spacing
#' of the visits, which is usually the thing a reader most needs to judge.
#'
#' @param events A data frame with `time`, `label`, and an optional `group` for
#'   parallel tracks such as study arms.
#' @param periods Optional data frame with `start`, `end`, `label` for shaded
#'   phases such as run-in, treatment and follow-up.
#' @param time_label Axis title, for instance `"Weeks from randomisation"`.
#' @param venue Venue.
#'
#' @return A ggplot.
#' @export
#' @examples
#' events <- data.frame(
#'   time = c(-2, 0, 4, 12, 24),
#'   label = c("Screening", "Randomisation", "Visit 1", "Visit 2", "Primary endpoint")
#' )
#' periods <- data.frame(start = c(0, 12), end = c(12, 24),
#'                       label = c("Treatment", "Follow-up"))
#' fig_timeline(events, periods, time_label = "Weeks from randomisation")
fig_timeline <- function(events, periods = NULL, time_label = "Time",
                         venue = "nature") {
  if (!is.data.frame(events) || !all(c("time", "label") %in% names(events))) {
    cli::cli_abort("{.arg events} must have {.field time} and {.field label}.")
  }
  check_numeric(events$time, "events$time")
  check_string(time_label, "time_label")
  tk <- vellum_tokens(venue)
  group <- if (is.null(events$group)) rep("", nrow(events)) else as.character(events$group)
  events$.group <- group
  events$.y <- match(group, unique(group))

  p <- ggplot2::ggplot()
  if (!is.null(periods)) {
    if (!all(c("start", "end", "label") %in% names(periods))) {
      cli::cli_abort("{.arg periods} must have {.field start}, {.field end} and {.field label}.")
    }
    periods$.fill <- dg_tint(vellum_pal("okabe_ito", n = max(nrow(periods), 1L)), 0.16)
    p <- p +
      ggplot2::geom_rect(
        data = periods,
        ggplot2::aes(xmin = .data$start, xmax = .data$end,
                     ymin = -Inf, ymax = Inf),
        fill = periods$.fill, colour = NA, inherit.aes = FALSE) +
      ggplot2::geom_text(
        data = periods,
        ggplot2::aes(x = (.data$start + .data$end) / 2, y = max(events$.y) + 0.62,
                     label = .data$label),
        size = fs(tk$type$size$small), colour = tk$ink$weak,
        family = tk$type$family, inherit.aes = FALSE)
  }
  p +
    ggplot2::geom_segment(
      data = data.frame(y = unique(events$.y)),
      ggplot2::aes(x = min(events$time), xend = max(events$time),
                   y = .data$y, yend = .data$y),
      colour = tk$ink$weak, linewidth = tk$rule$hairline, inherit.aes = FALSE) +
    ggplot2::geom_point(
      data = events, ggplot2::aes(x = .data$time, y = .data$.y),
      size = ps(1.7), colour = tk$ink$text, inherit.aes = FALSE) +
    ggplot2::geom_text(
      data = events,
      ggplot2::aes(x = .data$time, y = .data$.y + 0.16, label = .data$label),
      angle = 35, hjust = 0, vjust = 0, size = fs(tk$type$size$annot),
      colour = tk$ink$text, family = tk$type$family, inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = unique(events$.y),
      labels = unique(events$.group),
      limits = c(0.4, max(events$.y) + 1.15)) +
    ggplot2::labs(x = time_label, y = NULL) +
    theme_vellum(venue = venue) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}
