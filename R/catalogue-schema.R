# A structural schema over the chart taxonomy.
#
# `catalogue_rows()` classifies forms by the QUESTION they answer -- distribution,
# comparison, relationship, and so on -- because that is what someone with a
# dataset and a finding is actually searching on. That axis cannot answer two
# other questions, and both come up constantly:
#
#   * "what shape is my data?" A reader arrives with a hierarchy, a network, or
#     one variable, and needs the forms that consume that shape.
#   * "how accurately can this be read?" Every form puts its main comparison on
#     some perceptual channel, and those channels are not equal. This is the axis
#     that explains why a waffle beats a donut, and it is an ordering rather than
#     a taste.
#
# So the schema is two extra columns rather than a replacement. The vocabularies
# below are closed sets, checked in the tests, because a taxonomy whose terms
# drift is a list.

# The shape of the data the form consumes.
.chart_structures <- c(
  univariate   = "One variable, with or without a grouping",
  bivariate    = "Two variables measured on the same units",
  multivariate = "Three or more variables per unit",
  matrix       = "A rectangular grid of values indexed by row and column",
  hierarchy    = "A rooted tree with magnitude at the leaves",
  network      = "Nodes and the weighted edges between them",
  sequence     = "Values in an intrinsic order, usually time",
  geospatial   = "Values attached to locations or geometries",
  process      = "Declared stages or relations, not measured data"
)

# The perceptual channel the form's PRIMARY comparison rides on, ordered by the
# accuracy with which people decode it. Cleveland & McGill (1984) established the
# first seven experimentally and Heer & Bostock (2010) replicated them; the last
# four are Munzner's channels for structures that have no position to compare.
#
# The ordering is the useful part. A form near the bottom of this list is not
# forbidden, but it should be chosen knowing that a form nearer the top would
# have shown the same comparison more accurately.
.chart_encodings <- c(
  # Counting sits outside the experimental ordering rather than at the top of it.
  # Cleveland and McGill measured how accurately people ESTIMATE a magnitude from
  # a channel; enumeration is not an estimate, so it is not on that scale at all.
  # It heads the list because an exact read cannot be bettered by an estimated
  # one, and it holds only while the marks are few enough to count -- which is
  # the whole design constraint of a waffle chart.
  count              = "Enumeration of discrete marks, exact rather than estimated",
  position_aligned   = "Position on a common aligned scale",
  position_unaligned = "Position on identical but unaligned scales",
  length             = "Length of a mark",
  slope              = "Slope or direction of a segment",
  angle              = "Angle subtended at a point",
  area               = "Area of a mark",
  volume             = "Apparent volume or depth",
  luminance          = "Ordered lightness or saturation",
  hue                = "Categorical hue, which has no order",
  connection         = "Being joined to something else",
  density            = "Local density of overplotted marks"
)
# Containment is deliberately absent. It is real -- it is how a treemap conveys
# nesting -- but it is never the channel a form's PRIMARY magnitude comparison
# rides on, and this column records that channel. A vocabulary term with no
# members is an invitation to drift.

# chart -> c(structure, primary encoding).
.chart_schema <- list(
  "dot strip"                  = c("univariate", "position_aligned"),
  "raincloud"                  = c("univariate", "position_aligned"),
  "box plot"                   = c("univariate", "position_aligned"),
  "violin plot"                = c("univariate", "area"),
  "histogram"                  = c("univariate", "length"),
  "density plot"               = c("univariate", "position_aligned"),
  "ridgeline"                  = c("univariate", "position_aligned"),
  "ECDF"                       = c("univariate", "position_aligned"),
  "Q-Q plot"                   = c("univariate", "position_aligned"),

  "estimate interval"          = c("univariate", "position_aligned"),
  "forest plot"                = c("univariate", "position_aligned"),
  "paired slope"               = c("bivariate", "slope"),
  "lollipop ranking"           = c("univariate", "position_aligned"),
  "beeswarm"                   = c("univariate", "position_aligned"),
  "Gardner-Altman"             = c("univariate", "position_aligned"),
  "tornado"                    = c("univariate", "length"),

  "scatter plot"               = c("bivariate", "position_aligned"),
  "bubble plot"                = c("multivariate", "area"),
  "hexbin"                     = c("bivariate", "luminance"),
  "contour field"              = c("multivariate", "position_aligned"),
  "response surface"           = c("multivariate", "volume"),
  "allometry"                  = c("bivariate", "position_aligned"),

  "line chart"                 = c("sequence", "position_aligned"),
  "trajectory plot"            = c("sequence", "position_aligned"),
  "step-change plot"           = c("sequence", "position_aligned"),
  "seasonal polar"             = c("sequence", "position_unaligned"),
  "horizon graph"              = c("sequence", "luminance"),

  "Kaplan-Meier"               = c("sequence", "position_aligned"),
  "cumulative incidence"       = c("sequence", "position_aligned"),

  "ROC curve"                  = c("bivariate", "position_aligned"),
  "precision-recall curve"     = c("bivariate", "position_aligned"),
  "calibration curve"          = c("bivariate", "position_aligned"),
  "Bland-Altman"               = c("bivariate", "position_aligned"),
  "decision curve"             = c("sequence", "position_aligned"),

  "funnel plot"                = c("bivariate", "position_aligned"),
  "risk-of-bias matrix"        = c("matrix", "hue"),
  "orchard plot"               = c("univariate", "position_aligned"),
  "GOSH plot"                  = c("bivariate", "density"),

  "correlation matrix"         = c("matrix", "luminance"),
  "PCA biplot"                 = c("multivariate", "position_aligned"),
  "parallel coordinates"       = c("multivariate", "slope"),
  "radar chart"                = c("multivariate", "area"),
  "clustered heatmap"          = c("matrix", "luminance"),

  "volcano plot"               = c("bivariate", "position_aligned"),
  "MA plot"                    = c("bivariate", "position_aligned"),
  "Manhattan plot"             = c("sequence", "position_aligned"),

  "stacked area"               = c("sequence", "length"),
  "waffle"                     = c("univariate", "count"),
  "donut"                      = c("univariate", "angle"),
  "alluvial"                   = c("network", "length"),
  "Marimekko"                  = c("matrix", "area"),

  "heatmap"                    = c("matrix", "luminance"),
  "tile map"                   = c("matrix", "luminance"),
  "calendar heatmap"           = c("matrix", "luminance"),

  "choropleth"                 = c("geospatial", "luminance"),
  "proportional symbols"       = c("geospatial", "area"),
  "dot-density map"            = c("geospatial", "density"),
  "flow map"                   = c("geospatial", "length"),
  "cartogram"                  = c("geospatial", "area"),

  "small-multiple map"         = c("geospatial", "luminance"),
  "change choropleth"          = c("geospatial", "luminance"),
  "space-time cube"            = c("geospatial", "volume"),
  "animated map"               = c("geospatial", "luminance"),

  "risk matrix"                = c("matrix", "position_unaligned"),
  "cost-effectiveness plane"   = c("bivariate", "density"),
  "CEAC"                       = c("sequence", "position_aligned"),
  "value-of-information curve" = c("sequence", "position_aligned"),

  "node-link graph"            = c("network", "connection"),
  "Sankey diagram"             = c("network", "length"),
  "chord diagram"              = c("network", "length"),

  "treemap"                    = c("hierarchy", "area"),
  "icicle"                     = c("hierarchy", "length"),
  "sunburst"                   = c("hierarchy", "angle"),
  "dendrogram"                 = c("hierarchy", "connection"),

  "flow diagram"               = c("process", "connection"),
  "CONSORT flow"               = c("process", "connection"),
  "causal DAG"                 = c("process", "connection"),
  "analysis pipeline"          = c("process", "connection"),
  "compartmental model"        = c("process", "connection"),
  "pathway diagram"            = c("process", "connection"),
  "study timeline"             = c("sequence", "position_aligned"),
  "projected surface"          = c("multivariate", "volume"),
  "albatross plot"             = c("bivariate", "position_aligned"),
  "PRISMA flow" = c("process", "connection"),
  "summary ROC" = c("bivariate", "position_aligned"),
  "Baujat plot" = c("bivariate", "position_aligned"),
  "L'Abbe plot" = c("bivariate", "position_aligned"),
  "cumulative meta-analysis" = c("univariate", "position_aligned"),
  "network geometry" = c("network", "connection"),
  "rankogram" = c("sequence", "position_aligned"),
  "covariate balance" = c("univariate", "position_aligned"),
  "event study" = c("sequence", "position_aligned"),
  "regression discontinuity" = c("bivariate", "position_aligned"),
  "Lexis diagram" = c("bivariate", "position_aligned"),
  "epidemic curve" = c("sequence", "length"),
  "bar chart" = c("univariate", "length"),
  "scatterplot matrix" = c("multivariate", "position_aligned"),
  "UpSet plot" = c("matrix", "length"),
  "waterfall" = c("univariate", "length"),
  "streamgraph" = c("sequence", "length"),
  "bump chart" = c("sequence", "position_aligned"),
  "population pyramid" = c("univariate", "length"),
  "swimmer plot" = c("sequence", "length"),
  "Markov trace" = c("sequence", "length"),
  "efficiency frontier" = c("bivariate", "position_aligned"),
  "EVPPI" = c("univariate", "length")
)

#' The vocabularies the chart schema is built from
#'
#' @description
#' The two closed sets that classify every form in [chart_catalogue()]:
#' `structure`, the shape of the data a form consumes, and `encoding`, the
#' perceptual channel its primary comparison rides on.
#'
#' The encoding vocabulary is returned in order of decoding accuracy, from
#' position on a common scale down to density. That ordering is the point of it.
#' It is why [fig_waffle()] is offered as the alternative to [fig_donut()] and
#' not the reverse, and it is a body of measurement rather than a preference.
#'
#' @param what `"structure"` or `"encoding"`.
#' @return A data frame of terms and their definitions, in schema order.
#' @references Cleveland WS, McGill R (1984) Graphical perception. *JASA*
#'   79:531-554. Heer J, Bostock M (2010) Crowdsourcing graphical perception.
#'   *CHI '10*. Munzner T (2014) *Visualization Analysis and Design*.
#' @export
#' @examples
#' chart_vocabulary("structure")
#' chart_vocabulary("encoding")
chart_vocabulary <- function(what = c("structure", "encoding")) {
  what <- match.arg(what)
  x <- if (what == "structure") .chart_structures else .chart_encodings
  out <- data.frame(term = names(x), definition = unname(x),
                    stringsAsFactors = FALSE)
  if (what == "encoding") out$accuracy_rank <- seq_len(nrow(out))
  out
}

chart_schema_columns <- function(charts, call = rlang::caller_env()) {
  unknown <- setdiff(charts, names(.chart_schema))
  if (length(unknown)) {
    cli::cli_abort(c(
      "No structural classification for {.val {unknown}}.",
      i = "Every catalogue row must be classified; see {.code vellum:::.chart_schema}."
    ), call = call)
  }
  m <- do.call(rbind, .chart_schema[charts])
  data.frame(structure = m[, 1], encoding = m[, 2], stringsAsFactors = FALSE)
}
