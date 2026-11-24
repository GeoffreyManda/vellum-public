# Institutional identities and journal art direction ------------------------
#
# Identity is deliberately separate from venue geometry, analytical profile and
# data semantics. An organisation may own a recognisable visual language; it
# does not get to override a journal's artwork specification or the requirement
# for an accessible encoding.

.identity_accessible <- c(
  black = "#000000", orange = "#E69F00", sky = "#56B4E9",
  green = "#009E73", yellow = "#F0E442", blue = "#0072B2",
  vermilion = "#D55E00", purple = "#CC79A7"
)

.identity_pick <- function(x, pattern, fallback) {
  hit <- grep(pattern, names(x), ignore.case = TRUE, value = TRUE)
  if (length(hit)) unname(x[[hit[1]]]) else fallback
}

.identity_semantic <- function(core, categorical) {
  c(
    primary = unname(core[["primary"]]),
    secondary = unname(core[["secondary"]]),
    emphasis = .identity_pick(categorical, "orange|gold|yellow", unname(core[["primary"]])),
    positive = .identity_pick(categorical, "green|teal", unname(core[["secondary"]])),
    warning = .identity_pick(categorical, "orange|gold|yellow", "#E69F00"),
    negative = .identity_pick(categorical, "red|vermilion|burgundy|coral", "#C43D4D"),
    information = unname(core[["primary"]]),
    neutral = unname(core[["grid"]]),
    missing = unname(core[["na"]]),
    background = unname(core[["paper"]]),
    surface = unname(core[["panel"]]),
    text = unname(core[["ink"]]),
    text_muted = unname(core[["weak"]]),
    grid_major = unname(core[["grid"]]),
    grid_minor = grDevices::adjustcolor(core[["grid"]], alpha.f = 0.55)
  )
}

.provenance_levels <- c(
  "official_public", "official_legacy", "official_observed",
  "private_inferred", "journal_inspired"
)

.identity_entry <- function(name, label, type, status, source, fonts, core,
                            categorical, sequential, diverging,
                            accessible = .identity_accessible,
                            venue = "portfolio", note = "", retrieved = "2026-07-30",
                            palette_source = source, font_source = source,
                            provenance = NULL, confidence = NULL,
                            source_version = NULL, semantic = NULL,
                            geographic = NULL, layout = list(), chart_rules = list(),
                            table_rules = list(), legal = list(), sources = NULL) {
  provenance <- provenance %||% switch(
    status,
    official = "official_public",
    `official-data-system` = "official_public",
    observed = "official_observed",
    `editorial-overlay` = "journal_inspired",
    "official_observed"
  )
  confidence <- confidence %||% switch(
    provenance,
    official_public = 1, official_legacy = 0.82,
    official_observed = 0.78, private_inferred = 0.62,
    journal_inspired = 0.70
  )
  source_version <- source_version %||% "See primary source"
  semantic <- semantic %||% .identity_semantic(core, categorical)
  geographic <- geographic %||% categorical
  layout <- utils::modifyList(list(
    alignment = "left", whitespace = "structural", hierarchy = "typographic",
    direct_labels = TRUE, logo_placement = "disabled"
  ), layout)
  chart_rules <- utils::modifyList(list(
    colour_is_redundant = TRUE, uncertainty_visible = TRUE,
    missing_distinct_from_zero = TRUE, direct_labels_preferred = TRUE,
    max_brand_categories = min(6L, length(categorical))
  ), chart_rules)
  table_rules <- utils::modifyList(list(
    decimal_alignment = TRUE, units_consistent = TRUE,
    uncertainty_subordinate_but_visible = TRUE, vertical_rules = FALSE
  ), table_rules)
  legal <- utils::modifyList(list(
    logos_included = FALSE, endorsement_implied = FALSE,
    permission_required_for_marks = TRUE
  ), legal)
  sources <- unique(sources %||% c(source, palette_source, font_source))
  list(
    name = name, label = label, type = type, status = status,
    provenance = provenance, confidence = confidence,
    verified_on = as.Date(retrieved), source_version = source_version,
    source = source, palette_source = palette_source, font_source = font_source,
    retrieved = retrieved, venue = venue, note = note,
    fonts = fonts, core = core, semantic = semantic,
    layout = layout, chart_rules = chart_rules, table_rules = table_rules,
    legal = legal, sources = sources,
    palettes = list(
      categorical = categorical, sequential = sequential,
      diverging = diverging, accessible = accessible,
      identity = unname(core[c("primary", "secondary")]),
      corporate = categorical,
      status = unname(semantic[c("positive", "warning", "negative", "neutral")]),
      geographic = geographic
    )
  )
}

.vellum_identities <- list(
  who = .identity_entry(
    "who", "World Health Organization", "institution", "official-data-system",
    "https://apps.who.int/gho/data/design-language/design-system/colors/",
    list(body = c("Noto Sans", "Source Sans 3", "Arial", "Helvetica", "sans"),
         display = c("Noto Sans", "Source Sans 3", "Arial", "Helvetica", "sans"),
         mono = c("Noto Sans Mono", "IBM Plex Mono", "Courier", "Courier New", "mono")),
    c(primary = "#008DC9", secondary = "#0F2D5B", tint = "#90D3FB",
      ink = "#000000", weak = "#595959", paper = "#FFFFFF",
      panel = "#F7F7F7", grid = "#CCCCCC", na = "#CCCCCC"),
    c(orange = "#F4A81D", vermilion = "#F26829", magenta = "#BD53BD",
      violet = "#6363C0", blue = "#008DC9", green = "#40BF73"),
    c("#D7E6F3", "#75B6D6", "#0582BB", "#0F2D5B"),
    c("#A00016", "#DA484A", "#F8908B", "#F7F7F7", "#90D3FB", "#008DC9", "#0F2D5B"),
    note = "WHO Data Design Language v0.9.2. The WHO emblem is not included and requires permission.",
    font_source = "https://apps.who.int/gho/data/design-language/design-system/typography/"
  ),
  unicef = .identity_entry(
    "unicef", "UNICEF", "institution", "official",
    "https://www.unicef.org/jordan/media/7166/file/ANNEX_G_-_Brand_book_V3.1.pdf",
    list(body = c("Univers LT Pro", "Source Sans 3", "Noto Sans", "Arial", "Roboto", "Helvetica", "sans"),
         display = c("Univers LT Pro", "Montserrat", "Source Sans 3", "Arial", "Roboto", "Helvetica", "sans"),
         accent = c("Aleo", "Source Serif 4", "Times New Roman", "serif"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#1CABE2", secondary = "#374EA2", ink = "#2D2926",
      weak = "#777779", paper = "#FFFFFF", panel = "#F5F8FA",
      grid = "#D8D1C9", na = "#D8D1C9"),
    c(blue = "#1CABE2", blue_violet = "#374EA2", green = "#00833D",
      light_green = "#80BD41", yellow = "#FFC20E", orange = "#F26A21",
      red = "#E2231A", burgundy = "#961A49", purple = "#6A1E74"),
    c("#EAF8FD", "#A8DCF0", "#58C1E8", "#1CABE2", "#087AA8"),
    c("#961A49", "#E2231A", "#F6B5B0", "#F7F7F7", "#A8DCF0", "#1CABE2", "#374EA2"),
    note = "UNICEF Blue should dominate secondary colours. Marks and logos are not included."
  ),
  world_bank = .identity_entry(
    "world_bank", "World Bank Group", "institution", "official-data-system",
    "https://worldbank.github.io/data-visualization-style-guide/colors",
    list(body = c("Open Sans", "Arial", "Helvetica", "sans"),
         display = c("Open Sans", "Arial", "Helvetica", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#0071BC", secondary = "#002244", ink = "#111111",
      weak = "#666666", paper = "#FFFFFF", panel = "#F7F7F7",
      grid = "#CED4DE", na = "#CED4DE"),
    c(blue = "#34A7F2", orange = "#FF9800", violet = "#664AB6",
      teal = "#4EC2C0", pink = "#F3578E", navy = "#081079",
      green = "#0C7C68", red = "#AA0000", olive = "#767712"),
    c("#E3F6FD", "#75CCEC", "#089BD4", "#0169A1", "#023B6F"),
    c("#920000", "#BD6126", "#E3A763", "#EFEFEF", "#80BDE7", "#3587C3", "#025288"),
    accessible = c("#106CA1", "#B65F0C", "#664AB6", "#208383",
                   "#BB3B64", "#081079", "#0C7C68", "#AA0000", "#767712"),
    note = "The official World Bank data-visualisation guide identifies itself as a draft.",
    font_source = "https://worldbank.github.io/data-visualization-style-guide/typography"
  ),
  usaid = .identity_entry(
    "usaid", "USAID", "institution", "official",
    "https://www.usaid.gov/sites/default/files/2022-12/BrandingMarking-QRG-09-13-21.pdf",
    list(body = c("Gill Sans", "Source Sans 3", "Cabin", "Arial", "Helvetica", "sans"),
         display = c("Gill Sans", "Cabin", "Source Sans 3", "Arial", "Helvetica", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#002F6C", secondary = "#BA0C2F", ink = "#212721",
      weak = "#6C6463", paper = "#FFFFFF", panel = "#F4F7FA",
      grid = "#A7C6ED", na = "#B8B8B8"),
    c(navy = "#002F6C", red = "#BA0C2F", medium_blue = "#0067B9",
      light_blue = "#A7C6ED", web_blue = "#205493", dark_red = "#651D32",
      dark_grey = "#6C6463"),
    c("#E9F1FA", "#A7C6ED", "#5C96CF", "#0067B9", "#002F6C"),
    c("#651D32", "#BA0C2F", "#E6A6B4", "#F7F7F7", "#A7C6ED", "#0067B9", "#002F6C"),
    note = "Use of USAID marks and co-branding remains governed by the official marking rules."
  ),
  undp = .identity_entry(
    "undp", "United Nations Development Programme", "institution", "official-data-system",
    "https://data-viz.data.undp.org/color-palette",
    list(body = c("Proxima Nova", "Inter", "Source Sans 3", "Arial", "Helvetica", "sans"),
         display = c("Sohne Breit", "S\u00f6hne Breit", "Inter Tight", "Lora", "Arial", "sans"),
         accent = c("Lora", "Times New Roman", "serif"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#006EB5", secondary = "#02A38A", ink = "#232E3D",
      weak = "#55606E", paper = "#FFFFFF", panel = "#F7F7F7",
      grid = "#D4D6D8", na = "#D4D6D8"),
    c(blue = "#006EB5", cyan = "#5DD4F0", teal = "#02A38A",
      orange = "#E78625", pink = "#E0529E", violet = "#757AF0",
      lime = "#A0C236", yellow = "#FBC412", ochre = "#BF8B2E", grey = "#A9B1B7"),
    c("#B5D5F5", "#94C4F5", "#6BABEB", "#4F95DD", "#3288CE", "#006EB5", "#1F5A95"),
    c("#E0529E", "#E78625", "#FBC412", "#F7F7F7", "#5DD4F0", "#006EB5", "#1F5A95"),
    note = "Colours follow the current UNDP data-viz library; typography records the 2020 manual and current digital practice.",
    font_source = "https://www.undp.org/sites/g/files/zskgke326/files/2021-08/UNDP%20Brand%20Manual%20rev.2020.pdf"
  ),
  kpmg = .identity_entry(
    "kpmg", "KPMG", "institution", "official",
    "https://assets.kpmg.com/content/dam/kpmg/dk/pdf/dk-2020/12/alexander.pdf",
    list(body = c("Univers", "Source Sans 3", "Noto Sans", "Arial", "Helvetica", "sans"),
         display = c("KPMG Font", "Inter Tight", "Archivo Narrow", "Arial", "Helvetica", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#00338D", secondary = "#005EB8", ink = "#212121",
      weak = "#666666", paper = "#FFFFFF", panel = "#F4F6FA",
      grid = "#D5DCE8", na = "#B8B8B8"),
    c(blue = "#00338D", medium_blue = "#005EB8", light_blue = "#0091DA",
      violet = "#483698", purple = "#470A68", light_purple = "#6D2077",
      teal = "#00A3A1"),
    c("#EAF5FB", "#9FD6F2", "#43AFE4", "#0091DA", "#005EB8", "#00338D"),
    c("#470A68", "#6D2077", "#BDA7C7", "#F7F7F7", "#8DD6D5", "#00A3A1", "#00338D"),
    note = "The proprietary KPMG Font and Univers resolve to Arial when unavailable."
  ),
  bdo = .identity_entry(
    "bdo", "BDO", "institution", "observed",
    "https://www.bdo.global/getmedia/43063307-3a15-4581-a74b-65add671c248/Consolidation-brochure_V13_23062020_Final3.pdf.aspx",
    list(body = c("Trebuchet MS", "Source Sans 3", "Noto Sans", "Arial", "Helvetica", "sans"),
         display = c("Proxima Nova", "Inter", "Trebuchet MS", "Arial", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#ED1A3B", secondary = "#22409A", ink = "#292929",
      weak = "#666666", paper = "#FFFFFF", panel = "#F6F7FA",
      grid = "#D9DDE8", na = "#B8B8B8"),
    c(red = "#ED1A3B", blue = "#22409A", navy = "#182B54",
      coral = "#F05A67", cyan = "#2A9FD6", gold = "#D99A1B"),
    c("#F3F6FC", "#B9C5E7", "#7087C7", "#22409A", "#182B54"),
    c("#9B1730", "#ED1A3B", "#F6B5C0", "#F7F7F7", "#9FCDEA", "#2A9FD6", "#22409A"),
    note = "Observed public-output preset: the current global brand manual is not public. Confirm local BDO guidance before external use."
  ),
  atni = .identity_entry(
    "atni", "Access to Nutrition Initiative", "institution", "observed",
    "https://accesstonutrition.org/",
    list(body = c("Lato", "Source Sans 3", "Arial", "Helvetica", "sans"),
         display = c("Lato", "Source Serif 4", "Arial", "Helvetica", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#9658AD", secondary = "#0099D9", ink = "#111111",
      weak = "#787878", paper = "#FFFFFF", panel = "#F2F2F2",
      grid = "#E4E4E4", na = "#E4E4E4"),
    c(purple = "#9658AD", blue = "#0099D9", green = "#3F783F",
      yellow = "#ECC441", orange = "#EE952F", red = "#B63735", pink = "#FC7984"),
    c("#F4EDF7", "#D9C4E2", "#B88FC8", "#9658AD", "#704083"),
    c("#B63735", "#EE952F", "#F2D58A", "#F2F2F2", "#9AD4EC", "#0099D9", "#337D9C"),
    note = "Observed from public ATNi outputs and the ATNi-facing figure guide; not an official public brand manual."
  ),
  unhcr = .identity_entry(
    "unhcr", "UNHCR", "institution", "official-data-system",
    "https://dataviz.unhcr.org/guidance/",
    list(body = c("Lato", "Source Sans 3", "Arial", "Helvetica", "sans"),
         display = c("Proxima Nova", "Inter", "Lato", "Arial", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#0072BC", secondary = "#05568B", ink = "#222222",
      weak = "#666666", paper = "#FFFFFF", panel = "#F5F5F5",
      grid = "#D9D9D9", na = "#B8B8B8"),
    c(blue = "#0072BC", yellow = "#FFC740", green = "#32C189",
      cyan = "#6CD8FD", red = "#D25A45", purple = "#A097E3",
      brown = "#7C3C36", grey = "#BFBFBF"),
    c("#CDE3F1", "#8FC1E1", "#4F9ED0", "#0072BC", "#05568B", "#0B3754"),
    c("#683229", "#D25A45", "#FCE2DE", "#F5F5F5", "#CDE3F1", "#0072BC", "#05568B"),
    accessible = c("#0072BC", "#FFC740", "#32C189", "#6CD8FD",
                   "#D25A45", "#A097E3", "#7C3C36", "#BFBFBF"),
    note = "Data colours, ramps and typography follow UNHCR's official 2026 data-visualisation guidelines."
  ),

  # Journal entries are artwork contracts plus optional editorial overlays.
  nature = .identity_entry(
    "nature", "Nature Portfolio", "journal", "editorial-overlay",
    "https://research-figure-guide.nature.com/figures/preparing-figures-our-specifications/",
    list(body = c("Helvetica", "Arial", "sans"), display = c("Helvetica", "Arial", "sans"),
         mono = c("Courier", "Courier New", "mono")),
    c(primary = "#111111", secondary = "#E64B35", ink = "#111111", weak = "#666666",
      paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#7E6148"),
    c("#F5F5F5", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    c("#B2182B", "#EF8A62", "#F7F7F7", "#67A9CF", "#2166AC"),
    venue = "nature", note = "Nature requires accessible colour and Helvetica/Arial; the editorial colours are optional, not a publisher requirement."
  ),
  science = .identity_entry(
    "science", "Science / AAAS", "journal", "editorial-overlay", "https://www.science.org/",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#3B4992", secondary = "#EE0000", ink = "#1B1919", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280", "#BB0021", "#5F559B", "#A20056"),
    c("#F1F3FA", "#AAB3D8", "#6877B8", "#3B4992", "#202B63"),
    c("#BB0021", "#EE7777", "#F7F7F7", "#76C5C5", "#008280"),
    venue = "science", note = "Editorially inspired overlay; Science venue geometry remains unverified in this package."
  ),
  cell = .identity_entry(
    "cell", "Cell Press", "journal", "editorial-overlay", "https://www.elsevier.com/en-gb/about/policies-and-standards/author/artwork-and-media-instructions",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#00718B", secondary = "#E64B35", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#00718B", "#E64B35", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#7E6148"),
    c("#EDF7F8", "#A9D7DE", "#54A8B8", "#00718B", "#004B5C"),
    c("#B23A2B", "#E64B35", "#F7F7F7", "#65B6C6", "#00718B"),
    venue = "cell", note = "Elsevier accessibility and font guidance is official; the colours are an optional Cell-inspired overlay."
  ),
  lancet = .identity_entry(
    "lancet", "The Lancet", "journal", "editorial-overlay", "https://www.thelancet.com/",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#00468B", secondary = "#ED0000", ink = "#1B1919", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#ADB6B6"),
    c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6"),
    c("#EEF5FA", "#A8CBE1", "#5A97C0", "#00468B", "#002D59"),
    c("#AD002A", "#ED0000", "#F7F7F7", "#66C2D1", "#00468B"),
    venue = "generic", note = "Lancet-inspired editorial palette; not a submission requirement."
  ),
  jama = .identity_entry(
    "jama", "JAMA Network", "journal", "editorial-overlay", "https://jamanetwork.com/journals/jama/pages/instructions-for-authors",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#374E55", secondary = "#DF8F44", ink = "#1F1F1F", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97", "#6A6599", "#80796B"),
    c("#EEF3F4", "#B8C7CB", "#718E96", "#374E55", "#223238"),
    c("#B24745", "#E1A09E", "#F7F7F7", "#8DCCDD", "#007A9F"),
    venue = "generic", note = "JAMA recreates accepted graphics to its own style; this overlay is for visual planning, not exact replication."
  ),
  nejm = .identity_entry(
    "nejm", "New England Journal of Medicine", "journal", "editorial-overlay", "https://www.nejm.org/",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#BC3C29", secondary = "#0072B5", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
    c("#EDF6FA", "#A8D1E5", "#55A4CB", "#0072B5", "#00476F"),
    c("#BC3C29", "#E29B90", "#F7F7F7", "#8FC5DE", "#0072B5"),
    venue = "generic", note = "NEJM-inspired editorial palette; not an official figure palette."
  ),
  bmj = .identity_entry(
    "bmj", "BMJ", "journal", "editorial-overlay", "https://www.bmj.com/about-bmj/resources-authors/article-types/research",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#2A6EBB", secondary = "#F28E2B", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#2A6EBB", "#F28E2B", "#59A14F", "#E15759", "#B07AA1", "#76B7B2", "#4E79A7"),
    c("#EEF4FB", "#A9C5E5", "#6596CE", "#2A6EBB", "#174376"),
    c("#B03A3C", "#E15759", "#F7F7F7", "#91C9C5", "#2A6EBB"),
    venue = "generic", note = "BMJ-inspired editorial palette; confirm the target journal's current author instructions."
  ),
  plos = .identity_entry(
    "plos", "PLOS", "journal", "editorial-overlay", "https://journals.plos.org/plosone/s/figures",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#1F78B4", secondary = "#33A02C", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A", "#E31A1C", "#A6CEE3", "#B2DF8A", "#FDBF6F"),
    c("#EFF7FB", "#A6CEE3", "#65A7CF", "#1F78B4", "#0E4B75"),
    c("#B2182B", "#EF8A62", "#F7F7F7", "#67A9CF", "#2166AC"),
    venue = "plos", note = "PLOS venue dimensions are verified; the palette is an accessible editorial overlay, not a mandated palette."
  ),
  elife = .identity_entry(
    "elife", "eLife", "journal", "editorial-overlay", "https://elifesciences.org/inside-elife/author-guide",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#CF3E4F", secondary = "#087F8C", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#CF3E4F", "#087F8C", "#F3B61F", "#6A4C93", "#2A9D8F", "#E76F51", "#457B9D"),
    c("#EEF7F8", "#A8D7DB", "#55AEB7", "#087F8C", "#04505A"),
    c("#A92D3C", "#CF3E4F", "#F7F7F7", "#65B6BE", "#087F8C"),
    venue = "elife", note = "eLife-inspired editorial palette; venue geometry remains unverified in Vellum."
  ),
  pnas = .identity_entry(
    "pnas", "PNAS", "journal", "editorial-overlay", "https://www.pnas.org/author-center/submitting-your-manuscript",
    list(body = c("Arial", "Helvetica", "sans"), display = c("Arial", "Helvetica", "sans"), mono = c("Courier", "mono")),
    c(primary = "#1B4F72", secondary = "#D35400", ink = "#202020", weak = "#666666", paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    c("#1B4F72", "#D35400", "#2E8B57", "#7D3C98", "#C0392B", "#148F77", "#B7950B"),
    c("#EEF4F7", "#B2CBD9", "#6C9BB5", "#1B4F72", "#102F45"),
    c("#9E3225", "#D35400", "#F7F7F7", "#78AFCA", "#1B4F72"),
    venue = "pnas", note = "PNAS-inspired editorial palette; venue geometry remains unverified in Vellum."
  )
)

.journal_overlay <- function(name, label, primary, secondary, categorical,
                             source, venue = "generic",
                             display = c("Source Serif 4", "Times New Roman", "serif"),
                             body = c("Source Sans 3", "Inter", "Arial", "Helvetica", "sans")) {
  .identity_entry(
    name, label, "journal", "editorial-overlay", source,
    list(body = body, display = display, mono = c("IBM Plex Mono", "Courier", "mono")),
    c(primary = primary, secondary = secondary, ink = "#202020", weak = "#666666",
      paper = "#FFFFFF", panel = "#FFFFFF", grid = "#D9D9D9", na = "#B8B8B8"),
    categorical,
    grDevices::colorRampPalette(c("#F5F5F5", primary), space = "Lab")(6),
    grDevices::colorRampPalette(c(secondary, "#F7F7F7", primary), space = "Lab")(7),
    venue = venue,
    note = paste0(label, "-inspired editorial system; not an official author palette."),
    provenance = "journal_inspired", confidence = 0.70,
    source_version = "Editorial overlay, Vellum 0.3"
  )
}

.vellum_identities <- c(.vellum_identities, list(
  frontiers = .journal_overlay(
    "frontiers", "Frontiers", "#F5A623", "#17365D",
    c("#F5A623", "#17365D", "#2F80ED", "#27AE60", "#9B51E0", "#EB5757", "#56CCF2"),
    "https://www.frontiersin.org/guidelines/author-guidelines"
  ),
  springer = .journal_overlay(
    "springer", "Springer", "#003B5C", "#00A6D6",
    c("#003B5C", "#00A6D6", "#E87722", "#6A8EAE", "#2D7F5E", "#8C4B91", "#C6A15B"),
    "https://www.springernature.com/gp/authors/campaigns/writing-a-manuscript"
  ),
  elsevier = .journal_overlay(
    "elsevier", "Elsevier", "#E9711C", "#333333",
    c("#E9711C", "#333333", "#007398", "#6E8B3D", "#8E5EA2", "#C84C4C", "#D5A021"),
    "https://www.elsevier.com/en-gb/about/policies-and-standards/author/artwork-and-media-instructions"
  ),
  wiley = .journal_overlay(
    "wiley", "Wiley", "#6F2C91", "#008C95",
    c("#6F2C91", "#008C95", "#0057A3", "#D05A28", "#6A9F58", "#B23A70", "#C89B2B"),
    "https://authorservices.wiley.com/asset/photos/electronic_artwork_guidelines.pdf"
  ),
  oup = .journal_overlay(
    "oup", "Oxford University Press", "#002147", "#8A1538",
    c("#002147", "#8A1538", "#007398", "#B9975B", "#4C8C2B", "#6D4C7D", "#C04A35"),
    "https://academic.oup.com/pages/authoring/journals/preparing_your_manuscript"
  ),
  cambridge = .journal_overlay(
    "cambridge", "Cambridge University Press", "#0072CE", "#002147",
    c("#0072CE", "#002147", "#D1495B", "#2E8B57", "#8E5EA2", "#E39C29", "#4F7CAC"),
    "https://www.cambridge.org/core/services/authors/journals/journals-artwork-guide"
  ),
  mdpi = .journal_overlay(
    "mdpi", "MDPI", "#007C92", "#E87722",
    c("#007C92", "#E87722", "#4D8B31", "#7A5195", "#C94C4C", "#3A76AF", "#C6A028"),
    "https://www.mdpi.com/authors/layout"
  ),
  sage = .journal_overlay(
    "sage", "SAGE", "#231F20", "#F58220",
    c("#231F20", "#F58220", "#0076A8", "#6A8A3A", "#8A4F7D", "#C54C4C", "#D1A329"),
    "https://uk.sagepub.com/en-gb/eur/manuscript-submission-guidelines"
  ),
  taylor_francis = .journal_overlay(
    "taylor_francis", "Taylor & Francis", "#1E4D7B", "#E56A54",
    c("#1E4D7B", "#E56A54", "#2A9D8F", "#7A5195", "#D4A72C", "#4C78A8", "#A0513B"),
    "https://authorservices.taylorandfrancis.com/publishing-your-research/making-your-submission/formatting-and-templates/"
  )
))

.identity_metadata <- list(
  who = list(
    provenance = "official_public", confidence = 1,
    source_version = "WHO Data Design Language 0.9.2",
    layout = list(multilingual_space = "generous", hierarchy = "restrained blue"),
    legal = list(permission_note = "WHO name and emblem require separate permission.")
  ),
  world_bank = list(
    provenance = "official_public", confidence = 1,
    source_version = "World Bank Data Visualization Style Guide (draft) + WBG Visual Identity 2016",
    sources = c(
      "https://worldbank.github.io/data-visualization-style-guide/colors",
      "https://worldbank.github.io/data-visualization-style-guide/typography",
      "WBG Branding and Visual Identity Guidelines, February 2016, pp. 17-22"
    ),
    layout = list(density = "policy-analytical", baseline_grid = "strong",
                  methods_notes = "generous"),
    legal = list(permission_note = "World Bank Group trademarks and logos are disabled by default.")
  ),
  unicef = list(
    provenance = "official_public", confidence = 1,
    source_version = "UNICEF Brand Book v3.1",
    layout = list(character = "human-centred", callout_scale = "large")
  ),
  usaid = list(
    provenance = "official_public", confidence = 1,
    source_version = "USAID Branding and Marking Quick Reference, 2021",
    layout = list(character = "technical government report", metadata = "prominent")
  ),
  undp = list(
    provenance = "official_public", confidence = 0.96,
    source_version = "UNDP Data Visualization Library + Brand Manual 2020",
    layout = list(character = "development policy", multilingual_space = "generous")
  ),
  kpmg = list(
    provenance = "official_public", confidence = 0.94,
    source_version = "KPMG visual identity asset, 2020",
    layout = list(character = "consulting", alignment = "strong left",
                  divisions = "geometric")
  ),
  bdo = list(
    provenance = "private_inferred", confidence = 0.62,
    source_version = "Observed public outputs; current Brandfolder is private",
    legal = list(review_note = "Verify against current BDO Brandfolder assets before external use.")
  ),
  atni = list(
    provenance = "private_inferred", confidence = 0.72,
    source_version = "Observed ATNi publications and official website, 2026",
    legal = list(review_note = "Re-observe current ATNi outputs before a major external release.")
  ),
  unhcr = list(
    provenance = "official_public", confidence = 1,
    source_version = "UNHCR Data Visualization Guidelines, 2026"
  )
)

for (.nm in intersect(names(.identity_metadata), names(.vellum_identities))) {
  .vellum_identities[[.nm]] <- utils::modifyList(
    .vellum_identities[[.nm]], .identity_metadata[[.nm]]
  )
}
rm(.nm)

# The 2016 WBG manual governs institutional identity; the current World Bank
# data-visualisation guide governs analytical colour. Keep them separate.
.vellum_identities$world_bank$core[c("primary", "secondary")] <-
  c(primary = "#002244", secondary = "#009FDA")
.vellum_identities$world_bank$fonts$display <-
  c("Andes", "Source Sans 3", "Arial", "Open Sans", "Helvetica", "sans")
.vellum_identities$world_bank$fonts$body <-
  c("Open Sans", "Source Sans 3", "Arial", "Helvetica", "sans")
.vellum_identities$world_bank$fonts$mono <-
  c("Roboto Mono", "Courier", "Courier New", "mono")
.vellum_identities$world_bank$palettes$identity <- c(
  solid_blue = "#002244", bright_blue = "#009FDA", black = "#000000", white = "#FFFFFF"
)
.vellum_identities$world_bank$palettes$corporate <- c(
  solid_blue = "#002244", bright_blue = "#009FDA",
  warm_red = "#F05023", gold = "#FDB714", vivid_red = "#EB1C2D", orange = "#F78D28",
  cyan = "#009CA7", green = "#00AB51", purple = "#872B90", teal = "#00A996",
  muted_red = "#98252B", muted_orange = "#E16A2D", ochre = "#B88C1D",
  muted_purple = "#614776", dark_teal = "#006068", dark_green = "#006450"
)
.vellum_identities$world_bank$semantic[c("primary", "secondary", "information")] <-
  c(primary = "#002244", secondary = "#009FDA", information = "#106CA1")

.identity_user <- new.env(parent = emptyenv())

identity_key <- function(x) vellum_key(x)

identity_get <- function(identity, type = NULL) {
  check_string(identity, "identity")
  key <- identity_key(identity)
  known <- union(names(.vellum_identities), ls(.identity_user))
  if (!key %in% known) {
    cli::cli_abort(c(
      "Unknown Vellum identity {.val {identity}}.",
      i = "Use {.fn vellum_identities} to list available identities."
    ))
  }
  x <- if (key %in% ls(.identity_user)) get(key, envir = .identity_user) else
    .vellum_identities[[key]]
  if (!is.null(type) && !identical(x$type, type)) {
    cli::cli_abort("{.val {identity}} is a {x$type} identity, not a {type} identity.")
  }
  x
}

#' List institutional and journal identities
#'
#' @description
#' Returns Vellum's provenance-bearing identity registry. `official` and
#' `official-data-system` entries come from primary sources; `observed` and
#' `editorial-overlay` entries are deliberately labelled so they cannot be
#' mistaken for official brand standards.
#'
#' @param type Optional `"institution"` or `"journal"` filter.
#' @param status Optional status filter.
#' @return A data frame with one row per identity.
#' @export
vellum_identities <- function(type = NULL, status = NULL) {
  keys <- union(names(.vellum_identities), ls(.identity_user))
  specs <- lapply(keys, identity_get)
  out <- do.call(rbind, lapply(specs, function(x) data.frame(
    identity = x$name, label = x$label, type = x$type, status = x$status,
    provenance = x$provenance, confidence = x$confidence,
    source_version = x$source_version,
    venue = x$venue, source = x$source, palette_source = x$palette_source,
    font_source = x$font_source, retrieved = x$retrieved,
    note = x$note, stringsAsFactors = FALSE
  )))
  rownames(out) <- NULL
  if (!is.null(type)) {
    type <- match.arg(type, c("institution", "journal"))
    out <- out[out$type == type, , drop = FALSE]
  }
  if (!is.null(status)) out <- out[out$status %in% status, , drop = FALSE]
  out
}

#' Inspect one identity specification
#'
#' @param identity Identity name from [vellum_identities()].
#' @return A deep copy of the identity specification.
#' @export
identity_spec <- function(identity) identity_get(identity)

#' Retrieve an identity palette
#'
#' @param identity Identity name.
#' @param variant One of `"categorical"`, `"accessible"`, `"sequential"`,
#'   `"diverging"`, `"identity"`, `"corporate"`, `"status"`, `"geographic"`,
#'   `"semantic"` or `"core"`.
#' @param n Optional number of colours. Categorical palettes never recycle;
#'   sequential and diverging palettes interpolate when more colours are needed.
#' @param named Preserve colour names where present.
#' @param rev Reverse the colour order.
#' @return A character vector of hexadecimal colours.
#' @export
identity_palette <- function(identity, variant = c("categorical", "accessible",
                                                    "sequential", "diverging", "identity",
                                                    "corporate", "status", "geographic", "semantic", "core"),
                             n = NULL, named = TRUE, rev = FALSE) {
  variant <- match.arg(variant)
  check_bool(named, "named")
  check_bool(rev, "rev")
  spec <- identity_get(identity)
  cols <- if (variant == "core") spec$core else if (variant == "semantic") {
    spec$semantic
  } else spec$palettes[[variant]]
  if (is.null(cols)) {
    cli::cli_abort("Identity {.val {identity}} does not define a {.val {variant}} palette.")
  }
  if (!is.null(n)) {
    check_number(n, "n", min = 1)
    n <- as.integer(n)
    if (variant %in% c("categorical", "accessible", "identity", "corporate", "status",
                       "geographic", "semantic", "core")) {
      if (n > length(cols)) {
        cli::cli_abort(c(
          "Identity {.val {identity}} has {length(cols)} {variant} colours; {n} were requested.",
          i = "Vellum does not silently recycle identity colours."
        ))
      }
      cols <- cols[seq_len(n)]
    } else {
      cols <- grDevices::colorRampPalette(unname(cols), space = "Lab")(n)
    }
  }
  if (rev) cols <- rev(cols)
  if (!named) names(cols) <- NULL
  cols
}

identity_font_candidates <- function(spec, role) {
  spec$fonts[[role]] %||% spec$fonts$body
}

#' Resolve identity typography with an auditable fallback
#'
#' @param identity Identity name.
#' @param role `"body"`, `"display"`, `"accent"` or `"mono"`.
#' @param resolve If `FALSE`, return the full preference chain instead of the
#'   first installed family.
#' @return A character value. Resolved values carry `requested`, `substituted`
#'   and `status` attributes.
#' @export
identity_font <- function(identity, role = c("body", "display", "accent", "mono"),
                          resolve = TRUE) {
  role <- match.arg(role)
  check_bool(resolve, "resolve")
  spec <- identity_get(identity)
  candidates <- identity_font_candidates(spec, role)
  if (!resolve) return(candidates)
  available <- tryCatch(unique(systemfonts::system_fonts()$family),
                        error = function(e) character())
  generic <- c("sans", "serif", "mono")
  hit <- candidates[candidates %in% c(available, generic)]
  resolved <- if (length(hit)) hit[1] else if (role == "mono") "mono" else "sans"
  requested <- candidates[1]
  substituted <- !identical(resolved, requested)
  if (substituted) {
    inform_once(
      paste0("identity-font-", spec$name, "-", role),
      c("{spec$label}: {.val {requested}} is unavailable; using {.val {resolved}} for {role} text.",
        i = "The complete preference chain remains attached to the resolved value.")
    )
  }
  structure(resolved, requested = candidates,
            substituted = substituted, status = spec$status, role = role)
}

#' Identity-aware Vellum design tokens
#'
#' @inheritParams vellum_tokens
#' @param identity Identity name.
#' @param mode `"brand"` uses the identity's own categorical palette;
#'   `"accessible"` uses its audited/data-system palette where available.
#' @return A `vellum_tokens` object carrying identity provenance.
#' @export
identity_tokens <- function(identity, venue = NULL,
                            mode = c("brand", "accessible"), base_size = NULL,
                            family = NULL, density = c("comfortable", "compact", "airy"),
                            dark = FALSE) {
  mode <- match.arg(mode)
  density <- match.arg(density)
  spec <- identity_get(identity)
  venue <- venue %||% spec$venue
  body_system <- as.character(identity_font(identity, "body"))
  family <- family %||% device_safe_family(body_system)
  core <- spec$core
  tk <- vellum_tokens(
    venue = venue, base_size = base_size, family = family,
    ink = if (dark) NULL else unname(core[["ink"]]),
    accent = unname(core[["primary"]]), palette = "okabe_ito",
    density = density, dark = dark
  )
  if (!dark) {
    tk$ink$text <- unname(core[["ink"]])
    tk$ink$axis <- unname(core[["ink"]])
    tk$ink$tick <- unname(core[["ink"]])
    tk$ink$data <- unname(core[["ink"]])
    tk$ink$weak <- unname(core[["weak"]])
    tk$ink$paper <- unname(core[["paper"]])
    tk$ink$panel <- unname(core[["panel"]])
    tk$ink$grid <- unname(core[["grid"]])
    tk$ink$grid_minor <- grDevices::adjustcolor(core[["grid"]], alpha.f = 0.55)
    tk$ink$na <- unname(core[["na"]])
  }
  display_system <- as.character(identity_font(identity, "display"))
  accent_system <- as.character(identity_font(identity, "accent"))
  tk$type$family_display <- device_safe_family(display_system)
  tk$type$family_accent <- device_safe_family(accent_system)
  tk$meta$identity <- spec$name
  tk$meta$identity_label <- spec$label
  tk$meta$identity_type <- spec$type
  tk$meta$identity_status <- spec$status
  tk$meta$identity_provenance <- spec$provenance
  tk$meta$identity_confidence <- spec$confidence
  tk$meta$identity_source_version <- spec$source_version
  tk$meta$identity_source <- spec$source
  tk$meta$identity_palette_source <- spec$palette_source
  tk$meta$identity_font_source <- spec$font_source
  tk$meta$identity_mode <- mode
  tk$meta$identity_font_body <- body_system
  tk$meta$identity_font_display <- display_system
  tk
}

identity_palette_elements <- function(identity, mode) {
  discrete_variant <- if (mode == "accessible") "accessible" else "categorical"
  discrete <- function(n) identity_palette(identity, discrete_variant, n = n, named = FALSE)
  continuous <- identity_palette(identity, "sequential", n = 256, named = FALSE)
  list(
    palette.colour.discrete = discrete,
    palette.fill.discrete = discrete,
    palette.colour.continuous = continuous,
    palette.fill.continuous = continuous
  )
}

#' Apply an institutional or journal identity
#'
#' @description
#' Applies colour and typography without changing the scientific contract.
#' Institution themes default to their brand/data-system palette. Journal themes
#' should normally use `mode = "accessible"`; editorial palettes are visual
#' overlays, not publisher requirements.
#'
#' @param identity Identity name.
#' @param venue Optional venue override.
#' @param profile Optional Vellum design profile controlling hierarchy.
#' @param mode `"brand"` or `"accessible"`.
#' @param base_size,family,dark Passed to [identity_tokens()].
#' @param grid,axis_style,ticks,legend,density Theme controls; omitted values are
#'   supplied by `profile` or a restrained default.
#' @param ... Additional [ggplot2::theme()] elements.
#' @return A complete ggplot2 theme carrying identity metadata.
#' @export
theme_identity <- function(identity, venue = NULL, profile = NULL,
                           mode = c("brand", "accessible"), base_size = NULL,
                           family = NULL, dark = FALSE, grid = NULL,
                           axis_style = NULL, ticks = NULL, legend = NULL,
                           density = NULL, ...) {
  mode <- match.arg(mode)
  spec <- identity_get(identity)
  cfg <- if (is.null(profile)) .vellum_profiles$restrained else {
    check_string(profile, "profile")
    key <- identity_key(profile)
    if (!key %in% names(.vellum_profiles)) {
      cli::cli_abort("Unknown design profile {.val {profile}}.")
    }
    .vellum_profiles[[key]]
  }
  density <- density %||% cfg$density
  tk <- identity_tokens(spec$name, venue = venue, mode = mode,
                        base_size = base_size, family = family,
                        density = density, dark = dark)
  th <- theme_vellum(
    tokens = tk, grid = grid %||% cfg$grid,
    axis_style = axis_style %||% cfg$axis_style,
    ticks = ticks %||% "out", legend = legend %||% cfg$legend
  )
  th <- th + do.call(ggplot2::theme, identity_palette_elements(spec$name, mode))
  th <- th + ggplot2::theme(
    plot.title = ggplot2::element_text(family = tk$type$family_display),
    strip.text = ggplot2::element_text(family = tk$type$family_display),
    legend.title = ggplot2::element_text(family = tk$type$family_display)
  )
  extra <- list(...)
  if (length(extra)) th <- th + do.call(ggplot2::theme, extra)
  attr(th, "vellum_tokens") <- tk
  attr(th, "vellum_identity") <- list(
    identity = spec$name, label = spec$label, type = spec$type,
    status = spec$status, provenance = spec$provenance,
    confidence = spec$confidence, source_version = spec$source_version,
    source = spec$source, mode = mode
  )
  th
}

#' Institutional Vellum theme
#'
#' @inheritParams theme_identity
#' @return A complete ggplot2 theme.
#' @export
theme_institution <- function(identity, venue = "portfolio", mode = c("brand", "accessible"), ...) {
  identity_get(identity, "institution")
  theme_identity(identity, venue = venue, mode = match.arg(mode), ...)
}

#' Scientific-journal Vellum theme
#'
#' @description
#' Defaults to accessible scientific colour. Set `mode = "editorial"` for the
#' optional publisher-inspired overlay; this does not claim the palette is an
#' official author requirement.
#'
#' @param journal Journal identity name.
#' @param mode `"accessible"` (default) or `"editorial"`.
#' @param venue Optional venue override. The journal's venue contract is the
#'   default; `"portfolio"` is useful for large report/showcase panels.
#' @param ... Passed to [theme_identity()].
#' @return A complete ggplot2 theme.
#' @export
theme_journal <- function(journal, mode = c("accessible", "editorial"),
                          venue = NULL, ...) {
  mode <- match.arg(mode)
  spec <- identity_get(journal, "journal")
  theme_identity(spec$name, venue = venue %||% spec$venue,
                 mode = if (mode == "editorial") "brand" else "accessible", ...)
}

brand_discrete_pal <- function(identity, variant, rev) {
  force(identity); force(variant); force(rev)
  function(n) identity_palette(identity, variant, n = n, named = FALSE, rev = rev)
}

#' Discrete identity colour and fill scales
#'
#' @param identity Identity name.
#' @param variant `"categorical"` or `"accessible"`.
#' @param rev Reverse the palette.
#' @param na.value Missing-value colour.
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @param aesthetics Aesthetic name.
#' @return A ggplot2 scale.
#' @export
scale_colour_brand <- function(identity, variant = c("categorical", "accessible"),
                               rev = FALSE, na.value = NULL, ...,
                               aesthetics = "colour") {
  variant <- match.arg(variant)
  spec <- identity_get(identity)
  ggplot2::discrete_scale(
    aesthetics, palette = brand_discrete_pal(spec$name, variant, rev),
    na.value = na.value %||% unname(spec$core[["na"]]), ...
  )
}

#' @rdname scale_colour_brand
#' @export
scale_color_brand <- scale_colour_brand

#' @rdname scale_colour_brand
#' @export
scale_fill_brand <- function(identity, variant = c("categorical", "accessible"),
                             rev = FALSE, na.value = NULL, ...,
                             aesthetics = "fill") {
  scale_colour_brand(identity, match.arg(variant), rev, na.value, ...,
                     aesthetics = aesthetics)
}

#' Continuous identity colour and fill scales
#'
#' @param identity Identity name.
#' @param variant `"sequential"` or `"diverging"`.
#' @param rev Reverse the palette.
#' @param na.value Missing-value colour.
#' @param n Ramp resolution.
#' @param guide Guide name.
#' @param ... Passed to [ggplot2::continuous_scale()].
#' @param aesthetics Aesthetic name.
#' @return A ggplot2 scale.
#' @export
scale_colour_brand_c <- function(identity, variant = c("sequential", "diverging"),
                                 rev = FALSE, na.value = NULL, n = 256,
                                 guide = "colourbar", ...,
                                 aesthetics = "colour") {
  variant <- match.arg(variant)
  spec <- identity_get(identity)
  cols <- identity_palette(spec$name, variant, n = n, named = FALSE, rev = rev)
  ggplot2::continuous_scale(
    aesthetics, palette = scales::pal_gradient_n(cols),
    na.value = na.value %||% unname(spec$core[["na"]]), guide = guide, ...
  )
}

#' @rdname scale_colour_brand_c
#' @export
scale_color_brand_c <- scale_colour_brand_c

#' @rdname scale_colour_brand_c
#' @export
scale_fill_brand_c <- function(identity, variant = c("sequential", "diverging"),
                               rev = FALSE, na.value = NULL, n = 256,
                               guide = "colourbar", ..., aesthetics = "fill") {
  scale_colour_brand_c(identity, match.arg(variant), rev, na.value, n, guide,
                       ..., aesthetics = aesthetics)
}

#' Audit an identity's palette, typography and provenance
#'
#' @param identity Identity name.
#' @param variant Palette variant to audit.
#' @return A list of class `vellum_identity_audit`.
#' @export
identity_audit <- function(identity, variant = c("categorical", "accessible")) {
  variant <- match.arg(variant)
  spec <- identity_get(identity)
  cols <- identity_palette(spec$name, variant, named = FALSE)
  structure(list(
    identity = spec$name, label = spec$label, type = spec$type,
    status = spec$status, provenance = spec$provenance,
    confidence = spec$confidence, source_version = spec$source_version,
    source = spec$source,
    palette_source = spec$palette_source, font_source = spec$font_source,
    retrieved = spec$retrieved,
    variant = variant, palette = palette_audit(cols, type = "qualitative"),
    text_contrast = contrast_ratio(spec$core[["ink"]], spec$core[["paper"]]),
    body_requested = identity_font(spec$name, "body", resolve = FALSE)[1],
    body_resolved = as.character(identity_font(spec$name, "body")),
    display_requested = identity_font(spec$name, "display", resolve = FALSE)[1],
    display_resolved = as.character(identity_font(spec$name, "display")),
    note = spec$note
  ), class = "vellum_identity_audit")
}

#' @export
print.vellum_identity_audit <- function(x, ...) {
  cli::cli_h1("vellum identity audit \u00b7 {x$label}")
  cli::cli_text("{.strong Provenance} {x$provenance} \u00b7 confidence {sprintf('%.2f', x$confidence)}")
  cli::cli_text("{.strong Source version} {x$source_version}")
  cli::cli_text("{.strong Palette} {x$variant} \u00b7 {length(x$palette$colours)} colours")
  cli::cli_text("{.strong Text contrast} {sprintf('%.2f', x$text_contrast)}:1")
  cli::cli_text("{.strong Body} {x$body_requested} \u2192 {x$body_resolved}")
  cli::cli_text("{.strong Display} {x$display_requested} \u2192 {x$display_resolved}")
  cli::cli_text("{.strong Palette source} {x$palette_source}")
  cli::cli_text("{.strong Font source} {x$font_source}")
  cli::cli_text("{x$note}")
  invisible(x)
}
