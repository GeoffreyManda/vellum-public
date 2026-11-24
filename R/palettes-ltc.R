# The ltc collection --------------------------------------------------------
#
# Loukas Theodosiou's `ltc` package: colour palettes drawn from paintings,
# places, cats and Booker Prize novels, each with a backstory. MIT licensed;
# see LICENSE.note. Values transcribed exactly from
# https://github.com/loukesio/ltc-color-palettes (R/ltc_functions.R), retrieved
# 2026-08-05 at version 0.4.0.9000.
#
# This is a fifth palette source and it sits BELOW the other four for
# scientific work, for a reason worth stating rather than implying: these
# palettes were chosen to be beautiful, not to be separable. Eleven of the
# twenty-eight qualitative ones fall below Vellum's dichromatic floor of 8
# CIEDE2000, and several survive only two or three of their colours. That is
# not a defect in `ltc` -- an art palette is not making an accessibility claim
# -- but it becomes one the moment the palette encodes a variable.
#
# So every entry carries its own measurement. `cvd` is the worst-case minimum
# pairwise CIEDE2000 across simulated deuteranopia, protanopia and tritanopia
# (for the diverging ramps, the worst-case ARM separation, which is what lets a
# reader recover the sign of a deviation). `safe_n` is the largest leading
# prefix that still clears `palette_max_safe()`'s default threshold of 10.
# Both appear in the `vellum_palettes()` rationale, so the catalogue states the
# cost before the figure does. Reproduce any of them with `palette_audit()`.
#
# Three entries are not original to `ltc` and are attributed here rather than
# passed along: `heatmap2` and `heatmap3` are Cynthia Brewer's ColorBrewer
# RdBu-5 and RdYlBu-5 exactly, and `trio3` is the first three Okabe-Ito
# colours.

.ltc <- list(
  # Picasso's circle: the women, the daughters, the cat, the paintings.
  paloma = list(type = "qual", cvd = 4.4, safe_n = 1L,
                cols = c("#83AF9B","#C8C8A9","#F8DA8A","#F7BF95","#FE8CA1"),
                bio = "Paloma Picasso, daughter of Francoise Gilot and Pablo Picasso"),
  maya = list(type = "qual", cvd = 14.8, safe_n = 5L,
              cols = c("#3D5A80","#98C1D9","#E0FBFC","#EE6C4D","#293241"),
              bio = "Maya Widmaier-Picasso, daughter of Marie-Therese Walter and Pablo Picasso"),
  dora = list(type = "qual", cvd = 10.5, safe_n = 5L,
              cols = c("#52777A","#542437","#C02942","#D95B43","#ECD078"),
              bio = "Dora Maar, French photographer, painter and poet"),
  ploen = list(type = "qual", cvd = 9.9, safe_n = 2L,
               cols = c("#3F5671","#83A1C3","#CEB5C8","#FAC898","#B17776"),
               bio = "Ploen, a village in Schleswig-Holstein, northern Germany"),
  olga = list(type = "qual", cvd = 4.4, safe_n = 3L,
              cols = c("#C9E3C2","#8BC8CB","#ECCD80","#F5AB70","#9C87A1"),
              bio = "Olga Khokhlova, Russian ballet dancer"),
  mterese = list(type = "qual", cvd = 5.6, safe_n = 1L,
                 cols = c("#F7DDAA","#FAC3AD","#F897A1","#9298BA","#9CBEED"),
                 bio = "Marie-Therese Walter, French model and mother of Maya"),
  gaby = list(type = "qual", cvd = 10.0, safe_n = 3L,
              cols = c("#FCEAAB","#F1A890","#A8C4CC","#82A0C2","#85496F"),
              bio = "Gabrielle Depeyre Lespinasse, French dancer"),
  franscoise = list(type = "qual", cvd = 9.3, safe_n = 1L,
                    cols = c("#5980B1","#B96A8D","#A55062","#E05256","#E9A986"),
                    bio = "Francoise Gilot, French painter"),
  fernande = list(type = "qual", cvd = 7.4, safe_n = 2L,
                  cols = c("#FF7676","#F9D662","#7CAB7D","#75B7D1"),
                  bio = "Fernande Olivier, French model and artist"),
  sylvie = list(type = "qual", cvd = 2.3, safe_n = 3L,
                cols = c("#E8B961","#E88170","#C6BDE8","#5DB7C4","#FD95BC"),
                bio = "Sylvette David, French artist and model"),
  minou = list(type = "qual", cvd = 11.1, safe_n = 6L,
               cols = c("#00798C","#D1495B","#EDAE49","#66A182","#2E4057","#8D96A3"),
               bio = "Minou, Picasso's cat"),
  kiss = list(type = "qual", cvd = 15.9, safe_n = 5L,
              cols = c("#FF7C7E","#FEC300","#9E3F71","#31BCBA","#E20035"),
              bio = "The Kiss, Picasso 1925"),
  hat = list(type = "qual", cvd = 4.6, safe_n = 1L,
             cols = c("#EFB306","#EB990C","#E8351E","#CD023D","#852F88",
                      "#4E54AC","#0F8096","#7DB954","#17A769","#000000"),
             bio = "Woman in Hat, Picasso 1937"),
  reading = list(type = "qual", cvd = 4.1, safe_n = 2L,
                 cols = c("#EFBC68","#919F89","#EDBDAE","#57717C","#5F97A4",
                          "#CAEAC8","#95A1AE","#C8CFD6"),
                 bio = "Two Girls Reading, Picasso 1934"),
  alger = list(type = "qual", cvd = 20.0, safe_n = 5L,
               cols = c("#000000","#1A5B5B","#ACC8BE","#F4AB5C","#D1422F"),
               bio = "Les Femmes d'Alger, Picasso 1955"),
  casa_natal = list(type = "qual", cvd = 6.7, safe_n = 5L,
                    cols = c("#245E55","#ED773C","#808BC5","#C63F3E","#EAC119",
                             "#EAA7C7","#9ED6DF","#1D1D1B","#EAE4DA"),
                    bio = "Casa Natal on the Plaza de la Merced, Picasso's birthplace"),

  # Working palettes and three-colour sets.
  expevo = list(type = "qual", cvd = 11.2, safe_n = 6L,
                cols = c("#FC4E07","#E7B800","#00AFBB","#8B4769","#1D457F","#808080"),
                bio = "An experimental-evolution palette in common use among biologists"),
  trio1 = list(type = "qual", cvd = 15.1, safe_n = 3L,
               cols = c("#0E7175","#FD7901","#C35BCA"),
               bio = "A three-colour set: teal, orange, orchid"),
  trio2 = list(type = "qual", cvd = 10.6, safe_n = 3L,
               cols = c("#89973D","#E8B92F","#A45E41"),
               bio = "A three-colour set: olive, gold, terracotta"),
  trio3 = list(type = "qual", cvd = 12.2, safe_n = 3L,
               cols = c("#E69F00","#56B4E9","#009E73"),
               bio = "A three-colour set; these are Okabe-Ito's first three colours",
               note = "Identical to okabe_ito(3); prefer that name, which says where they came from"),
  trio4 = list(type = "qual", cvd = 4.3, safe_n = 1L,
               cols = c("#94475E","#364C54","#E5A11F"),
               bio = "A three-colour set: wine, slate, amber"),
  pantone23 = list(type = "qual", cvd = 3.8, safe_n = 3L,
                   cols = c("#7A92A5","#1F2C43","#FFB000","#842C48","#46483D"),
                   bio = "Soft Chaos, released by Pantone in summer 2023"),

  # Booker Prize novels and one maritime set.
  remains = list(type = "qual", cvd = 13.1, safe_n = 4L,
                 cols = c("#69326E","#EEEDC0","#FF6D1F","#EED455"),
                 bio = "The Remains of the Day, Kazuo Ishiguro (Booker Prize 1989)"),
  midnight = list(type = "qual", cvd = 12.2, safe_n = 4L,
                  cols = c("#16232A","#FF5B04","#075056","#E4EEF0"),
                  bio = "Midnight's Children, Salman Rushdie (Booker Prize 1981)"),
  lincoln = list(type = "qual", cvd = 6.9, safe_n = 1L,
                 cols = c("#EEE9DF","#C9C1B1","#2C3B4D","#FFB162","#A35139","#1B2632"),
                 bio = "Lincoln in the Bardo, George Saunders (Booker Prize 2017)"),
  luminaries = list(type = "qual", cvd = 8.2, safe_n = 2L,
                    cols = c("#FF5B04","#075056","#233038","#FDF6E3","#F4D47C","#D3DBDD"),
                    bio = "The Luminaries, Eleanor Catton (Booker Prize 2013)"),
  shuggie = list(type = "qual", cvd = 13.0, safe_n = 5L,
                 cols = c("#5B5F8D","#9BB29E","#DA6B51","#F1DCBA","#484149"),
                 bio = "Shuggie Bain, Douglas Stuart (Booker Prize 2020)"),
  seafarer = list(type = "qual", cvd = 10.0, safe_n = 5L,
                  cols = c("#013D5A","#FCF3E3","#BDD3CE","#708C69","#E4A25B"),
                  bio = "A maritime set, after The Old Man and the Sea"),

  # The four heatmap ramps. These are the only ltc palettes that are ordered,
  # and they are typed here rather than inferred: `infer_palette_type()` reads
  # heatmap0 as qualitative because its arms are not monotone within tolerance,
  # and reads three of the art palettes as diverging because their lightest
  # colour happens to sit in the middle. A declared type is the only honest one.
  heatmap0 = list(type = "div", cvd = 23.5, safe_n = NA_integer_,
                  cols = c("#001219","#005F73","#0A9396","#94D2BD","#E9D8A6",
                           "#EE9B00","#CA6702","#AE2012","#9B2226"),
                  bio = "A nine-step diverging ramp for heatmaps: deep teal through cream to oxblood"),
  heatmap1 = list(type = "div", cvd = 16.7, safe_n = NA_integer_,
                  cols = c("#4D7799","#7FA4C4","#C5C8D4","#D48E95","#B5515B"),
                  bio = "A muted blue-to-red diverging ramp",
                  note = "Lightness range 33 L*, below the 40 that survives greyscale; label the arms"),
  heatmap2 = list(type = "div", cvd = 30.4, safe_n = NA_integer_,
                  cols = c("#CA0020","#F4A582","#F7F7F7","#92C5DE","#0571B0"),
                  bio = "A red-to-blue diverging ramp",
                  note = "This is ColorBrewer RdBu 5-class exactly (Brewer, Harrower & Penn State)"),
  heatmap3 = list(type = "div", cvd = 34.9, safe_n = NA_integer_,
                  cols = c("#D7191C","#FDAE61","#FFFFBF","#ABD9E9","#2C7BB6"),
                  bio = "A red-yellow-blue diverging ramp",
                  note = "This is ColorBrewer RdYlBu 5-class exactly (Brewer, Harrower & Penn State)")
)

# The catalogue line for an ltc palette: the backstory, then the measurement,
# then any correction to its provenance. The measurement is stated in the same
# breath as the story deliberately -- someone reaching for `sylvie` because of
# what it is should see 2.3 without having to go and look.
ltc_rationale <- function(nm) {
  x <- .ltc[[nm]]
  stop_full <- function(s) ifelse(grepl("[.]$", s), s, paste0(s, "."))
  parts <- x$bio
  parts <- c(parts, if (identical(x$type, "div")) {
    sprintf("Worst-case arm separation under dichromacy %.1f dE00.", x$cvd)
  } else if (x$cvd < .audit_thresholds$qual_cvd_pass) {
    sprintf(paste("Worst-case dichromatic separation %.1f dE00, BELOW vellum's",
                  "floor of %g; %s"),
            x$cvd, .audit_thresholds$qual_cvd_pass,
            if (x$safe_n < 2L) "no two of these colours are safely distinct."
            else sprintf("only the first %d are safely distinct.", x$safe_n))
  } else {
    sprintf("Worst-case dichromatic separation %.1f dE00; %s a threshold of 10.",
            x$cvd,
            if (x$safe_n == length(x$cols)) sprintf("all %d clear", x$safe_n)
            else if (x$safe_n == 1L) "only the first colour clears"
            else sprintf("only the first %d clear", x$safe_n))
  })
  paste(stop_full(c(parts, x$note)), collapse = " ")
}
