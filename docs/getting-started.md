# Scientific figures and plotting recipes

Use `vellum` for scientific plot themes, palettes, diagrams and figure recipes. Statistical estimates and intervals are supplied by the analyst. The public repository contains package code and synthetic test fixtures.

## Requirements

R 4.1 or later and the runtime dependencies in `DESCRIPTION`, including ggplot2. Raster and SVG export can use the optional `ragg` and `svglite` packages. Available fonts depend on the operating system; package fallbacks apply when a requested font is unavailable.

## Worked interval figure

The example supplies three synthetic ratios with lower and upper interval bounds. `fig_interval()` draws each estimate and its interval; `null = 1` adds the comparison line for a ratio measure.

```r
library(vellum)
estimates <- data.frame(
  group = c("A", "B", "C"),
  ratio = c(0.8, 1.0, 1.2),
  lower = c(0.6, 0.8, 0.9),
  upper = c(1.0, 1.2, 1.5)
)
p <- fig_interval(estimates, "ratio", "lower", "upper", "group", null = 1)
print(p)
ggplot2::ggsave("intervals.pdf", p, width = 6, height = 4, units = "in")
```

Use the appropriate null value for the estimand: ratios commonly use 1; differences commonly use 0. Ensure that the point estimates and interval bounds share the same scale. The recipe draws supplied values and does not estimate statistical uncertainty.

## Export and reuse

The result is a ggplot object, so labels and other ggplot layers can be added before export. Choose width, height and units for the intended document. The installed worked example writes a temporary PDF, checks its existence and size, and removes it after the check. Change the output path when saving your own result.

Palette, venue and institutional presets are available through the installed help. The original third-party palette credits are retained in `LICENSE.note`. Presets do not grant permission to use proprietary fonts or institutional marks.

## Troubleshooting

A missing font warning may change label widths and figure layout. Inspect the exported figure on the system used for publication. When a graphics device is unavailable, install its optional package or export to the base PDF device as in the example. Keep the plot dimensions explicit when comparing outputs.

## Run the complete installed example

```r
path <- system.file("examples", "worked-example.R", package = "vellum")
stopifnot(nzchar(path))
source(path)
```

## Development checks

GitHub Actions runs on pushes, pull requests and manual dispatch. It installs runtime and test dependencies, installs the package with tests, runs `testthat::test_package()` and executes the installed example. Failed steps fail the job. Optional capability skips remain visible in the log. This workflow covers Linux with the current R release; it does not establish cross-platform package acceptance.

Report an issue with the smallest synthetic example, your R version and the complete error message. Exclude private records and credentials.
