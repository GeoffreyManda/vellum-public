library(vellum)

# Synthetic risk ratios and confidence intervals, supplied by the analyst.
estimates <- data.frame(
  group = c("Group A", "Group B", "Group C"),
  ratio = c(0.8, 1.0, 1.2),
  lower = c(0.6, 0.8, 0.9),
  upper = c(1.0, 1.2, 1.5)
)
p <- fig_interval(estimates, "ratio", "lower", "upper", "group", null = 1)
print(p)

# Export to a disposable file; use a permanent path for your own figure.
local({
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)
  ggplot2::ggsave(path, p, width = 6, height = 4, units = "in")
  stopifnot(file.exists(path), file.info(path)$size > 1000)
  cat("Interval figure exported successfully.\n")
})
