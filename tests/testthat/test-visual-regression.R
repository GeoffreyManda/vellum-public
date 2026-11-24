test_that("core consultancy figures have stable visual structure", {
  skip_if_not_installed("vdiffr")

  interval_data <- data.frame(
    term = c("A", "B", "C"),
    estimate = c(.7, 1.0, 1.3),
    lower = c(.5, .8, 1.0),
    upper = c(.9, 1.2, 1.6)
  )
  vdiffr::expect_doppelganger(
    "estimate interval",
    fig_interval(interval_data, "estimate", "lower", "upper", "term", null = 1)
  )
  vdiffr::expect_doppelganger("chart taxonomy", fig_chart_taxonomy("portfolio"))
})
