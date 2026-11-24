test_that("spatial recipes build projected, layered ggplots", {
  skip_if_not_installed("sf")
  invisible(suppressWarnings(vellum_tokens("nature")))
  nc <- suppressWarnings(sf::st_read(
    system.file("shape/nc.shp", package = "sf"), quiet = TRUE
  ))
  nc$rate <- nc$SID79 / nc$BIR79 * 1000

  choropleth <- suppressWarnings(fig_choropleth(nc, "rate", crs = 5070))
  expect_s3_class(choropleth, "ggplot")
  expect_equal(length(choropleth$layers), 1)

  repeated <- rbind(
    transform(nc, period = "1974", rate = SID74 / BIR74 * 1000),
    transform(nc, period = "1979", rate = SID79 / BIR79 * 1000)
  )
  temporal <- fig_spatiotemporal_map(
    repeated, "period", "rate", crs = 5070, columns = 2
  )
  expect_s3_class(temporal, "ggplot")
  expect_s3_class(temporal$facet, "FacetWrap")

  symbols <- fig_spatial_symbols(
    nc, "BIR79", crs = 5070, background = nc
  )
  expect_s3_class(symbols, "ggplot")
  expect_equal(length(symbols$layers), 2)

  flows <- data.frame(
    from_lon = c(-84, -82, -80),
    from_lat = c(35.5, 35.7, 35.2),
    to_lon = c(-78.6, -78.6, -78.6),
    to_lat = c(35.8, 35.8, 35.8),
    people = c(20, 55, 90)
  )
  flow <- fig_flow_map(
    nc, flows, "from_lon", "from_lat", "to_lon", "to_lat", "people",
    crs = 5070
  )
  expect_s3_class(flow, "ggplot")
  expect_equal(length(flow$layers), 3)
})

test_that("spatial catalogue entries route to implemented recipes", {
  spatial <- chart_catalogue(c("Spatial", "Spatiotemporal"))
  expect_true(all(c(
    "fig_choropleth", "fig_spatial_symbols", "fig_flow_map",
    "fig_spatiotemporal_map"
  ) %in% spatial$vellum_recipe))
  expect_gte(nrow(chart_catalogue()), 76)
})

test_that("spatial recipes reject malformed inputs", {
  skip_if_not_installed("sf")
  expect_error(fig_choropleth(data.frame(x = 1), "x"), "sf")
  nc <- suppressWarnings(sf::st_read(
    system.file("shape/nc.shp", package = "sf"), quiet = TRUE
  ))
  expect_error(fig_choropleth(nc, "NAME"), "numeric")
  expect_error(
    fig_flow_map(
      nc, data.frame(x = 1), "a", "b", "c", "d", "w", crs = 5070
    ),
    "missing"
  )
})
