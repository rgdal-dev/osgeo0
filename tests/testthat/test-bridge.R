test_that(".ogr_layer_to_wk_df handles empty layers", {
  skip_if_no_gnm()

  ogr <- reticulate::import("osgeo.ogr")
  osr <- reticulate::import("osgeo.osr")

  sr <- osr$SpatialReference()
  sr$SetFromUserInput("EPSG:4326")

  drv <- ogr$GetDriverByName("Memory")
  ds <- drv$CreateDataSource("")
  lyr <- ds$CreateLayer("empty", sr, ogr$wkbPoint)

  df <- .ogr_layer_to_wk_df(lyr, crs = "EPSG:4326")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_true("geom" %in% names(df))
  expect_s3_class(df$geom, "wk_wkb")
})

test_that(".ogr_layer_to_wk_df preserves attributes and geometry", {
  skip_if_no_gnm()

  ogr <- reticulate::import("osgeo.ogr")
  osr <- reticulate::import("osgeo.osr")

  sr <- osr$SpatialReference()
  sr$SetFromUserInput("EPSG:4326")

  drv <- ogr$GetDriverByName("Memory")
  ds <- drv$CreateDataSource("")
  lyr <- ds$CreateLayer("test", sr, ogr$wkbPoint)
  lyr$CreateField(ogr$FieldDefn("name", ogr$OFTString))
  lyr$CreateField(ogr$FieldDefn("value", ogr$OFTReal))

  feat <- ogr$Feature(lyr$GetLayerDefn())
  feat$SetField("name", "hello")
  feat$SetField("value", 42.5)
  pt <- ogr$Geometry(ogr$wkbPoint)
  pt$AddPoint(147.3, -42.9)
  feat$SetGeometry(pt)
  lyr$CreateFeature(feat)

  df <- .ogr_layer_to_wk_df(lyr, crs = "EPSG:4326")

  expect_equal(nrow(df), 1L)
  expect_equal(df$name, "hello")
  expect_equal(df$value, 42.5)
  expect_s3_class(df$geom, "wk_wkb")
  expect_equal(wk::wk_crs(df$geom), "EPSG:4326")
})

test_that(".ogr_layer_to_wk_df adds role annotations", {
  skip_if_no_gnm()

  ogr <- reticulate::import("osgeo.ogr")
  osr <- reticulate::import("osgeo.osr")

  sr <- osr$SpatialReference()
  sr$SetFromUserInput("EPSG:4326")

  drv <- ogr$GetDriverByName("Memory")
  ds <- drv$CreateDataSource("")

  # Point layer -> node
  pt_lyr <- ds$CreateLayer("pts", sr, ogr$wkbPoint)
  feat <- ogr$Feature(pt_lyr$GetLayerDefn())
  pt <- ogr$Geometry(ogr$wkbPoint)
  pt$AddPoint(0.0, 0.0)
  feat$SetGeometry(pt)
  pt_lyr$CreateFeature(feat)

  df_pt <- .ogr_layer_to_wk_df(pt_lyr, include_role = TRUE)
  expect_equal(df_pt$.role, "node")

  # Line layer -> edge
  ln_lyr <- ds$CreateLayer("lns", sr, ogr$wkbLineString)
  feat <- ogr$Feature(ln_lyr$GetLayerDefn())
  line <- ogr$Geometry(ogr$wkbLineString)
  line$AddPoint(0.0, 0.0)
  line$AddPoint(1.0, 1.0)
  feat$SetGeometry(line)
  ln_lyr$CreateFeature(feat)

  df_ln <- .ogr_layer_to_wk_df(ln_lyr, include_role = TRUE)
  expect_equal(df_ln$.role, "edge")
})

test_that(".wk_df_to_ogr round-trips through temp GeoPackage", {
  skip_if_no_gnm()

  pts <- wk::wkb(list(
    wk::as_wkb(wk::wkt("POINT (147.3 -42.9)")),
    wk::as_wkb(wk::wkt("POINT (147.4 -42.8)"))
  ))

  df <- data.frame(label = c("hobart", "nearby"), value = c(1L, 2L))
  df$geom <- pts

  result <- .wk_df_to_ogr(df, name = "test", srs = "EPSG:4326")
  on.exit({
    result$py_ds <- NULL
    unlink(result$tmp_path, force = TRUE)
  })

  expect_true(file.exists(result$tmp_path))
  expect_equal(result$layer_name, "test")

  # Read it back via the bridge
  lyr <- result$py_ds$GetLayerByName("test")
  df2 <- .ogr_layer_to_wk_df(lyr, crs = "EPSG:4326")

  expect_equal(nrow(df2), 2L)
  expect_true("label" %in% names(df2))
  expect_true("value" %in% names(df2))
})
