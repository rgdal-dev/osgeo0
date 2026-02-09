test_that("gnm_import_path imports shapefiles into network", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)

  net_path <- file.path(dir, "water_net")
  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")

  layers <- gnm_layers(net)
  expect_true("wells" %in% layers)
  expect_true("pipes" %in% layers)
})

test_that("gnm_layer returns a data.frame with wk geometry", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)

  net_path <- file.path(dir, "water_net")
  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")

  wells <- gnm_layer(net, "wells")

  expect_s3_class(wells, "data.frame")
  expect_true(".gfid" %in% names(wells))
  expect_true("geom" %in% names(wells))
  expect_s3_class(wells$geom, "wk_wkb")
  expect_equal(nrow(wells), 4L)
})

test_that("gnm_layer has correct attributes from source data", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)

  net_path <- file.path(dir, "water_net")
  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$pipes, name = "pipes")
  pipes <- gnm_layer(net, "pipes")

  expect_true("name" %in% names(pipes))
  expect_true("diameter" %in% names(pipes))
  expect_equal(nrow(pipes), 3L)
})

test_that("gnm_layer errors for missing layer", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")
  on.exit(gnm_close(net), add = TRUE)

  expect_error(gnm_layer(net, "nonexistent"), "not found")
})

test_that("gnm_import works with wk data.frames", {
  skip_if_no_gnm()
  skip_if_not_installed("wk")

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  # Create a simple wk data.frame
  pts <- wk::wkb(list(
    wk::as_wkb(wk::wkt("POINT (0 0)")),
    wk::as_wkb(wk::wkt("POINT (1 1)"))
  ))

  df <- data.frame(name = c("a", "b"))
  df$geom <- pts

  gnm_import(net, df, "test_points")

  layers <- gnm_layers(net)
  expect_true("test_points" %in% layers)
})

test_that("gnm_layers excludes system layers", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")
  on.exit(gnm_close(net), add = TRUE)

  layers <- gnm_layers(net)
  expect_false(any(grepl("^_gnm_", layers)))
})
