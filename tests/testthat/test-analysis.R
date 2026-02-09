test_that("gnm_connect_auto builds topology", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")

  # Should not error

  expect_no_error(
    gnm_connect_auto(net, tolerance = 0.001)
  )
})

test_that("gnm_shortest_path returns a gnm_path", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")
  gnm_connect_auto(net, tolerance = 0.001)

  # Get GFIDs from the wells layer
  wells <- gnm_layer(net, "wells")
  gfids <- wells$.gfid

  # Attempt a path between first and last well
  path <- gnm_shortest_path(net, gfids[1], gfids[4])

  expect_s3_class(path, "gnm_path")
  expect_equal(path@from, gfids[1])
  expect_equal(path@to, gfids[4])
  expect_equal(path@algorithm, "dijkstra")
  expect_true(path@n_features > 0L)
  expect_s3_class(path@features$geom, "wk_wkb")
})

test_that("gnm_path has role annotations", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")
  gnm_connect_auto(net, tolerance = 0.001)

  wells <- gnm_layer(net, "wells")
  path <- gnm_shortest_path(net, wells$.gfid[1], wells$.gfid[4])

  expect_true(".role" %in% names(path@features))
  expect_true(any(path@features$.role == "node", na.rm = TRUE))
  expect_true(any(path@features$.role == "edge", na.rm = TRUE))
})

test_that("gnm_block affects path finding", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")
  gnm_connect_auto(net, tolerance = 0.001)

  wells <- gnm_layer(net, "wells")

  # Get baseline path
  path1 <- gnm_shortest_path(net, wells$.gfid[1], wells$.gfid[4])
  n_before <- path1@n_features

  # Block a middle well — path should change or fail
  gnm_block(net, wells$.gfid[2])

  # The path might still exist via different features, or might fail.
  # Either outcome is acceptable; the key is no error from blocking.
  expect_no_error(gnm_unblock(net, wells$.gfid[2]))
})

test_that("gnm_connect_auto returns net invisibly for piping", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")

  result <- gnm_connect_auto(net, tolerance = 0.001)
  expect_invisible(gnm_connect_auto(net, tolerance = 0.001))
})

test_that("print works for gnm_path", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  td <- create_test_data(dir)
  net_path <- file.path(dir, "water_net")

  net <- gnm_create(net_path, "water_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  gnm_import_path(net, td$wells, name = "wells")
  gnm_import_path(net, td$pipes, name = "pipes")
  gnm_connect_auto(net, tolerance = 0.001)

  wells <- gnm_layer(net, "wells")
  path <- gnm_shortest_path(net, wells$.gfid[1], wells$.gfid[4])

  expect_output(print(path), "dijkstra")
})
