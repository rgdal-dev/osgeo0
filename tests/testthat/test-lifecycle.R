test_that("gnm_create creates an open network", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net_path <- file.path(dir, "test_net")

  net <- gnm_create(net_path, "test_net", srs = "EPSG:4326")
  on.exit(gnm_close(net), add = TRUE)

  expect_s3_class(net, "gnm_network")
  expect_false(net@.env$closed)
  expect_equal(net@name, "test_net")
  expect_equal(net@srs, "EPSG:4326")
  expect_true(dir.exists(net_path))
})

test_that("gnm_close marks network as closed", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")

  gnm_close(net)

  expect_true(net@.env$closed)
  expect_null(net@.env$py_ds)
})

test_that("gnm_close is idempotent", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")

  gnm_close(net)
  expect_no_error(gnm_close(net))
  expect_true(net@.env$closed)
})

test_that("operations on closed network error informatively", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")
  gnm_close(net)

  expect_error(gnm_layers(net), "not open")
})

test_that("gnm_open re-opens an existing network", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net_path <- file.path(dir, "test_net")

  net <- gnm_create(net_path, "test_net", srs = "EPSG:4326")
  gnm_close(net)

  net2 <- gnm_open(net_path)
  on.exit(gnm_close(net2), add = TRUE)

  expect_false(net2@.env$closed)
  expect_equal(net2@path, normalizePath(net_path))
})

test_that("with_gnm auto-closes on success", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net_path <- file.path(dir, "test_net")
  gnm_close(gnm_create(net_path, "test_net"))

  env_ref <- NULL
  result <- with_gnm(net_path, function(net) {
    env_ref <<- net@.env
    gnm_layers(net)
  })

  expect_true(env_ref$closed)
  expect_type(result, "character")
})

test_that("with_gnm auto-closes on error", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net_path <- file.path(dir, "test_net")
  gnm_close(gnm_create(net_path, "test_net"))

  env_ref <- NULL
  try(with_gnm(net_path, function(net) {
    env_ref <<- net@.env
    stop("deliberate error")
  }), silent = TRUE)

  expect_true(env_ref$closed)
})

test_that("print works for open and closed networks", {
  skip_if_no_gnm()

  dir <- withr::local_tempdir()
  net <- gnm_create(file.path(dir, "test_net"), "test_net")

  expect_output(print(net), "OPEN")

  gnm_close(net)
  expect_output(print(net), "CLOSED")
})
