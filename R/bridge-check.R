# Internal validation and helper functions


#' Assert that a gnm_network is open
#' @noRd
.assert_open <- function(net) {
  if (net@.env$closed) {
    cli::cli_abort(
      "{.cls gnm_network} {.val {net@name}} is not open.",
      call = parent.frame()
    )
  }
}


#' Assert that GNM support is available in the Python environment
#' @noRd
.assert_gnm_available <- function() {
  gdal <- .osgeo$gdal
  drv <- tryCatch(
    gdal$GetDriverByName("GNMFile"),
    error = function(e) NULL
  )
  if (is.null(drv)) {
    cli::cli_abort(c(
      "GNM support not available in the current GDAL Python bindings.",
      i = "Ensure {.pkg osgeo} is built with GNM enabled.",
      i = "{.code conda install -c conda-forge gdal} includes GNM."
    ))
  }
}


#' Coerce an R value to an integer suitable for GFID arguments
#' @noRd
.as_gfid <- function(x, arg = "gfid") {
  if (length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be length 1, not {length(x)}.")
  }
  x <- as.integer(x)
  if (is.na(x)) {
    cli::cli_abort("{.arg {arg}} must not be NA.")
  }
  x
}


#' Map directed = TRUE/FALSE to GNM direction constants
#' @noRd
.direction <- function(directed = FALSE) {
  if (isTRUE(directed)) {
    .osgeo$gnm$GNM_EDGE_DIR_SRCTOTGT
  } else {
    .osgeo$gnm$GNM_EDGE_DIR_BOTH
  }
}


#' Register a destructor on the gnm_network's environment
#'
#' Called after successfully opening/creating a network. The destructor
#' flushes and releases the Python dataset reference on GC.
#' @noRd
.register_destructor <- function(net) {
  e <- net@.env
  reg.finalizer(e, function(env) {
    if (!env$closed && !is.null(env$py_ds)) {
      tryCatch(env$py_ds$FlushCache(), error = function(e) NULL)
      env$py_ds <- NULL
      env$closed <- TRUE
    }
  }, onexit = TRUE)
}


#' Get the CRS string from a network's Python dataset
#' @noRd
.net_crs <- function(net) {
  net@srs
}


#' Named lookup for GNM graph algorithm constants
#' @noRd
.gnm_algorithm <- function(name) {
  # These map to GNMGraphAlgorithmType enum values
  # We look them up from the module to stay in sync
  switch(name,
    dijkstra   = .osgeo$gnm$GATDijkstraShortestPath,
    kpaths     = .osgeo$gnm$GATKShortestPath,
    components = .osgeo$gnm$GATConnectedComponents,
    cli::cli_abort("Unknown GNM algorithm: {.val {name}}")
  )
}


#' List class layers (excluding GNM system layers)
#'
#' GNM system layers are prefixed with "_gnm_" or are named
#' "_gnm_graph", "_gnm_meta", "_gnm_features".
#' @noRd
.class_layer_names <- function(py_ds) {
  n <- py_ds$GetLayerCount()
  names <- character(n)
  for (i in seq_len(n)) {
    names[i] <- py_ds$GetLayerByIndex(as.integer(i - 1L))$GetName()
  }
  # Filter out system layers
  names[!grepl("^_gnm_", names)]
}
