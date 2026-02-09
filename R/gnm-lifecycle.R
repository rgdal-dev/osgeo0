#' Create a new GNM network
#'
#' Creates a new Geographic Network Model backed by a GDAL vector format
#' (default: ESRI Shapefile). The network is returned in an open state,
#' ready for importing layers and building topology.
#'
#' @param path Directory path where the network will be stored.
#' @param name Network name (also used as the directory/schema name).
#' @param srs Spatial reference system as a string (e.g., "EPSG:4326").
#' @param format Underlying OGR vector format. Default "ESRI Shapefile".
#' @param description Optional description string.
#'
#' @return A [gnm_network] object in the open state.
#' @export
#'
#' @examples
#' \dontrun{
#' net <- gnm_create(tempfile(), "my_network", srs = "EPSG:4326")
#' gnm_close(net)
#' }
gnm_create <- function(path, name, srs = "EPSG:4326",
                        format = "ESRI Shapefile", description = "") {
  .assert_gnm_available()

  path <- normalizePath(path, mustWork = FALSE)
  gdal <- .osgeo$gdal

  drv <- gdal$GetDriverByName("GNMFile")

  # Creation options
  opts <- list(
    paste0("net_name=", name),
    paste0("net_srs=", srs),
    paste0("net_description=", description),
    paste0("FORMAT=", format)
  )

  py_ds <- tryCatch(
    drv$Create(path, 0L, 0L, 0L, gdal$GDT_Unknown, opts),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to create GNM network at {.path {path}}.",
        x = conditionMessage(e)
      ))
    }
  )

  if (is.null(py_ds)) {
    cli::cli_abort("Failed to create GNM network at {.path {path}}.")
  }

  env <- new.env(parent = emptyenv())
  env$py_ds <- py_ds
  env$closed <- FALSE

  net <- gnm_network(
    path = path,
    name = name,
    srs = srs,
    description = description,
    .env = env
  )

  .register_destructor(net)
  net
}


#' Open an existing GNM network
#'
#' Opens a previously created GNM network for reading and analysis.
#' Use `gnm_close()` when finished, or rely on the GC destructor.
#'
#' @param path Directory path to the network.
#' @param update If TRUE, open for writing (default TRUE).
#'
#' @return A [gnm_network] object in the open state.
#' @export
#'
#' @examples
#' \dontrun{
#' net <- gnm_open("path/to/my_network")
#' gnm_shortest_path(net, 1, 10)
#' gnm_close(net)
#' }
gnm_open <- function(path, update = TRUE) {
  .assert_gnm_available()

  path <- normalizePath(path, mustWork = TRUE)
  gdal <- .osgeo$gdal

  flags <- gdal$OF_GNM
  if (update) flags <- bitwOr(flags, gdal$OF_UPDATE)

  py_ds <- tryCatch(
    gdal$OpenEx(path, flags),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to open GNM network at {.path {path}}.",
        x = conditionMessage(e)
      ))
    }
  )

  if (is.null(py_ds)) {
    cli::cli_abort("Failed to open GNM network at {.path {path}}.")
  }

  # Extract metadata from the opened network
  name <- tryCatch(py_ds$GetName(), error = function(e) basename(path))
  description <- tryCatch(py_ds$GetDescription(), error = function(e) "")

  # Get SRS from the network
  srs_str <- tryCatch({
    sr <- py_ds$GetSpatialRef()
    if (!is.null(sr)) sr$ExportToWkt() else "EPSG:4326"
  }, error = function(e) "EPSG:4326")

  env <- new.env(parent = emptyenv())
  env$py_ds <- py_ds
  env$closed <- FALSE

  net <- gnm_network(
    path = path,
    name = name,
    srs = srs_str,
    description = description,
    .env = env
  )

  .register_destructor(net)
  net
}


#' Close a GNM network
#'
#' Flushes pending changes and releases the Python dataset reference.
#' After closing, the network object can no longer be used for operations.
#'
#' @param net A [gnm_network] object.
#' @return NULL invisibly.
#' @export
gnm_close <- function(net) {
  if (!net@.env$closed) {
    tryCatch(net@.env$py_ds$FlushCache(), error = function(e) NULL)
    net@.env$py_ds <- NULL
    net@.env$closed <- TRUE
  }
  invisible(NULL)
}


#' Use a GNM network with automatic cleanup
#'
#' Opens a network, passes it to `fun`, and ensures it is closed
#' afterwards regardless of success or failure.
#'
#' @param path Directory path to an existing network.
#' @param fun A function taking a single argument (the gnm_network).
#' @param update If TRUE, open for writing.
#'
#' @return The return value of `fun`.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- with_gnm("path/to/network", function(net) {
#'   gnm_shortest_path(net, 1, 10)
#' })
#' }
with_gnm <- function(path, fun, update = TRUE) {
  net <- gnm_open(path, update = update)
  on.exit(gnm_close(net), add = TRUE)
  fun(net)
}
