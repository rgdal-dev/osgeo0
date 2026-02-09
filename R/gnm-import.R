#' Import a wk data.frame into a GNM network
#'
#' Adds a new class layer to the network from an R data.frame containing
#' a wk geometry column. Features are registered in the network and assigned
#' Global Feature IDs (GFIDs).
#'
#' @param net A [gnm_network] object (must be open).
#' @param data A data.frame with a wk geometry column.
#' @param name Layer name for the imported data.
#' @param geom_col Name of the geometry column. If NULL, auto-detected
#'   as the first column inheriting from `wk_vctr`.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_import <- function(net, data, name, geom_col = NULL) {
  .assert_open(net)

  src <- .wk_df_to_ogr(data, name = name, srs = net@srs, geom_col = geom_col)
  on.exit({
    src$py_ds <- NULL
    unlink(src$tmp_path, force = TRUE)
  }, add = TRUE)

  src_layer <- src$py_ds$GetLayerByName(name)
  result <- net@.env$py_ds$CopyLayer(src_layer, name)

  if (is.null(result)) {
    cli::cli_abort("Failed to import layer {.val {name}} into the network.")
  }

  invisible(net)
}


#' Import a file-based dataset into a GNM network
#'
#' Opens a vector dataset via osgeo.ogr and copies a layer into the network.
#'
#' @param net A [gnm_network] object (must be open).
#' @param dsn Data source name (file path, connection string, etc.).
#' @param layer Layer name or index to import. If NULL, uses the first layer.
#' @param name Name for the layer in the network. If NULL, uses the source
#'   layer name.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_import_path <- function(net, dsn, layer = NULL, name = NULL) {
  .assert_open(net)

  gdal <- .osgeo$gdal

  src_ds <- tryCatch(
    gdal$OpenEx(dsn, gdal$OF_VECTOR),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to open {.path {dsn}} as a vector dataset.",
        x = conditionMessage(e)
      ))
    }
  )
  if (is.null(src_ds)) {
    cli::cli_abort("Failed to open {.path {dsn}} as a vector dataset.")
  }
  on.exit({ src_ds <- NULL }, add = TRUE)

  if (is.null(layer)) {
    src_layer <- src_ds$GetLayerByIndex(0L)
  } else if (is.numeric(layer)) {
    src_layer <- src_ds$GetLayerByIndex(as.integer(layer))
  } else {
    src_layer <- src_ds$GetLayerByName(layer)
  }

  if (is.null(src_layer)) {
    cli::cli_abort("Layer {.val {layer %||% 0}} not found in {.path {dsn}}.")
  }

  if (is.null(name)) {
    name <- src_layer$GetName()
  }

  result <- net@.env$py_ds$CopyLayer(src_layer, name)
  if (is.null(result)) {
    cli::cli_abort("Failed to import layer {.val {name}} into the network.")
  }

  invisible(net)
}


#' List class layers in a GNM network
#'
#' Returns the names of user data layers, excluding GNM system layers
#' (`_gnm_graph`, `_gnm_meta`, `_gnm_features`).
#'
#' @param net A [gnm_network] object (must be open).
#' @return Character vector of layer names.
#' @export
gnm_layers <- function(net) {
  .assert_open(net)
  .class_layer_names(net@.env$py_ds)
}


#' Extract a class layer as a data.frame with wk geometry
#'
#' @param net A [gnm_network] object (must be open).
#' @param name Layer name.
#' @return data.frame with `.gfid`, attribute columns, and `geom` (wk_wkb).
#' @export
gnm_layer <- function(net, name) {
  .assert_open(net)

  py_layer <- net@.env$py_ds$GetLayerByName(name)
  if (is.null(py_layer)) {
    cli::cli_abort("Layer {.val {name}} not found in the network.")
  }

  .ogr_layer_to_wk_df(py_layer, crs = .net_crs(net))
}


#' Look up a single feature by Global Feature ID
#'
#' @param net A [gnm_network] object (must be open).
#' @param gfid Integer Global Feature ID.
#' @return A 1-row data.frame with `.gfid`, attributes, and `geom` (wk_wkb).
#' @export
gnm_feature <- function(net, gfid) {
  .assert_open(net)
  gfid <- .as_gfid(gfid, "gfid")

  feat <- tryCatch(
    net@.env$py_ds$GetFeatureByGlobalFID(gfid),
    error = function(e) NULL
  )

  if (is.null(feat)) {
    cli::cli_abort("Feature with GFID {.val {gfid}} not found.")
  }

  # Extract single feature
  g <- feat$GetGeometryRef()
  geom_raw <- if (!is.null(g)) list(as.raw(g$ExportToWkb())) else list(NULL)

  n <- feat$GetFieldCount()
  rec <- list(.gfid = gfid)
  if (n > 0L) {
    for (i in seq_len(n) - 1L) {
      fname <- feat$GetFieldDefnRef(i)$GetName()
      rec[[fname]] <- feat$GetField(i)
    }
  }

  df <- as.data.frame(rec, stringsAsFactors = FALSE)
  df$geom <- wk::wkb(geom_raw)
  wk::wk_crs(df$geom) <- .net_crs(net)

  df
}
