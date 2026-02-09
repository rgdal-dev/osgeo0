#' osgeo0: R Interface to GDAL Geographic Network Model
#'
#' @description
#' Idiomatic R bindings for the GDAL Geographic Network Model (GNM) via
#' the Python osgeo.gnm module and reticulate. Create, manage, and analyse
#' geographic networks with topology building, shortest-path analysis,
#' and connected component detection.
#'
#' Uses [wk] for geometry interchange and [S7] for the class system.
#'
#' @section Getting started:
#'
#' ```r
#' net <- gnm_create(tempdir(), "my_network", srs = "EPSG:4326")
#' gnm_import(net, wells_df, "wells")
#' gnm_import(net, pipes_df, "pipes")
#' gnm_connect_auto(net, tolerance = 0.000001)
#' path <- gnm_shortest_path(net, from = 1, to = 10)
#' gnm_close(net)
#' ```
#'
#' @keywords internal
"_PACKAGE"


# NULL operator for %||%
`%||%` <- function(x, y) if (is.null(x)) y else x
