#' Automatically build network topology
#'
#' Connects point features to line features by matching line endpoints
#' to nearby points within the specified tolerance. Each line whose start
#' and end points both fall within tolerance of a point feature creates
#' a graph edge.
#'
#' @param net A [gnm_network] object (must be open).
#' @param layers Character vector of layer names to consider. If NULL, uses
#'   all class layers.
#' @param tolerance Spatial tolerance in SRS units. For geographic CRS
#'   (degrees), a value like 0.000001 corresponds to ~0.1m at mid-latitudes.
#' @param cost Forward edge cost (default 1).
#' @param inverse_cost Inverse (reverse) edge cost (default 1).
#' @param directed If TRUE, edges are directed (source → target only).
#'   Default FALSE (bidirectional).
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_connect_auto <- function(net, layers = NULL, tolerance = 0.001,
                              cost = 1.0, inverse_cost = 1.0,
                              directed = FALSE) {
  .assert_open(net)

  if (is.null(layers)) {
    layers <- gnm_layers(net)
  }

  result <- tryCatch(
    net@.env$py_ds$ConnectPointsByLines(
      as.list(layers),
      as.double(tolerance),
      as.double(cost),
      as.double(inverse_cost),
      .direction(directed)
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to auto-connect network topology.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Manually connect two features
#'
#' Creates a graph connection between two features identified by their
#' Global Feature IDs. The features can be in the same or different layers.
#'
#' @param net A [gnm_network] object (must be open).
#' @param src Source feature GFID (node).
#' @param tgt Target feature GFID (node).
#' @param connector Connector feature GFID (edge), or -1 for a virtual edge.
#' @param cost Forward edge cost.
#' @param inverse_cost Inverse edge cost.
#' @param directed If TRUE, edge is directed source → target only.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_connect <- function(net, src, tgt, connector = -1L,
                         cost = 1.0, inverse_cost = 1.0,
                         directed = FALSE) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$ConnectFeatures(
      .as_gfid(src, "src"),
      .as_gfid(tgt, "tgt"),
      .as_gfid(connector, "connector"),
      as.double(cost),
      as.double(inverse_cost),
      .direction(directed)
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to connect features {.val {src}} and {.val {tgt}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Disconnect two features
#'
#' Removes the graph connection between two features.
#'
#' @param net A [gnm_network] object (must be open).
#' @param src Source feature GFID.
#' @param tgt Target feature GFID.
#' @param connector Connector feature GFID, or -1 for virtual edges.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_disconnect <- function(net, src, tgt, connector = -1L) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$DisconnectFeatures(
      .as_gfid(src, "src"),
      .as_gfid(tgt, "tgt"),
      .as_gfid(connector, "connector")
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to disconnect features {.val {src}} and {.val {tgt}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Disconnect all features in the network
#'
#' Clears the entire network graph topology. Features remain in the
#' network but have no connections.
#'
#' @param net A [gnm_network] object (must be open).
#' @return `net` invisibly (for piping).
#' @export
gnm_disconnect_all <- function(net) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$DisconnectAll(),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to disconnect all features.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}
