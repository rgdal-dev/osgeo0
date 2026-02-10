#' Find the shortest path between two features
#'
#' Uses Dijkstra's algorithm to find the shortest path between two
#' features identified by their Global Feature IDs. The result is
#' eagerly materialised as a data.frame with wk geometry — no Python
#' references are retained.
#'
#' @param net A [gnm_network] object (must be open).
#' @param from Source feature GFID.
#' @param to Target feature GFID.
#'
#' @return A [gnm_path] object containing the path features.
#' @export
gnm_shortest_path <- function(net, from, to) {
  .assert_open(net)

  from <- .as_gfid(from, "from")
  to <- .as_gfid(to, "to")

  py_layer <- tryCatch(
    net@.env$py_ds$GetPath(from, to, .gnm_algorithm("dijkstra")),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to compute shortest path from {.val {from}} to {.val {to}}.",
        x = conditionMessage(e)
      ))
    }
  )

  if (is.null(py_layer)) {
    cli::cli_abort(
      "No path found from GFID {.val {from}} to GFID {.val {to}}."
    )
  }

  features <- .path_layer_to_wk_df(py_layer, net@.env$py_ds, crs = .net_crs(net))

  # Release the result set
  tryCatch(
    net@.env$py_ds$ReleaseResultSet(py_layer),
    error = function(e) NULL
  )

  gnm_path(
    from = from,
    to = to,
    algorithm = "dijkstra",
    features = features
  )
}


#' Find K shortest paths between two features
#'
#' Uses Yen's algorithm (which internally uses Dijkstra) to find the
#' K shortest paths between two features.
#'
#' @param net A [gnm_network] object (must be open).
#' @param from Source feature GFID.
#' @param to Target feature GFID.
#' @param k Number of paths to find (default 3).
#'
#' @return A [gnm_path] object. The features data.frame contains all K paths;
#'   individual paths can be identified by breaks in the sequence.
#' @export
gnm_k_paths <- function(net, from, to, k = 3L) {
  .assert_open(net)

  from <- .as_gfid(from, "from")
  to <- .as_gfid(to, "to")
  k <- as.integer(k)

  py_layer <- tryCatch(
    net@.env$py_ds$GetPath(
      from, to,
      .gnm_algorithm("kpaths"),
      list(paste0("num_paths=", k))
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to compute K shortest paths from {.val {from}} to {.val {to}}.",
        x = conditionMessage(e)
      ))
    }
  )

  if (is.null(py_layer)) {
    cli::cli_abort(
      "No paths found from GFID {.val {from}} to GFID {.val {to}}."
    )
  }

  features <- .path_layer_to_wk_df(py_layer, net@.env$py_ds, crs = .net_crs(net))

  tryCatch(
    net@.env$py_ds$ReleaseResultSet(py_layer),
    error = function(e) NULL
  )

  gnm_path(
    from = from,
    to = to,
    algorithm = paste0("kpaths_", k),
    features = features
  )
}


#' Find connected components in the network
#'
#' Identifies groups of features that are connected to each other
#' in the network graph.
#'
#' @param net A [gnm_network] object (must be open).
#'
#' @return A [gnm_components] object.
#' @export
gnm_components <- function(net) {
  .assert_open(net)

  py_layer <- tryCatch(
    net@.env$py_ds$GetPath(0L, 0L, .gnm_algorithm("components")),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to compute connected components.",
        x = conditionMessage(e)
      ))
    }
  )

  if (is.null(py_layer)) {
    cli::cli_abort("Connected components analysis returned no results.")
  }

  features <- .path_layer_to_wk_df(py_layer, net@.env$py_ds, crs = .net_crs(net))

  tryCatch(
    net@.env$py_ds$ReleaseResultSet(py_layer),
    error = function(e) NULL
  )

  # Component IDs need to be derived from the result structure.
  # GNM connected components returns features grouped by component,
  # but the grouping mechanism varies. For now, assign a sequential

  # component ID — this will need refinement based on actual GNM output.
  if (nrow(features) > 0 && !".component_id" %in% names(features)) {
    features$.component_id <- 1L
  }

  gnm_components(features)
}
