#' @title GNM Network
#' @description S7 class representing an open (or closed) GNM network dataset.
#' The mutable Python dataset reference lives in a private environment for
#' reference semantics within the value-semantic S7 shell.
#' @export
gnm_network <- S7::new_class("gnm_network",
  properties = list(
    name = S7::class_character,
    srs  = S7::class_character,
    path = S7::class_character,
    description = S7::class_character,
    .env = S7::class_environment
  ),
  constructor = function(path, name, srs = "EPSG:4326",
                         description = "", .env = NULL) {
    if (is.null(.env)) {
      .env <- new.env(parent = emptyenv())
      .env$py_ds <- NULL
      .env$closed <- TRUE
    }
    S7::new_object(
      S7::S7_object(),
      name = name,
      srs = srs,
      path = path,
      description = description,
      .env = .env
    )
  }
)


#' @title GNM Path Result
#' @description S7 class holding an eagerly materialised path result as a
#' data.frame with a wk geometry column. No Python references are retained.
#' @export
gnm_path <- S7::new_class("gnm_path",
  properties = list(
    from = S7::class_integer,
    to = S7::class_integer,
    algorithm = S7::class_character,
    features = S7::class_data.frame,
    n_features = S7::class_integer
  ),
  constructor = function(from, to, algorithm, features) {
    S7::new_object(
      S7::S7_object(),
      from = as.integer(from),
      to = as.integer(to),
      algorithm = algorithm,
      features = features,
      n_features = as.integer(nrow(features))
    )
  }
)


#' @title GNM Connected Components Result
#' @description S7 class holding connected component analysis results.
#' @export
gnm_components <- S7::new_class("gnm_components",
  properties = list(
    features = S7::class_data.frame,
    n_components = S7::class_integer
  ),
  constructor = function(features) {
    nc <- length(unique(features$.component_id))
    S7::new_object(
      S7::S7_object(),
      features = features,
      n_components = as.integer(nc)
    )
  }
)
