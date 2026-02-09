#' @export
print.gnm_network <- function(x, ...) {
  state <- if (x@.env$closed) "CLOSED" else "OPEN"

  cli::cli_h3("GNM Network: {x@name} [{state}]")
  cli::cli_bullets(c(
    " " = "Path: {x@path}",
    " " = "SRS: {x@srs}"
  ))

  if (nzchar(x@description)) {
    cli::cli_bullets(c(" " = "Description: {x@description}"))
  }

  if (!x@.env$closed) {
    layers <- tryCatch(gnm_layers(x), error = function(e) character(0))
    rules <- tryCatch(gnm_rules(x), error = function(e) character(0))
    cli::cli_bullets(c(
      " " = "Layers ({length(layers)}): {paste(layers, collapse = ', ')}",
      " " = "Rules: {length(rules)}"
    ))
  }

  invisible(x)
}


#' @export
print.gnm_path <- function(x, ...) {
  cli::cli_h3("GNM Path ({x@algorithm})")
  cli::cli_bullets(c(
    " " = "From GFID {x@from} to GFID {x@to}",
    " " = "{x@n_features} feature{?s} along path"
  ))

  if (x@n_features > 0L) {
    n_nodes <- sum(x@features$.role == "node", na.rm = TRUE)
    n_edges <- sum(x@features$.role == "edge", na.rm = TRUE)
    if (n_nodes > 0 || n_edges > 0) {
      cli::cli_bullets(c(
        " " = "{n_nodes} node{?s}, {n_edges} edge{?s}"
      ))
    }
  }

  invisible(x)
}


#' @export
print.gnm_components <- function(x, ...) {
  cli::cli_h3("GNM Connected Components")
  cli::cli_bullets(c(
    " " = "{x@n_components} component{?s}",
    " " = "{nrow(x@features)} total feature{?s}"
  ))

  invisible(x)
}
