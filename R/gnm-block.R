#' Block a feature in the network
#'
#' Blocked features are excluded from path calculations, simulating
#' e.g. a broken pipe or closed road.
#'
#' @param net A [gnm_network] object (must be open).
#' @param gfid Global Feature ID to block.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_block <- function(net, gfid) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$ChangeBlockState(.as_gfid(gfid), TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to block feature {.val {gfid}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Unblock a feature in the network
#'
#' @param net A [gnm_network] object (must be open).
#' @param gfid Global Feature ID to unblock.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_unblock <- function(net, gfid) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$ChangeBlockState(.as_gfid(gfid), FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to unblock feature {.val {gfid}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Block all features in the network
#'
#' @param net A [gnm_network] object (must be open).
#' @return `net` invisibly (for piping).
#' @export
gnm_block_all <- function(net) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$ChangeAllBlockState(TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to block all features.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Unblock all features in the network
#'
#' @param net A [gnm_network] object (must be open).
#' @return `net` invisibly (for piping).
#' @export
gnm_unblock_all <- function(net) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$ChangeAllBlockState(FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to unblock all features.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}
