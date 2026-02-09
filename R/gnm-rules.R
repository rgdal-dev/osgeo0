#' Add a connection rule to the network
#'
#' Rules constrain which layer types can be connected. For example,
#' `"ALLOW CONNECTS pipes WITH wells"` means pipes can only connect to wells.
#'
#' @param net A [gnm_network] object (must be open).
#' @param rule Rule string (e.g., "ALLOW CONNECTS pipes WITH wells").
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_add_rule <- function(net, rule) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$CreateRule(rule),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to add rule {.val {rule}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' List network rules
#'
#' @param net A [gnm_network] object (must be open).
#' @return Character vector of rule strings.
#' @export
gnm_rules <- function(net) {
  .assert_open(net)

  rules <- tryCatch(
    net@.env$py_ds$GetRules(),
    error = function(e) character(0)
  )

  if (is.null(rules)) return(character(0))
  as.character(rules)
}


#' Delete a specific rule from the network
#'
#' @param net A [gnm_network] object (must be open).
#' @param rule Rule string to delete.
#'
#' @return `net` invisibly (for piping).
#' @export
gnm_delete_rule <- function(net, rule) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$DeleteRule(rule),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to delete rule {.val {rule}}.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}


#' Delete all rules from the network
#'
#' @param net A [gnm_network] object (must be open).
#' @return `net` invisibly (for piping).
#' @export
gnm_delete_all_rules <- function(net) {
  .assert_open(net)

  tryCatch(
    net@.env$py_ds$DeleteAllRules(),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to delete all rules.",
        x = conditionMessage(e)
      ))
    }
  )

  invisible(net)
}
