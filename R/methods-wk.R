# wk integration
#
# Register gnm_path and gnm_components as wk-handleable so they
# work directly with wk_plot(), wk_bbox(), geos::as_geos_geometry(), etc.


#' Handle wk operations on gnm_path objects
#'
#' Delegates to the underlying wk geometry column in the features data.frame.
#'
#' @param handleable A [gnm_path] object.
#' @param handler A wk handler.
#' @param ... Passed to [wk::wk_handle()].
#'
#' @return Result of the handler.
#' @export
wk_handle.gnm_path <- function(handleable, handler, ...) {
  wk::wk_handle(handleable@features$geom, handler, ...)
}


#' Handle wk operations on gnm_components objects
#'
#' @param handleable A [gnm_components] object.
#' @param handler A wk handler.
#' @param ... Passed to [wk::wk_handle()].
#'
#' @return Result of the handler.
#' @export
wk_handle.gnm_components <- function(handleable, handler, ...) {
  wk::wk_handle(handleable@features$geom, handler, ...)
}
