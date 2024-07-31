#' Initialise a new Lifemap visualisation.
#'
#' @description
#' Initialise a lifemap_obj object.
#' It can be used to describe aesthetics like the zoom level at which taxids becomes visible...
#'
#'
#' @param lm_obj lifemap_obj object used for data visualisation.
#' @param zoom The level of zoom for which values are displayed (if zoom = 0 only the nodes at the current zoom level or lower can have their values displayed).
#'
#' @return A lifemap_obj object.
#' @export
lifemap <- function(lm_obj, zoom = 4) {
  lm_obj$options <- list(zoom = zoom)
  return(lm_obj)
}
