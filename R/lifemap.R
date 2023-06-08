#' create a new Lifemap visualisation
#'
#' @description
#' initialise a lifemap object. It can be used to describe aesthetics like legend's properties ...
#'
#'
#' @param lm_obj Lifemap object used for data visualisation
#'
#' @return a lifemap object
#' @export
lifemap <- function(lm_obj,
                    zoom = 4) {
  lm_obj$options <- list(zoom = zoom)
  return(lm_obj)
}
