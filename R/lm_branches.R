#' add a layer to a Lifemap object
#'
#' @param col either a color for the branches or a variable that will be represented by the branches' color
#' @param FUN the function to be applied if col represent a variable
#' @param pal the branches' palette
#'
#' @return a lifemap object
#' @export
#'
#' @examples
#' # a unique color for all the branches
#' lm_branches(col="red")
#'
#' # branches are colored according to the average GC rate
#' lm_branches(col="GC.", FUN="mean")
lm_branches <- function(col = "yellow", FUN = "mean", pal = "Accent", legend=TRUE) {
  res <- data.frame(color = col, FUN = FUN, color_pal = pal, legend = legend)
  class(res)=c("lifemap_obj", "lm_branches", "data.frame")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_branches <- function(x) {inherits(x, "lm_branches")}
