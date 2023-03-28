#' add a layer to a Lifemap object
#'
#' @param col either a color for the branches or a variable that will be represented by the branches' color
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
lm_branches <- function(col="yellow", FUN="mean") {
  res <- c(col, FUN)
  class(res)=c("lifemap_obj", "lm_branches", "vector")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_branches <- function(x) {inherits(x, "lm_branches")}
