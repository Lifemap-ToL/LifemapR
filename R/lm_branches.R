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
#' lm_branches(col = "red")
#'
#' # branches are colored according to the average GC rate
#' lm_branches(col = "GC.", FUN = "mean")
#'
#' # to apply it on a subdataset
#' lm_branches(data = LM$df[LM$df$Group %in% "Fungi",], col = "GC.", FUN = "mean")
lm_branches <- function(data = NULL,
                        color = NULL,
                        var_color = NULL,
                        FUN = "mean",
                        legend=TRUE,
                        legendPosition = c("topright", "bottomright", "bottomleft", "topleft")) {

  legendPosition <- match.arg(arg = legendPosition, choices = legendPosition)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}

  res <- list(taxids = taxids, color = color,
              FUN = FUN, var_color = var_color,
              legend = legend, legendPosition = legendPosition)
  class(res)=c("lifemap_obj", "lm_branches", "list")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_branches <- function(x) {inherits(x, "lm_branches")}
