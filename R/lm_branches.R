#' add a layer to a Lifemap object
#'
#' @param data a sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used
#' @param color either a color for the branches or a palette if a variable is used to rperesent branches' color
#' @param var_color a column name of the original dataframe to represent this variable by the color of branches
#' @param size either a numeric for the branche's thickness or a variable to be représented by the branche's thickness
#' @param min an integer indicating the minimal thickness of the branches if the size represent a variable
#' @param max an integer indicating the maximal thickness of the branches if the size represent a variable
#' @param FUN the function to be applied to infer values
#' @param legend a logical indicating whether or not to display the legend
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"), the position of the legend
#'
#' @return a lifemap object
#' @export
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' lm_branches(var_color = "GC.", color = "Accent")
#'
#' lm_branches(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants",])
#'
lm_branches <- function(data = NULL,
                        color = NULL,
                        var_color = NULL,
                        size = 5,
                        min = 2,
                        max = 20,
                        FUN = NULL,
                        legend=TRUE,
                        legendPosition = c("topright", "bottomright", "bottomleft", "topleft")) {

  legendPosition <- match.arg(arg = legendPosition, choices = legendPosition)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}

  res <- list(taxids = taxids, color = color,
              FUN = FUN, var_color = var_color,
              size = size, min = min, max = max,
              legend = legend, legendPosition = legendPosition)
  class(res)=c("lifemap_obj", "lm_branches", "list")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_branches <- function(x) {inherits(x, "lm_branches")}
