#' add a layer to a Lifemap object
#'
#' @param data a sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used
#' @param color either a color for the branches or a palette if a variable is used to rperesent branches' color
#' @param var_color a column name of the original dataframe to represent this variable by the color of branches
#' @param FUN the function to be applied if col represent a variable
#' @param legend a logical indicating whether or not to display the legend
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"), the position of the legend
#'
#' @return a lifemap object
#' @export
lm_branches <- function(data = NULL,
                        color = NULL,
                        var_color = NULL,
                        FUN = mean,
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
