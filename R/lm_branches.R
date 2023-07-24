#' Add a "branches" layer to a lifemap_obj object.
#'
#' @param data A sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used.
#' @param color Either a color for the branches or a palette if a variable is used to represent branches' color.
#' @param var_color A column name of the original dataframe to represent this variable by the color of branches.
#' @param size Either a numeric for the branche's thickness or a variable to be represented by the branche's thickness.
#' @param min An integer indicating the minimal thickness of the branches if the size is a column name.
#' @param max An integer indicating the maximal thickness of the branches if the size is a column name.
#' @param opacity An integer indicating branche's opacity.
#' @param FUN The function to be applied to infer values. If NULL values won't be inferred
#' @param legend A logical indicating whether or not to display the legend.
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"), the position of the legend.
#'
#' @return An lm_branches object containing all aesthetics details for one layer of branches
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
                        opacity = 0.5,
                        FUN = NULL,
                        legend=TRUE,
                        legendPosition = c("topright", "bottomright", "bottomleft", "topleft")) {

  legendPosition <- match.arg(arg = legendPosition, choices = legendPosition)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}

  if (is.null(var_color)){
    var_color <- "default"
  }
  palette <- NULL
  if (var_color %in% "default") {
    if (is.null(color)) {
      color <- "yellow"
    }
  } else {
    if (is.null(color)) {
      palette <- "RdBu"
    } else { palette <- color }
  }

  value <- NULL
  if (is.numeric(size)){
    value <- size
    size <- "default"
  }

  res <- list(taxids = taxids, color = color, palette = palette,
              FUN = FUN, var_color = var_color, value = value,
              size = size, min = min, max = max, opacity = opacity,
              legend = legend, legendPosition = legendPosition)
  class(res)=c("lifemap_obj", "lm_branches", "list")
  return(res)
}

#' Reports whether x is a lm_branches object.
#' @param x The object to test.
#' @return A boolean indicating whether or not the object is of lm_branches type.
#' @export
is.lm_branches <- function(x) {inherits(x, "lm_branches")}
