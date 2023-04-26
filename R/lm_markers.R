#' add a layer to a Lifemap object
#'
#' @description
#' Allow to add a layer with circles that can represent data. The parameters that can be used to represent data are the following:
#' - radius
#' - fillColor
#' - stroke
#' - color
#'
#'
#'
#' @param data a sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used
#' @param radius either a numerical value or a column name of the original dataframe to represent this variable by the size of points
#' @param min an integer indicating the minimal size of the points if radius represent a variable
#' @param max an integer indicating the maximal size of the points if radius represent a variable
#' @param fillColor either a color or a palette if the fillColor is represented by a variable
#' @param var_fillColor a column name of the original dataframe to represent this variable by the fillColor of points
#' @param fillOpacity a numeric indicating the fill opacity
#' @param stroke a logical indicating whether to draw a border for points
#' @param color the stroke color. Either a color or a palette if the stroke color is represented by a variable
#' @param var_color a column name of the original dataframe to represent this variable by the stroke color of points
#' @param weight stroke width in pixels
#' @param opacity stroke opacity
#' @param legend whether to display the legend
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"),
#' @param legendOrientation c("vertical", "horizontal"),
#' @param legendOpacity legend's opacity (apply on the shapes in the legend, not the background itself)
#' @param FUN the function to be applied to the variables if NULL then the information missing from the parent nodes won't be inferred
#' @param display c("auto", "requested", "all", "leaves"), a string indicating how to display points :
#' - "auto" : the markers are displayed depending on the zoom, by default, allow to have a lot of points
#' - "requested" : only display the requested taxa, but all at the same time
#' - "all" : display all the taxa including all the ancestors to the root
#' - "leaves" : display only the last (most recent) taxa
#'
#' (WARNING : "requested" and "auto" shouldn't be used to display more than 1000 markers as it may result in long computing time)
#'
#' @return a list containing all aesthetics informations for one serie of markers
#' @export
lm_markers <- function(data = NULL,
                       radius = 20,
                       min = 20,
                       max = 50,
                       fillColor = NULL,
                       var_fillColor = NULL,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       color = NULL,
                       var_color = NULL,
                       weight = 3,
                       opacity = 0.8,
                       legend = TRUE,
                       legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
                       legendOrientation = c("vertical", "horizontal"),
                       legendOpacity = 0.5,
                       FUN = NULL,
                       display = c("auto", "requested", "all", "leaves")) {
  legendPosition <- match.arg(legendPosition)
  legendOrientation <- match.arg(legendOrientation)
  display <- match.arg(display)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}
  res <- list(taxids = taxids, radius = radius,
              fillColor = fillColor, min = min,
              max = max, FUN = FUN, stroke = stroke,
              color = color, weight = weight,
              opacity = opacity, fillOpacity = fillOpacity,
              var_fillColor = var_fillColor, var_color = var_color,
              legend = legend, legendPosition = legendPosition,
              legendOrientation = legendOrientation, legendOpacity = legendOpacity,
              display = display)
  class(res)=c("lifemap_obj", "lm_markers", "list")
  return(res)
}

#' Reports whether x is a lm_markers object
#' @param x the object to test
#' @export
is.lm_markers <- function(x) {inherits(x, "lm_markers")}
