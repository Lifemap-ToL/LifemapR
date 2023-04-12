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
#' @param data a subdataset to use, if NULL then all of the taxids from the lifemap object given to lifemap will be used
#' @param radius either a numerical value or a column name of the original dataframe to represent this variable by the size of points
#' @param min the minimal size of the points
#' @param max the maximal size of the points
#' @param fillColor either a color or a column name of the original dataframe to represent this variable by the size of points
#' @param fillColor_pal the palette to be used if <code>fillColor</code> represent a variable
#' @param fillOpacity fill opacity
#' @param stroke whether to draw a border for points
#' @param color stroke color
#' @param color_pal the palette to be used if <code>color</code> represent a variable
#' @param weight stroke width in pixels
#' @param opacity stroke opacity
#' @param legend whether to display the legend
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"),
#' @param legendOrientation c("vertical", "horizontal"),
#' @param legendOpacity legend's opacity (apply on the shapes in the legend, not the background itself)
#' @param FUN the function to be applied to the variables if None then the information missing from the parent nodes won't be inferred
#' @param display a string indicating how to display points :
#' - "auto" : the markers are displayed depending on the zoom, by default, allow to have a lot of points
#' - "requested" : only display the requested taxids, but all at the same time
#' - "all" : display all the taxids including all the ancestors to the root
#'
#' (WARNING : "requested" and "auto" shouldn't be used to display more than 1000 markers as it may result in a crash)
#'
#' @return a dataframe containing all aesthetics informations for one serie of markers
#' @export
#'
#' @examples lm_markers(radius = "GC.", fillColor = "Genes", min = 10, max = 80, FUN = "mean", fillColor_pal = "Accent", legend = TRUE, stroke = TRUE)
#'
#' # to apply it only on a subdataset
#' lm_markers(data = LM$df[LM$df$Group %in% "Fungi",], radius = "GC.", fillColor = "Genes", min = 10, max = 80, FUN = "mean", pal = "Accent", legend = TRUE, stroke = TRUE)
lm_markers <- function(data = NULL,
                       radius = 30,
                       min = 20,
                       max = 50,
                       fillColor = "red",
                       fillColor_pal = "Accent",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       color = "black",
                       color_pal = "viridis",
                       weight = 0,
                       opacity = 0.8,
                       legend = TRUE,
                       legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
                       legendOrientation = c("vertical", "horizontal"),
                       legendOpacity = 0.5,
                       FUN = NULL,
                       display = c("auto", "requested", "all")) {
  legendPosition <- match.arg(legendPosition)
  legendOrientation <- match.arg(legendOrientation)
  display <- match.arg(display)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}
  res <- list(taxids = taxids, radius = radius,
              fillColor = fillColor, min = min,
              max = max,FUN = FUN, stroke = stroke,
              color = color, weight = weight,
              opacity = opacity, fillOpacity = fillOpacity,
              fillColor_pal = fillColor_pal, color_pal = color_pal,
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
