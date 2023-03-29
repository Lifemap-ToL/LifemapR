#' add a layer to a Lifemap object
#'
#' @param radius a numeric vector of radii for the points
#' @param min the minimal size
#' @param max the maximal size
#' @param fillColor fill color
#' @param fillColor_pal the palette to be used if <code>fillColor</code> represent a variable
#' @param stroke whether to draw a border for points
#' @param color stroke color
#' @param color_pal the palette to be used if <code>color</code> represent a variable
#' @param weight stroke width in pixels
#' @param opacity stroke opacity
#' @param fillOpacity fill opacity
#' @param legend whether to display the legend
#' @param FUN the function to be applied to the variables
#' @param ... every argument that can be passed to the leaflet::addCircleMarkers function
#'
#' @return a dataframe containing all aesthetics informations for one serie of markers
#' @export
#'
#' @examples lm_markers(radius="GC.", fillColor="Genes", min=10, max=80, FUN="mean", pal="Accent", legend=TRUE, stroke = TRUE, weight="Size..Mb.")
lm_markers <- function(radius,
                       min = 20,
                       max = 50,
                       fillColor,
                       fillColor_pal,
                       shape = "circle",
                       stroke = FALSE,
                       color = "black",
                       color_pal = "viridis",
                       weight = 0,
                       opacity = 0.8,
                       fillOpacity = 0.8,
                       legend = TRUE,
                       FUN,
                       ...) {
  res <- data.frame(radius = radius, fillColor = fillColor,
                    min = min,max = max,
                    pass_info = FUN, stroke = stroke,
                    color = color, weight = weight,
                    opacity = opacity, fillOpacity = fillOpacity,
                    shape = shape, fillColor_pal = fillColor_pal,
                    color_pal = color_pal, legend = legend, ...)
  class(res)=c("lifemap_obj", "lm_markers", "data.frame")
  return(res)
}

#' Reports whether x is a lm_markers object
#' @param x the object to test
#' @export
is.lm_markers <- function(x) {inherits(x, "lm_markers")}
