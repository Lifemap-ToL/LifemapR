#' add a layer to a Lifemap object
#'
#' @param size the size
#' @param col the colour
#' @param min the minimal size
#' @param max the maximal size
#' @param FUN the function to be applied to the variables
#' @param ... every argument that can be passed to the leaflet::addCircleMarkers function
#'
#' @return a dataframe containing all aesthetics informations for one serie of markers
#' @export
#'
#' @examples lm_markers(radius="GC.", fillColor="Genes", min=30, max=40, FUN="mean")
lm_markers <- function(radius, fillColor, stroke=FALSE, color="black", weight=5, opacity=0.8, fillOpacity=0.8, popup="", label="", min=20, max=50, FUN="",...) {
  res <- data.frame(radius=radius, fillColor=fillColor,
                    min=min,max=max,
                    pass_info=FUN, stroke=stroke,
                    color=color, weight=weight,
                    opacity=opacity, fillOpacity=fillOpacity,
                    popup=popup, label=label,...)
  class(res)=c("lifemap_obj", "lm_markers","data.frame")
  return(res)
}

#' Reports whether x is a lm_obj object
#' @param x the object to test
#' @export
is.lm_markers <- function(x) {inherits(x, "lm_markers")}
