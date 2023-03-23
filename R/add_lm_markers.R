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
#' @examples add_lm_markers(size="GC.", col="Genes", min=30, max=40, FUN="mean")
add_lm_markers <- function(radius, fillColor, stroke=FALSE, color="#03F", weight=5, opacity=0.8, fillOpacity=0.5, popup="", label="", min=20, max=50, FUN="",...) {
  res <- data.frame(radius=radius, fillColor=fillColor,
                    min=min,max=max,
                    pass_info=FUN, stroke=stroke,
                    color=color, weight=weight,
                    opacity=opacity, fillOpacity=fillOpacity,
                    popup=popup, label=label,...)
  class(res)=c("lifemap.obj", "lm_markers","data.frame")
  return(res)
}

#' Reports wheter x is a lm_obj object
#' @param x the object to test
#' @export
is.lm_markers <- function(x) {inherits(x, "lm_markers")}

#method for lm_obj
#' Add a graphical element to a tree visualisation
#' @param e1 an object of class lm_obj that contains at least $df, a dataframe, and $basemap, hte map used ti get the coordinates
#' @param e2 a description of the graphical features wanted for a set of points
#' @export
#' @return a lm_obj object
#'
#' @examples LM_df + add_lm_markers(size="GC.", col="Genes", min=10, max=80, FUN="mean", pal="Accent")
"+.lifemap.obj" <- function(e1,e2) {
  if(is.lm_markers(e2)) {
    if(is.null(e1$aes)) {
      e1$aes <- e2
    } else {
      e1$aes <- dplyr::bind_rows(e1$aes,e2)
    }
  }
  return(e1)
}

# initialise the add_Lifemap_markers function
# draw_markers <- function(lm_obj,...){
#   return(lm_obj)
# }
