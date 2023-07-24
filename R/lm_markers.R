#' add a "markers" layer to a lifemap_obj object.
#'
#' @description
#' Adds a layer with circles that can represent data. The main parameters that can be used to represent data are the following:
#' - radius
#' - var_fillColor
#' - var_color (stroke's color)
#'
#'
#' @param data A sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used.
#' @param radius Either a numerical value or a column name of the original dataframe to represent this variable by the size of markers.
#' @param min An integer indicating the minimal size of the markers if radius is a column name.
#' @param max An integer indicating the maximal size of the markers if radius is a column name.
#' @param fillColor Either a color or a palette if the fillColor is represented by a variable.
#' @param var_fillColor A column name of the original dataframe to represent this variable by the fillColor of markers.
#' @param fillOpacity A numeric indicating the fill opacity.
#' @param stroke A logical indicating whether to draw a border for markers.
#' @param color The stroke color. Either a color or a palette if the stroke color is represented by a variable.
#' @param var_color A column name of the original dataframe to represent this variable by the stroke color.
#' @param weight The stroke width in pixels.
#' @param opacity The stroke opacity.
#' @param legend Whether to display the legend or not.
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft").
#' @param legendOrientation c("vertical", "horizontal").
#' @param legendOpacity Legend opacity (applies on the shapes in the legend, not the background itself).
#' @param FUN The function to be applied to infer values. If NULL then the information missing from the parent nodes won't be inferred.
#' @param display c("auto", "requested", "all", "leaves"), a string indicating how to display markers :
#' - "auto" : the markers are displayed depending on the zoom level, by default, allow to have a lot of markers
#' - "requested" : only displays the requested taxa, but all at the same time
#' - "all" : displays all the taxa including all the ancestors to the root
#' - "leaves" : displays only the latest (most recent) taxa
#'
#' (WARNING : "requested", "leaves" and "auto" shouldn't be used to display more than 2000 markers as it may result in long computing time).
#'
#' @param popup A column name indicating what to display when clicking on a node.
#' @param label A column name indicating what to display when hovering on a node.
#'
#' @return An lm_markers object containing all aesthetics details for one layer of markers.
#' @export
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' lm_markers(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ])
#'
#' lm_markers(radius = "GC.", var_fillColor = "Genes")
#'
lm_markers <- function(data = NULL,
                       radius = 10,
                       min = 10,
                       max = 40,
                       fillColor = NULL,
                       var_fillColor = NULL,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       color = NULL,
                       var_color = NULL,
                       weight = 1,
                       opacity = 0.8,
                       legend = TRUE,
                       legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
                       legendOrientation = c("vertical", "horizontal"),
                       legendOpacity = 0.5,
                       FUN = NULL,
                       display = c("auto", "requested", "all", "leaves"),
                       popup = NULL,
                       label = NULL) {
  legendPosition <- match.arg(legendPosition)
  legendOrientation <- match.arg(legendOrientation)
  display <- match.arg(display)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- NULL}

  if (is.null(var_fillColor)){
    var_fillColor <- "default"
  }
  fillPalette <- NULL
  if (var_fillColor %in% "default") {
    if (is.null(fillColor)) {
      fillColor <- "red"
    }
  } else {
    if (is.null(fillColor)) {
      fillPalette <- "RdBu"
    } else { fillPalette <- fillColor }
  }

  if (is.null(var_color)){
    var_color <- "default"
  }
  palette <- NULL
  if (var_color %in% "default") {
    if (is.null(color)) {
      color <- "black"
    }
  } else {
    if (is.null(color)) {
      palette <- "RdBu"
    } else { palette <- color }
  }

  value <- NULL
  if (is.numeric(radius)){
    value <- radius
    radius <- "default"
  }

  res <- list(taxids = taxids, radius = radius, value = value,
              fillColor = fillColor, fillPalette = fillPalette, min = min,
              max = max, FUN = FUN, stroke = stroke,
              color = color, palette = palette, weight = weight,
              opacity = opacity, fillOpacity = fillOpacity,
              var_fillColor = var_fillColor, var_color = var_color,
              legend = legend, legendPosition = legendPosition,
              legendOrientation = legendOrientation, legendOpacity = legendOpacity,
              display = display, popup = popup, label = label)
  class(res)=c("lifemap_obj", "lm_markers", "list")
  return(res)
}

#' Reports whether x is a lm_markers object.
#' @param x The object to test.
#' @return A boolean indicating whether or not the object is of lm_markers type.
#' @export
is.lm_markers <- function(x) {inherits(x, "lm_markers")}
