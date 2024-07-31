#' Add a "piecharts" layer to a lifemap_obj object.
#'
#' @param data A sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used.
#' @param param A column name indicating the discret variable to be represented.
#' @param type The type of chart to draw.
#' @param width The maximal width of the charts.
#' @param height The maximal height of the charts.
#' @param opacity The chart's opacity.
#' @param showLabels A boolean indicating whether to display the values directly on the chart or not.
#' @param pal The palette to be used for the charts.
#' @param legend A boolean indiacting whether to draw the legend or not.
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"). Where should the legend be placed.
#' @param display c("auto", "requested", "all", "leaves"), a string indicating how to display charts :
#' - "auto" : the markers are displayed depending on the zoom level, by default, allow to have a lot of charts
#' - "requested" : only displays the requested taxa, but all at the same time
#' - "all" : displays all the taxa including all the ancestors to the root
#' - "leaves" : displays only the latest (most recent) taxa
#' 
#' (WARNING : "requested", "leaves" and "auto" shouldn't be used to display more than 2000 charts as it may result in long computing time)
#' @return An lm_piecharts object containing all aesthetics details for one layer of charts
#' @export
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' lm_piecharts(param = "Status")
#'
#' lm_piecharts(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ], param = "Status")
#'
lm_piecharts <- function(data = NULL,
                       param,
                       type = c("pie", "bar", "polar-area", "polar-radius", "auto"),
                       width = 30,
                       height = 30,
                       opacity = 1,
                       showLabels = FALSE,
                       pal = "Accent",
                       legend = TRUE,
                       legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
                       display = c("auto", "requested", "all", "leaves")) {
  
  type <- match.arg(arg = type, choices = type)
  legendPosition <- match.arg(arg = legendPosition, choices = legendPosition)
  display <- match.arg(display)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else {
    taxids <- NULL
  }

  res <- list(taxids = taxids, param = param,
              type = type, width = width, opacity = opacity,
              showLabels = showLabels, pal = pal,
              legend = legend, legendPosition = legendPosition, display = display)
  class(res) <- c("lifemap_obj", "lm_piecharts", "list")
  return(res)
}


#' Reports whether x is a lm_branches object.
#' @param x The object to test.
#' @return A boolean indicating whether or not the object is of lm_piecharts type.
#' @export
is.lm_piecharts <- function(x) {
  inherits(x, "lm_piecharts")
}
