#' Add a "piecharts" layer to a lifemap_obj object.
#'
#' @param data A sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used.
#' @param param A string indicating the discret variable to be represented.
#' @param type The type of chart to draw.
#' @param width The maximal width of the charts.
#' @param height The maximal height of the charts.
#' @param opacity The chart's opacity.
#' @param showLabels A boolean indicating whether to display the values directly on the chart or not.
#' @param pal The palette to be used for the charts.
#' @param legend Whether to draw the legend or not.
#' @param legendPosition c("topright", "bottomright", "bottomleft", "topleft"). Where should the legend be placed.
#' @param display c("auto", "requested", "all", "leaves"), a string indicating how to display points :
#' - "auto" : the markers are displayed depending on the zoom, by default, allow to have a lot of points
#' - "requested" : only display the requested taxa, but all at the same time
#' - "all" : display all the taxa including all the ancestors to the root
#' - "leaves" : display only the last (most recent) taxa
#'
#' @return A lifemap object.
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
                       type = c("pie","bar", "polar-area", "polar-radius", "auto"),
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
  } else { taxids <- NULL}

  res <- list(taxids = taxids, param = param,
              type = type, width = width, opacity = opacity,
              showLabels = showLabels, pal = pal,
              legend = legend, legendPosition = legendPosition, display = display)
  class(res)=c("lifemap_obj", "lm_piecharts", "list")
  return(res)
}

#' Reports whether x is a lm_branches object.
#' @param x The object to test.
#' @export
is.lm_piecharts <- function(x) {inherits(x, "lm_piecharts")}
