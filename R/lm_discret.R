#' add a layer to a Lifemap object
#'
#' @param data a sub dataset to use, if NULL then all of the taxids from the lifemap object given to lifemap() will be used
#' @param param a string indicating the discret variable to be represented
#' @param type the type of chart to draw
#' @param width the maximal width of the charts
#' @param height the maximal height of the charts
#' @param opacity the chart's opacity
#' @param showLabels a boolean indicating whether to display the values directly on the chart or not
#' @param pal the palette to be used for the charts
#' @param legend whether to draw the legend or not
#' @param legendPosition where should the legend be placed
#' @param display c("auto", "requested", "all", "leaves"), a string indicating how to display points :
#' - "auto" : the markers are displayed depending on the zoom, by default, allow to have a lot of points
#' - "requested" : only display the requested taxa, but all at the same time
#' - "all" : display all the taxa including all the ancestors to the root
#' - "leaves" : display only the last (most recent) taxa
#'
#' @return a lifemap object
#' @export
#'
#' @examples
#' data(LM_eukaryotes)
#'
#' lm_discret(param = "Status")
#'
#' lm_discret(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ], param = "Status")
#'
lm_discret <- function(data = NULL,
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
  class(res)=c("lifemap_obj", "lm_discret", "list")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_discret <- function(x) {inherits(x, "lm_discret")}
