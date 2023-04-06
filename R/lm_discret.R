#' add a layer to a Lifemap object
#'
#' @param param the discret variable to be represented
#' @param type the type of chart to draw
#' @param width the maximal width of the charts
#' @param height the maximal height of the charts
#' @param opacity the chart's opacity
#' @param showLabels a boolean indicating whether to display the values directly on the chart or not
#' @param pal the palette to be used for the charts
#' @param legend whether to draw the legend or not
#' @param legendPosition where should the legend be placed
#'
#' @return a lifemap object
#' @export
#'
#' @examples lm_discret(param="Status")
#'
#' # to apply it only on a subdataset
#' lm_discret(data = LM$df[LM$df$Group %in% "Fungi",], param = "Status")
lm_discret <- function(data = NULL,
                       param,
                       type = c("pie","bar", "polar-area","polar-radius", "auto"),
                       width = 30,
                       height = 30,
                       opacity = 1,
                       showLabels = FALSE,
                       pal = "Accent",
                       legend=TRUE,
                       legendPosition = c("topright", "bottomright", "bottomleft", "topleft")) {
  type <- match.arg(arg = type, choices = type)
  legendPosition <- match.arg(arg = legendPosition, choices = legendPosition)

  if (!(is.null(data))) {
    taxids <- I(list(c(data$taxid)))
  } else { taxids <- ""}

  res <- data.frame(taxids = taxids, param, type, width, opacity,
                    showLabels, pal, legend, legendPosition)
  class(res)=c("lifemap_obj", "lm_discret", "data.frame")
  return(res)
}

#' Reports whether x is a lm_branches object
#' @param x the object to test
#' @export
is.lm_discret <- function(x) {inherits(x, "lm_discret")}


# pass_infos_discret <- function(df, information) {
#   # separate leaves from nodes
#   all_ancestors <- unique(unlist(df[df$type=="requested",]$ascend))
#   tax_request <- as.vector(df[df$type=="requested",]$taxid)
#   only_leaves <- setdiff(tax_request,all_ancestors)
#
#   # fill the matrix (the value of each leaf is added to all of it's ancestors)
#   info <- data.frame(ancestors=all_ancestors)
#   for (id in only_leaves) {
#     info_tmp <- data.frame(unlist(df[df$taxid==id,"ascend"]),df[df$taxid==id,information])
#     colnames(info_tmp) <- c("ancestors", as.character(id))
#
#     info <- left_join(info,info_tmp, by="ancestors")
#   }
#
#   apply(info, MARGIN = 1,FUN = function(x) {
#     print(table(x))
#   })
#
# }
# pass_infos_discret(LM$df, "Status")
