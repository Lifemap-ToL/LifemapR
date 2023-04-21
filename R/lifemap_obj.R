#' print lifemap_obj objects
#'
#'
#' @description When the LifemapR object is just created, give the number of columns and rows
#' as well as the basemap used to get the datas form the database
#'
#' @param lm_obj
#'
#' @return NA
#' @export
#'
#' @examples print(LM_df)
print.lifemap_obj <- function(x) {
  if (is.null(x$aes)) {
    cat('The dataframe contains', nrow(x$df),'rows and', ncol(x$df), 'columns. \n')
    cat('The basemap used is :', x$basemap,'\n')
  } else {
    print(draw_Lifemap(x))
  }

}

#' Reports whether x is a lm_obj object
#' @param x the object to test
#' @export
is.lifemap_obj <- function(x) inherits(x, "lifemap_obj")

#' Add a graphical element to a tree visualisation
#' @param e1 an object of class lm_obj that contains at least $df, a dataframe, and $basemap, the map used to get the coordinates
#' @param e2 a description of the graphical features wanted for a set of points (eg. markers, subtree, popups, ...)
#' @export
#' @return a lm_obj object
#'
#' @examples LM_df + lm_markers(radius="GC.", fillColor="Genes", min=10, max=80, FUN="mean", pal="Accent", legend=TRUE, stroke = TRUE, weight="Size..Mb.")
"+.lifemap_obj" <- function(e1,e2) {
  if (is.lm_markers(e2)){
    if (is.null(e2$var_fillColor)) {
      if (is.null(e2$fillColor)){
        e2$fillColor <- "red"
      }
    } else {
      e2$var_fillColor <- match.arg(arg = e2$var_fillColor, choices = colnames(e1$df))
      if (is.null(e2$fillColor)){
        e2$fillColor <- "Accent"
      }
    }
    if (is.null(e2$var_color)) {
      if (is.null(e2$color)){
        e2$color <- "blue"
      }
    } else {
      e2$var_color <- match.arg(arg = e2$var_color, choices = colnames(e1$df))
      if (is.null(e2$color)){
        e2$color <- "viridis"
      }
    }
    for (aes in c("radius", "opacity", "weight", "fillOpacity")){
      if (is.character(e2[[aes]])) {
        e2[[aes]] <- match.arg(arg = e2[[aes]], choices = colnames(e1$df))
      }
    }
  } else if (is.lm_branches(e2)){
    if (is.null(e2$var_color)) {
      if (is.null(e2$color)){
        e2$color <- "yellow"
      }
    } else {
      e2$var_color <- match.arg(arg = e2$var_color, choices = colnames(e1$df))
      if (is.null(e2$color)){
        e2$color <- "Accent"
      }
    }
  }

  if(is.null(e1$aes)) {
    e1$aes <- list(e2)
  } else {
    e1$aes <- append(e1$aes, list(e2))
  }
  return(e1)
}
