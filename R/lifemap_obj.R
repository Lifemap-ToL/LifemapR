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
  if(is.null(e1$aes)) {
    e1$aes <- list(e2)
  } else {
    e1$aes <- append(e1$aes, list(e2))
  }
  return(e1)
}
