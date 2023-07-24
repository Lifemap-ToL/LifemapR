#' Method to print lifemap_obj objects.
#'
#'
#' @param x An lifemap_obj.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Either a description of the dataframe and basemap used for the lm_obj object, or a shiny application if aesthetics are furnished
#' @export
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' data(LM_eukaryotes)
#' print(LM_eukaryotes)
#' }
print.lifemap_obj <- function(x,...) {
  if (is.null(x$aes) && !is.null(x$df)) {
    cat('The dataframe contains', nrow(x$df),'rows and', ncol(x$df), 'columns. \n')
    cat('The basemap used is :', x$basemap,'\n')
  } else if (is.null(x$df)){
    class(x) <- "list"
    print(x)
  } else {
    print(draw_Lifemap(x))
  }

}

#' Reports whether x is a lifemap_obj object.
#' @param x The object to test.
#' @export
#' @return A boolean indicating whether or not the object is of lifemap_obj type.

#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' data(LM_eukaryotes)
#' is.lifemap_obj(LM_eukaryotes)
#' }
#'
is.lifemap_obj <- function(x) inherits(x, "lifemap_obj")

#' Add a graphical element to a tree visualisation.
#' @param e1 An object of class lifemap_obj that contains at least $df, a dataframe, and $basemap, the map used to get the coordinates.
#' @param e2 A description of the graphical features wanted for a set of points (eg. markers, subtree, piecharts, ...).
#' @export
#' @return A lifemap_obj object.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' data(LM_eukaryotes)
#' LM_obj <- lifemap(LM_eukaryotes) + lm_markers() + lm_branches()
#' }
#'
"+.lifemap_obj" <- function(e1,e2) {
  if (is.lm_markers(e2)){
    for (aes in c("radius", "var_fillColor", "var_color", "fillOpacity")){
      if (is.character(e2[[aes]]) && !(e2[[aes]] %in% "default")) {
        e2[[aes]] <- match.arg(arg = e2[[aes]], choices = colnames(e1$df))
      }
    }

  } else if (is.lm_branches(e2)){
    for (aes in c("var_color", "size")){
      if (is.character(e2[[aes]]) && !(e2[[aes]] %in% "default")) {
        e2[[aes]] <- match.arg(arg = e2[[aes]], choices = colnames(e1$df))
      }
    }
  } else if (is.lm_piecharts(e2)) {
    e2$param <- match.arg(arg = e2$param, choices = colnames(e1$df))
  }

  if(is.null(e1$aes)) {
    e1$aes <- list(e2)
  } else {
    e1$aes <- append(e1$aes, list(e2))
  }
  return(e1)
}
