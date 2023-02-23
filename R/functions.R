#' Create a Lifemap base
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#' if a dataframe is provided, it will be used for the creation of the map object
#'
#' @param map The base choose to display, either \itemize{
#' - 'fr' for \url{https://lifemap-fr.univ-lyon1.fr/}
#' - 'ncbi' for \url{https://lifemap-ncbi.univ-lyon1.fr/} }
#'
#' @return HTML widget object  with graphics layers
#' @export
#'
#' @examples display_map("fr")
display_map <- function(df=NULL,map="ncbi") {
  if (map == "fr"){
    display="http://lifemap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (map == "ncbi"){
    display="http://lifemap-ncbi.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
    }
  m <- leaflet::leaflet(df) %>%
    leaflet::addTiles(display)
  return(m)
}
