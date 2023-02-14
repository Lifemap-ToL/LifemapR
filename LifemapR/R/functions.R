#' Create a Lifemap base
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#'
#' @param map The base choose to display, either \itemize{
#' - 'fr' for \url{https://lifemap-fr.univ-lyon1.fr/}
#' - 'ncbi' for \url{https://lifemap-ncbi.univ-lyon1.fr/} }
#'
#' @return HTML widget object  with graphics layers
#' @export
#'
#' @examples display_map("fr")
display_map <- function(map) {
  if (map == "fr"){
    display="http://lifemap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (map == "ncbi"){
    display="http://lifemap-ncbi.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
    }
  leaflet::leaflet() %>%
    leaflet::addTiles(display)
}

