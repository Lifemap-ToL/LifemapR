#' Create a Lifemap base
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#' if a dataframe is provided, it will be used for the creation of the map object
#'
#' @param map The base choose to display, either \itemize{
#' - 'fr' for \url{https://lifemap-fr.univ-lyon1.fr/}
#' - 'ncbi' for \url{https://lifemap-ncbi.univ-lyon1.fr/}
#' - 'base' for \url{https://lifemap.univ-lyon1.fr/}
#' - 'virus' for \url{https://virusmap.univ-lyon1.fr/} }
#'
#' @return HTML widget object  with graphics layers
#' @export
#' @importFrom leaflet leaflet addTiles providerTileOptions
#'
#' @examples display_map("fr")
display_map <- function(df=NULL,basemap = c("fr","ncbi", "base","virus")) {
  basemap <- match.arg(basemap)
  if (basemap == "fr"){
    display="http://lifemap-fr.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "ncbi"){
    display="http://lifemap-ncbi.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "base"){
    display="http://lifemap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "virus"){
    display="http://virusmap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  }
  m <- leaflet::leaflet(df) |>
    leaflet::addTiles(display, options = leaflet::providerTileOptions(minZoom = 5, maxZoom = 50))
  return(m)
}
