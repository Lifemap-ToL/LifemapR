#' Create a Lifemap base.
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#' If a dataframe is provided, it will be used for the creation of the map.
#'
#' @param df A dataframe. If given, its columns can be easily accessed with "~" (eg. ~GC.).
#' @param basemap The basemap choosen to be displayed, it can be either :
#' - 'fr' for \url{https://lifemap-fr.univ-lyon1.fr/}
#' - 'ncbi' for \url{https://lifemap-ncbi.univ-lyon1.fr/}
#' - 'base' for \url{https://lifemap.univ-lyon1.fr/}
#' - 'virus' for \url{https://virusmap.univ-lyon1.fr/}
#'
#' @return An HTML widget object  with graphics layers.
#' @export
#' @importFrom leaflet leaflet addTiles providerTileOptions
#' @importFrom RCurl url.exists
#'
#' @examples
#' display_map()
display_map <- function(df = NULL,basemap = c("fr","ncbi", "base","virus")) {
  basemap <- match.arg(basemap)
  if (basemap == "fr"){
    display="http://lifemap-fr.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "ncbi"){
    display="http://lifemap-ncbi.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "base"){
    display="http://lifemap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  } else if (basemap == "virus"){
    display="https://virusmap.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"
  }
  url2check<-strsplit(display, "osm_tiles")[[1]][1]
  if (!url.exists(url2check)) stop ("The Lifemap server or some remote lifemap files cannot be reached. Please try again later.")
  m <- leaflet::leaflet(df) |>
    leaflet::addTiles(display, options = leaflet::providerTileOptions(minZoom = 5, maxZoom = 50))
  return(m)
}
