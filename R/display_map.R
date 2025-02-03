#' Create a Lifemap base.
#'
#' @description This function create a blank Leaflet map using the leaflet function from the leaflet package.
#' If a dataframe is provided, it will be used for the creation of the map.
#'
#' @param df A dataframe. If given, its columns can be easily accessed with "~" (eg. ~GC.).
#' @param basemap Deprecated argument.
#'
#' @return An HTML widget object with graphics layers.
#' @export
#' @importFrom leaflet leaflet addTiles providerTileOptions
#' @importFrom RCurl url.exists
#'
#' @examples
#' display_map()
display_map <- function(df = NULL, basemap = NULL) {
  if (!is.null(basemap)) {
    warning("The basemap argument is now deprecated.")
  }
  display <- "https://lifemap-back.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png"

  m <- tryCatch(
    {
      leaflet::leaflet(df) |>
        leaflet::addTiles(display, options = leaflet::providerTileOptions(minZoom = 5, maxZoom = 50))
    },
    warning = function(w) {
      message("The Lifemap server or some remote lifemap files cannot be reached. Please try again later.")
      return(NA)
    },
    error = function(e) {
      message("The Lifemap server or some remote lifemap files cannot be reached. Please try again later.")
      return(NA)
    }
  )

  if (!all(is.na(m))) {
    return(m)
  } else {
    return(NA)
  }
}
