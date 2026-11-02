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
#' @importFrom leaflet leaflet
#' @importFrom htmltools htmlDependency
#' @importFrom htmlwidgets onRender

display_map <- function(df = NULL, basemap = NULL) {
  if (!is.null(basemap)) {
    warning("The basemap argument is now deprecated.")
  }

  # Lifemap no longer serves raster osm_tiles. The current basemap is a
  # Mapbox Vector Tile (MVT) endpoint, rendered client-side by Leaflet.VectorGrid.
  display <- "https://lifemap-back.univ-lyon1.fr/vector_tiles/xyz/composite/{z}/{x}/{y}.pbf"

  vectorgrid_dependency <- htmltools::htmlDependency(
    name = "leaflet-vectorgrid",
    version = "1.3.0",
    src = c(href = "https://unpkg.com/leaflet.vectorgrid@1.3.0/dist"),
    script = "Leaflet.VectorGrid.bundled.js"
  )

  vectorgrid_script <- sprintf(
    "function(el, x) {
      var map = this;
      var tileUrl = %s;
      var vectorLayer = L.vectorGrid.protobuf(tileUrl, {
        rendererFactory: L.canvas.tile,
        maxNativeZoom: 42,
        maxZoom: 42,
        vectorTileLayerStyles: {
          'poly-layer': function(properties, zoom) {
            var colors = {
              1: {fillColor: '#aaffee', fillOpacity: 0.20},
              2: {fillColor: '#6599ff', fillOpacity: 0.23},
              3: {fillColor: '#ff8080', fillOpacity: 0.18}
            };
            var style = colors[properties.ref] || colors[3];
            return {fill: true, stroke: false, weight: 0, fillColor: style.fillColor,
              fillOpacity: style.fillOpacity, opacity: 0};
          },
          'branches-layer': {
            fill: false, color: '#cacaca', opacity: 1, weight: 0.8
          },
          'ranks-layer': {
            fill: false, color: '#aaddee', opacity: 0.85, weight: 1
          }
        }
      });
      vectorLayer.addTo(map);
      map._lifemapVectorLayer = vectorLayer;
    }",
    jsonlite::toJSON(display, auto_unbox = TRUE)
  )

  map <- leaflet::leaflet(
    df,
    options = leaflet::leafletOptions(minZoom = 4, maxZoom = 42)
  )
  map$dependencies <- c(map$dependencies, list(vectorgrid_dependency))
  htmlwidgets::onRender(map, vectorgrid_script)
}
