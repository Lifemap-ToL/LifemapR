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
      var solrUrl = 'https://lifemap-back.univ-lyon1.fr/solr/taxo/select';

      map.getContainer().style.backgroundColor = '#000000';

      var labelStyle = document.createElement('style');
      labelStyle.textContent = '.lifemap-taxon-label { background: transparent; border: 0; white-space: nowrap; pointer-events: none; }' +
        '.lifemap-taxon-label .name { color: rgba(255, 255, 255, 1); font-family: Segoe UI, Helvetica, sans-serif; font-weight: 400; line-height: 1.05; text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000; }';
      document.head.appendChild(labelStyle);

      map.createPane('lifemap-polygons');
      map.getPane('lifemap-polygons').style.zIndex = 200;
      map.createPane('lifemap-branches');
      map.getPane('lifemap-branches').style.zIndex = 300;

      var polygonsLayer = L.vectorGrid.protobuf(tileUrl, {
        pane: 'lifemap-polygons',
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
            var zoomLevel = Number(properties.zoomview);
            var opacityFactor = Number.isFinite(zoomLevel) ?
              1 - Math.abs(zoom - zoomLevel - 1) / 5 : 1;
            return {fill: true, stroke: false, weight: 0, fillColor: style.fillColor,
              fillOpacity: Math.max(0, style.fillOpacity * opacityFactor), opacity: 0};
          }
        }
      });

      var branchesLayer = L.vectorGrid.protobuf(tileUrl, {
        pane: 'lifemap-branches',
        rendererFactory: L.canvas.tile,
        maxNativeZoom: 42,
        maxZoom: 42,
        vectorTileLayerStyles: {
          'branches-layer': {
            fill: false, color: '#cacaca', opacity: 1, weight: 0.8
          }
        }
      });
      polygonsLayer.addTo(map);
      branchesLayer.addTo(map);
      map._lifemapVectorLayer = L.layerGroup([polygonsLayer, branchesLayer]);

      var labelsLayer = L.layerGroup().addTo(map);
      var labelsRequest = 0;

      function first(value) {
        return Array.isArray(value) ? value[0] : value;
      }

      function escapeHtml(value) {
        return String(value == null ? '' : value)
          .replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/\"/g, '&quot;')
          .replace(/'/g, '&#039;');
      }

      function refreshLabels() {
        var currentRequest = ++labelsRequest;
        var bounds = map.getBounds();
        var currentZoom = Math.round(map.getZoom());
        var queryZoom = currentZoom + 3;
        var params = new URLSearchParams({
          q: '*:*',
          fq: 'zoom:[0 TO ' + queryZoom + ']',
          fl: 'sci_name,lon,lat,zoom,common_name_en',
          wt: 'json',
          rows: '1000'
        });
        params.append('fq', 'lat:[' + bounds.getSouth() + ' TO ' + bounds.getNorth() + ']');
        params.append('fq', 'lon:[' + bounds.getWest() + ' TO ' + bounds.getEast() + ']');

        fetch(solrUrl + '?' + params.toString())
          .then(function(response) { return response.json(); })
          .then(function(data) {
            if (currentRequest !== labelsRequest) return;
            labelsLayer.clearLayers();
            var docs = data.response && data.response.docs ? data.response.docs : [];

            docs.forEach(function(doc) {
              var lon = Number(first(doc.lon));
              var lat = Number(first(doc.lat));
              var name = first(doc.sci_name);
              var taxonZoom = Number(first(doc.zoom));
              if (!Number.isFinite(lon) || !Number.isFinite(lat) || !name) return;

              var fontSize = 16 + (currentZoom - taxonZoom) * 3;
              var commonName = first(doc.common_name_en);
              var label = '<span class=\"name\" style=\"font-size:' + fontSize + 'px\">' +
                escapeHtml(name) + (commonName && fontSize > 10 ? '<br><span style=\"font-size:' + Math.max(10, fontSize - 8) + 'px\">' + escapeHtml(commonName) + '</span>' : '') +
                '</span>';

              L.circleMarker([lat, lon], {
                radius: 4,
                fillColor: '#e15759',
                color: '#000',
                weight: 1,
                opacity: 1,
                fillOpacity: 1,
                interactive: false
              }).addTo(labelsLayer);

              L.marker([lat, lon], {
                icon: L.divIcon({
                  className: 'lifemap-taxon-label',
                  html: label,
                  iconSize: null,
                  iconAnchor: [0, 0]
                }),
                interactive: false,
                keyboard: false
              }).addTo(labelsLayer);
            });
          })
          .catch(function(error) {
            console.warn('Lifemap labels could not be loaded:', error);
          });
      }

      var labelTimer;
      map.on('moveend', function() {
        clearTimeout(labelTimer);
        labelTimer = setTimeout(refreshLabels, 100);
      });
      map.whenReady(function() { setTimeout(refreshLabels, 0); });
      map._lifemapLabelsLayer = labelsLayer;
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
