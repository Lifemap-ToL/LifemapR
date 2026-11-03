# Create a Lifemap base.

This function create a blank Leaflet map using the leaflet function from
the leaflet package. If a dataframe is provided, it will be used for the
creation of the map.

## Usage

``` r
display_map(df = NULL, basemap = NULL)
```

## Arguments

- df:

  A dataframe. If given, its columns can be easily accessed with "~"
  (eg. ~GC.).

- basemap:

  Deprecated argument.

## Value

An HTML widget object with graphics layers.

## Examples

``` r
display_map()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://lifemap-back.univ-lyon1.fr/osm_tiles/{z}/{x}/{y}.png",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false,"minZoom":5,"maxZoom":50}]}]},"evals":[],"jsHooks":[]}
```
