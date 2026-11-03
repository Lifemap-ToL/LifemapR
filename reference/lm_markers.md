# add a "markers" layer to a lifemap_obj object.

Adds a layer with circles that can represent data. The main parameters
that can be used to represent data are the following:

- radius

- var_fillColor

- var_color (stroke's color)

## Usage

``` r
lm_markers(
  data = NULL,
  radius = 10,
  min = 10,
  max = 40,
  fillColor = NULL,
  var_fillColor = NULL,
  fillOpacity = 0.8,
  stroke = FALSE,
  color = NULL,
  var_color = NULL,
  weight = 1,
  opacity = 0.8,
  legend = TRUE,
  legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
  legendOrientation = c("vertical", "horizontal"),
  legendOpacity = 0.5,
  FUN = NULL,
  display = c("auto", "requested", "all", "leaves"),
  popup = NULL,
  label = NULL
)
```

## Arguments

- data:

  A sub dataset to use, if NULL then all of the taxids from the lifemap
  object given to lifemap() will be used.

- radius:

  Either a numerical value or a column name of the original dataframe to
  represent this variable by the size of markers.

- min:

  An integer indicating the minimal size of the markers if radius is a
  column name.

- max:

  An integer indicating the maximal size of the markers if radius is a
  column name.

- fillColor:

  Either a color or a palette if the fillColor is represented by a
  variable.

- var_fillColor:

  A column name of the original dataframe to represent this variable by
  the fillColor of markers.

- fillOpacity:

  A numeric indicating the fill opacity.

- stroke:

  A logical indicating whether to draw a border for markers.

- color:

  The stroke color. Either a color or a palette if the stroke color is
  represented by a variable.

- var_color:

  A column name of the original dataframe to represent this variable by
  the stroke color.

- weight:

  The stroke width in pixels.

- opacity:

  The stroke opacity.

- legend:

  Whether to display the legend or not.

- legendPosition:

  c("topright", "bottomright", "bottomleft", "topleft").

- legendOrientation:

  c("vertical", "horizontal").

- legendOpacity:

  Legend opacity (applies on the shapes in the legend, not the
  background itself).

- FUN:

  The function to be applied to infer values. If NULL then the
  information missing from the parent nodes won't be inferred.

- display:

  c("auto", "requested", "all", "leaves"), a string indicating how to
  display markers :

  - "auto" : the markers are displayed depending on the zoom level, by
    default, allow to have a lot of markers

  - "requested" : only displays the requested taxa, but all at the same
    time

  - "all" : displays all the taxa including all the ancestors to the
    root

  - "leaves" : displays only the latest (most recent) taxa

  (WARNING : "requested", "leaves" and "auto" shouldn't be used to
  display more than 2000 markers as it may result in long computing
  time).

- popup:

  A column name indicating what to display when clicking on a node.

- label:

  A column name indicating what to display when hovering on a node.

## Value

An lm_markers object containing all aesthetics details for one layer of
markers.

## Examples

``` r
data(LM_eukaryotes)

lm_markers(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ])
#> $taxids
#> [[1]]
#>  [1] "2711"    "3055"    "3077"    "3088"    "3352"    "3435"    "3555"   
#>  [8] "3635"    "3641"    "3668"    "3702"    "3708"    "3710"    "3712"   
#> [15] "3726"    "3760"    "3823"    "3847"    "3871"    "3880"    "3888"   
#> [22] "3915"    "4002"    "4072"    "4083"    "4097"    "4113"    "4513"   
#> [29] "4516"    "4529"    "4530"    "4577"    "4679"    "13288"   "13419"  
#> [36] "29656"   "34315"   "34316"   "38846"   "39905"   "39919"   "39946"  
#> [43] "39955"   "44586"   "49829"   "53169"   "55890"   "57919"   "59691"  
#> [50] "69181"   "80294"   "80754"   "83906"   "85692"   "96569"   "102169" 
#> [57] "109997"  "110450"  "112509"  "122812"  "123599"  "126366"  "128810" 
#> [64] "130453"  "157748"  "160369"  "176864"  "178133"  "210143"  "228173" 
#> [71] "296587"  "310722"  "314410"  "357465"  "462830"  "586396"  "586722" 
#> [78] "1000413" "1010633" "1042139" "1381980" "1711249" "1862640" "1985974"
#> [85] "2303040" "2589997" "2758712" "2905994"
#> 
#> 
#> $radius
#> [1] "default"
#> 
#> $value
#> [1] 10
#> 
#> $fillColor
#> [1] "red"
#> 
#> $fillPalette
#> NULL
#> 
#> $min
#> [1] 10
#> 
#> $max
#> [1] 40
#> 
#> $FUN
#> NULL
#> 
#> $stroke
#> [1] FALSE
#> 
#> $color
#> [1] "black"
#> 
#> $palette
#> NULL
#> 
#> $weight
#> [1] 1
#> 
#> $opacity
#> [1] 0.8
#> 
#> $fillOpacity
#> [1] 0.8
#> 
#> $var_fillColor
#> [1] "default"
#> 
#> $var_color
#> [1] "default"
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 
#> $legendOrientation
#> [1] "vertical"
#> 
#> $legendOpacity
#> [1] 0.5
#> 
#> $display
#> [1] "auto"
#> 
#> $popup
#> NULL
#> 
#> $label
#> NULL
#> 

lm_markers(radius = "GC.", var_fillColor = "Genes")
#> $taxids
#> NULL
#> 
#> $radius
#> [1] "GC."
#> 
#> $value
#> NULL
#> 
#> $fillColor
#> NULL
#> 
#> $fillPalette
#> [1] "RdBu"
#> 
#> $min
#> [1] 10
#> 
#> $max
#> [1] 40
#> 
#> $FUN
#> NULL
#> 
#> $stroke
#> [1] FALSE
#> 
#> $color
#> [1] "black"
#> 
#> $palette
#> NULL
#> 
#> $weight
#> [1] 1
#> 
#> $opacity
#> [1] 0.8
#> 
#> $fillOpacity
#> [1] 0.8
#> 
#> $var_fillColor
#> [1] "Genes"
#> 
#> $var_color
#> [1] "default"
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 
#> $legendOrientation
#> [1] "vertical"
#> 
#> $legendOpacity
#> [1] 0.5
#> 
#> $display
#> [1] "auto"
#> 
#> $popup
#> NULL
#> 
#> $label
#> NULL
#> 
```
