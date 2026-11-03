# Add a "piecharts" layer to a lifemap_obj object.

Add a "piecharts" layer to a lifemap_obj object.

## Usage

``` r
lm_piecharts(
  data = NULL,
  param,
  type = c("pie", "bar", "polar-area", "polar-radius", "auto"),
  width = 30,
  height = 30,
  opacity = 1,
  showLabels = FALSE,
  pal = "Accent",
  legend = TRUE,
  legendPosition = c("topright", "bottomright", "bottomleft", "topleft"),
  display = c("auto", "requested", "all", "leaves")
)
```

## Arguments

- data:

  A sub dataset to use, if NULL then all of the taxids from the lifemap
  object given to lifemap() will be used.

- param:

  A column name indicating the discret variable to be represented.

- type:

  The type of chart to draw.

- width:

  The maximal width of the charts.

- height:

  The maximal height of the charts.

- opacity:

  The chart's opacity.

- showLabels:

  A boolean indicating whether to display the values directly on the
  chart or not.

- pal:

  The palette to be used for the charts.

- legend:

  A boolean indiacting whether to draw the legend or not.

- legendPosition:

  c("topright", "bottomright", "bottomleft", "topleft"). Where should
  the legend be placed.

- display:

  c("auto", "requested", "all", "leaves"), a string indicating how to
  display charts :

  - "auto" : the markers are displayed depending on the zoom level, by
    default, allow to have a lot of charts

  - "requested" : only displays the requested taxa, but all at the same
    time

  - "all" : displays all the taxa including all the ancestors to the
    root

  - "leaves" : displays only the latest (most recent) taxa

  (WARNING : "requested", "leaves" and "auto" shouldn't be used to
  display more than 2000 charts as it may result in long computing time)

## Value

An lm_piecharts object containing all aesthetics details for one layer
of charts

## Examples

``` r
data(LM_eukaryotes)

lm_piecharts(param = "Status")
#> $taxids
#> NULL
#> 
#> $param
#> [1] "Status"
#> 
#> $type
#> [1] "pie"
#> 
#> $width
#> [1] 30
#> 
#> $opacity
#> [1] 1
#> 
#> $showLabels
#> [1] FALSE
#> 
#> $pal
#> [1] "Accent"
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 
#> $display
#> [1] "auto"
#> 

lm_piecharts(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants", ], param = "Status")
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
#> $param
#> [1] "Status"
#> 
#> $type
#> [1] "pie"
#> 
#> $width
#> [1] 30
#> 
#> $opacity
#> [1] 1
#> 
#> $showLabels
#> [1] FALSE
#> 
#> $pal
#> [1] "Accent"
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 
#> $display
#> [1] "auto"
#> 
```
