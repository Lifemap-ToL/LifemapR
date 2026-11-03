# Add a "branches" layer to a lifemap_obj object.

Add a "branches" layer to a lifemap_obj object.

## Usage

``` r
lm_branches(
  data = NULL,
  color = NULL,
  var_color = NULL,
  size = 5,
  min = 2,
  max = 20,
  opacity = 0.5,
  FUN = NULL,
  legend = TRUE,
  legendPosition = c("topright", "bottomright", "bottomleft", "topleft")
)
```

## Arguments

- data:

  A sub dataset to use, if NULL then all of the taxids from the lifemap
  object given to lifemap() will be used.

- color:

  Either a color for the branches or a palette if a variable is used to
  represent branches' color.

- var_color:

  A column name of the original dataframe to represent this variable by
  the color of branches.

- size:

  Either a numeric for the branche's thickness or a variable to be
  represented by the branche's thickness.

- min:

  An integer indicating the minimal thickness of the branches if the
  size is a column name.

- max:

  An integer indicating the maximal thickness of the branches if the
  size is a column name.

- opacity:

  An integer indicating branche's opacity.

- FUN:

  The function to be applied to infer values. If NULL values won't be
  inferred

- legend:

  A logical indicating whether or not to display the legend.

- legendPosition:

  c("topright", "bottomright", "bottomleft", "topleft"), the position of
  the legend.

## Value

An lm_branches object containing all aesthetics details for one layer of
branches

## Examples

``` r
data(LM_eukaryotes)

lm_branches(var_color = "GC.", color = "Accent")
#> $taxids
#> NULL
#> 
#> $color
#> [1] "Accent"
#> 
#> $palette
#> [1] "Accent"
#> 
#> $FUN
#> NULL
#> 
#> $var_color
#> [1] "GC."
#> 
#> $value
#> [1] 5
#> 
#> $size
#> [1] "default"
#> 
#> $min
#> [1] 2
#> 
#> $max
#> [1] 20
#> 
#> $opacity
#> [1] 0.5
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 

lm_branches(data = LM_eukaryotes$df[LM_eukaryotes$df$Group %in% "Plants",])
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
#> $color
#> [1] "yellow"
#> 
#> $palette
#> NULL
#> 
#> $FUN
#> NULL
#> 
#> $var_color
#> [1] "default"
#> 
#> $value
#> [1] 5
#> 
#> $size
#> [1] "default"
#> 
#> $min
#> [1] 2
#> 
#> $max
#> [1] 20
#> 
#> $opacity
#> [1] 0.5
#> 
#> $legend
#> [1] TRUE
#> 
#> $legendPosition
#> [1] "topright"
#> 
```
