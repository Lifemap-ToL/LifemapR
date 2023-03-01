# LifemapR

## Overview

An R package to visualise datas on a Lifemap base (https://lifemap-ncbi.univ-lyon1.fr/) 

## Installation


## Usage

Here is a brief introduction on how to use LifemapR.

1. With ```construct_dataframe``` function transform your already existing datas intos a format usable by LifemapR functions

```r
df <- data.frame("taxid"=c(3641,3649,403667,3394,54308,1902823),"info1"=c(3,3,4,3,5,1))

# Construction of a LifemapR usable dataframe
LM_df <- LifemapR::construct_dataframe(df)
```
2. Then you can display a map with wanted informations either by calling one of ```LifemapR``` functions or by calling ```leaflet``` functions \
Note that with the ```LifemapR``` functions, a ```shiny``` application will be launched

```r
# A LifemapR function
LifemapR::draw_subtree(LM_df)


# a leaflet function
# Note that even when using leaflet functions, you need to use the LifemapR display_map function
LifemapR::display_map(LM_df[LM_df$type == "requested",]) %>% 
    addMarkers(~lon,~lat,label=~sci_name)

map <- LifemapR::display_map(LM_df[LM_df$type == "requested",]) 
leaflet::addMarkers(map, ~lon,~lat,label=~sci_name)
# Two ways of writing the same operation 
```


## Development

Functions : 
- [x] Display The Lifemap base (choice between 'fr' and 'ncbi') -> display_map()
- [x] Transform the given dataframe to a format usable by future functions -> construct_dataframe() and get_full_ancestry()  
- [ ] Represent continuous datas (like GC-content) with Shiny -> continuous_datas()
- [ ] Represent discret datas (like Status in the NCBI data format)
- [ ] Passing information along the branches
- [x] Display the trace between TaxIDs 
    - \+ [ ] generate a Newick subtree


### How to use during development

Go to the package's folder

```r
library(devtools)
devtools::load_all()
```
then you can simply use the package's functions