# LifemapR

## Overview

An R package to visualise datas on a Lifemap base (https://lifemap-ncbi.univ-lyon1.fr/) 

## Installation


## Usage

Start with ```construct_dataframe(df)``` with a dataset containing at least a ```"taxid"``` column to create a dataframe you will then be able to use with the package's functions.

Then, you can create the first layer of the map with ```display_map(df)```.

```r
df <- data.frame("taxid"=c(3641,3649,403667,3394,54308,1902823),
                "info1"=c(3,3,4,3,5,1))

Lifemap_df <- construct_dataframe(df,"ncbi")

display_map(Lifemap_df)
```

After that, you can add layers (with %>%) by calling either leaflet functions for simple representations or LifemapR functions for more complicated operations

```r
# add markers to the coordinates of requested taxids
display_map(Lifemap_df[Lifemap_df$type == "r",]) %>% 
addMarkers(~lon, ~lat, label=~sci_name)
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
then simply uses the package's functions