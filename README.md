[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/LifemapR)](https://cran.r-project.org/package=LifemapR)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/LifemapR)](https://cran.r-project.org/package=LifemapR)
[![R-CMD-check](https://github.com/Lifemap-ToL/LifemapR/workflows/R-CMD-check/badge.svg)](https://github.com/Lifemap-ToL/LifemapR/actions)

# LifemapR <img src="man/figures/lifemapr-logo.png" align="right" style="float:right; width:20%;"/>

An R package to visualise data on a Lifemap base (https://lifemap-ncbi.univ-lyon1.fr/)

## Installation

To install the development version of <code>LifemapR</code> from GitHub :

```r
remotes::install_github("damiendevienne/LifemapR")
```

Once installed, load the package with :

```r
require("LifemapR")
```

## Usage

Here is a brief introduction on how to use LifemapR.

1. With the `build_Lifemap` function transform your already existing data into a format usable by LifemapR functions

```r
data(eukaryotes_1000)

# Construction of a LifemapR usable dataframe
LM_obj <- LifemapR::build_Lifemap(eukaryotes_1000)
```

After the `build_Lifemap` function the result is a LifemapR format containing a dataframe :

```r
full_df <- LM_obj$df
```

2. Then you can display a map with wanted informations by calling one ore more `LifemapR` functions.
   Note that with the `LifemapR` functions, a `shiny` application will be launched

```r
# Initialise a visualisation for LM_obj
lifemap(LM_obj) +
    # adding a subtree with colored branches
    LifemapR::lm_branches(var_col = "Protein", FUN = mean, col = "PiYG")+
    # adding a set of points
    LifemapR::lm_markers(radius = "GC.", var_fillColor = "Genes", FUN = mean)
```

## Development

To do list :

-   [ ] New protocole to fetch data with improved databases (parquet format)
-   [ ] Improve create_matrix() to use less CPU (merge before joining all lists)
-   [ ] Improve make_newick() to make it recursive
-   [ ] Improve the shiny application version

-   [x] Redo pass_info function to only infer unknown values and only from the value of direct ancestors
-   [x] Implement popups and labels for markers
-   [ ] Implement popups and labels for polylines
-   [x] Black background on leaflet
-   [x] Repair the size legend
-   [x] Option to thicker the line depending on a variable
-   [x] Improve create_matrix function to pass check

### How to use during development

Go to the package's folder

```r
require(devtools)
devtools::load_all()
```

then you can simply use the package's functions
