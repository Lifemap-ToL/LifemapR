# A function to construct a LifemapR object, usable by the other functions of the package.

A function to construct a LifemapR object, usable by the other functions
of the package.

## Usage

``` r
build_Lifemap(df, basemap = NULL, verbose = TRUE)
```

## Arguments

- df:

  A dataframe containing at least one column named "taxid" that contains
  NCBI Taxonomy Identifiers (taxid). The dataframe can contain any
  number of additional columns defining traits/characters/values
  associated to each taxid.

- basemap:

  Deprecated argument.

- verbose:

  If TRUE (the default), the function will print detailed information to
  the console. If FALSE, it will run silently.

## Value

A list of class lifemap_obj containing:

- df : a dataframe containing at least for each taxid :

  - The x coordinate (lon)

  - The y coordonate (lat)

  - The scientific name (sci_name)

  - The zoom level at which the taxa is visible (zoom)

  - A list of its ascendants (ascend)

  - Its type ("requested" or "ancestor")

  - Its direct ancestor

  - Its type (type), i.e. whether the taxid was requested by the user
    ("requested") or if it is the anecestor of a requested taxid
    ("ancestor")

- basemap : the basemap used to get taxa's details

## Examples

``` r
data(eukaryotes_80)
if (FALSE) { # \dontrun{
# make sure you have a good internet connection to load these very large files
LM <- build_Lifemap(eukaryotes_80)
} # }
```
