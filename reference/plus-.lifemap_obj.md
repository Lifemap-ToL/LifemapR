# Add a graphical element to a tree visualisation.

Add a graphical element to a tree visualisation.

## Usage

``` r
# S3 method for class 'lifemap_obj'
e1 + e2
```

## Arguments

- e1:

  An object of class lifemap_obj that contains at least \$df, a
  dataframe, and \$basemap, the map used to get the coordinates.

- e2:

  A description of the graphical features wanted for a set of points
  (eg. markers, subtree, piecharts, ...).

## Value

A lifemap_obj object.

## Examples

``` r
## Only run examples in interactive R sessions
if (interactive()) {
data(LM_eukaryotes)
LM_obj <- lifemap(LM_eukaryotes) + lm_markers() + lm_branches()
}
```
