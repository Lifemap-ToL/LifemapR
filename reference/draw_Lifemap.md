# Represent data on a Lifemap basemap.

Draw a map and all the aesthetics in the order you put them in, the last
one will be on top of the others.

## Usage

``` r
draw_Lifemap(lm_obj)
```

## Arguments

- lm_obj:

  A Lifemap object filled with aesthetics.

## Value

A shiny application

## Examples

``` r
## Only run examples in interactive R sessions
if (interactive()) {
  data(LM_eukaryotes)
  lifemap(LM_eukaryotes) + lm_markers() + lm_branches()
}
```
