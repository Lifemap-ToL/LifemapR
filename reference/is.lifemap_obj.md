# Reports whether x is a lifemap_obj object.

Reports whether x is a lifemap_obj object.

## Usage

``` r
is.lifemap_obj(x)
```

## Arguments

- x:

  The object to test.

## Value

A boolean indicating whether or not the object is of lifemap_obj type.

## Examples

``` r
## Only run examples in interactive R sessions
if (interactive()) {
data(LM_eukaryotes)
is.lifemap_obj(LM_eukaryotes)
}
```
