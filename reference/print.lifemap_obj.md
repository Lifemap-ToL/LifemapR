# Method to print lifemap_obj objects.

Method to print lifemap_obj objects.

## Usage

``` r
# S3 method for class 'lifemap_obj'
print(x, ...)
```

## Arguments

- x:

  An lifemap_obj.

- ...:

  Further arguments passed to or from other methods.

## Value

Either a description of the dataframe and basemap used for the lm_obj
object, or a shiny application if aesthetics are furnished

## Examples

``` r
## Only run examples in interactive R sessions
if (interactive()) {
data(LM_eukaryotes)
print(LM_eukaryotes)
}
```
