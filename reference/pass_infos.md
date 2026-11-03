# Infer numerical values to nodes.

Infer numerical values to nodes.

## Usage

``` r
pass_infos(M, FUN, value)
```

## Arguments

- M:

  The dataframe returned by create_matrix.

- FUN:

  The function to be applied when inferring the values.

- value:

  The column name to which the function applies.

## Value

An array of values.

## Examples

``` r
data(LM_eukaryotes)

infos <- create_matrix(LM_eukaryotes$df, c("GC.", "Genes"))

inferred_values <- pass_infos(M = infos, FUN = mean, value = "GC.")
```
