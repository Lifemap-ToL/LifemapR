# Infer discret values to nodes for lm_piecharts function.

Infer discret values to nodes for lm_piecharts function.

## Usage

``` r
pass_infos_discret(M, value)
```

## Arguments

- M:

  The dataframe returned by create_matrix.

- value:

  The column name represented as piecharts.

## Value

A dataframe containing the TaxIDs and as many columns as there are
distinct values.

## Examples

``` r
data(LM_eukaryotes)

infos <- create_matrix(LM_eukaryotes$df, "Status")

inferred_values <- pass_infos_discret(M = infos, value = "Status")
```
