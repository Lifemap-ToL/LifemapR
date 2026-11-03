# Create a newick for the given dataset.

Create a newick for the given dataset.

## Usage

``` r
make_newick(df)
```

## Arguments

- df:

  A dataframe either enriched with by the build_Lifemap function or
  containing at least two columns :

  - "taxid" containing NCBI format taxids

  - "ancestor" containing for each taxid, it's direct ancestor in the
    phylogeny

## Value

A character string.
