# Compute the aesthetics for a subtree visualisation.

Compute the aesthetics for a subtree visualisation.

## Usage

``` r
add_lm_branches(
  proxy,
  aes,
  df,
  df_visible,
  df_descendants,
  group_info,
  all_taxids
)
```

## Arguments

- proxy:

  The map to be modified.

- aes:

  The dataframe containing the aesthetics details (must be of
  lm_branches class).

- df:

  The full dataframe.

- df_visible:

  The dataframe containing visible taxa.

- df_descendants:

  The dataframe containing all the information on the descendants of
  visible taxa.

- group_info:

  the ID of this group of lines.

- all_taxids:

  A vector containing all the visible taxids and their direct
  descendants.

## Value

An updated map with the new layer added.
