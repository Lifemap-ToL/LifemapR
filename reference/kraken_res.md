# Kraken results

A dataset containing NCBI information of 1000 eukaryotes. The variables
are as follows:

## Usage

``` r
data(kraken_res)
```

## Format

A data frame with 4427 rows and 6 variables:

- coverage_percent:

  Percentage of fragments covered by the clade rooted at this taxon

- coverage_number:

  Number of fragments covered by the clade rooted at this taxon

- fragment_number:

  Number of fragments assigned directly to this taxon

- rank:

  A rank code, indicating (U)nclassified, (R)oot, (D)omain, (K)ingdom,
  (P)hylum, (C)lass, (O)rder, (F)amily, (G)enus, or (S)pecies. Taxa that
  are not at any of these 10 ranks have a rank code that is formed by
  using the rank code of the closest ancestor rank with a number
  indicating the distance from that rank. E.g., "G2" is a rank code
  indicating a taxon is between genus and species and the grandparent
  taxon is at the genus rank.

- taxid:

  NCBI taxonomic ID number

- name:

  Indented scientific name
