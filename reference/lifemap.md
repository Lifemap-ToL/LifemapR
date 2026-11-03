# Initialise a new Lifemap visualisation.

Initialise a lifemap_obj object. It can be used to describe aesthetics
like the zoom level at which taxids becomes visible...

## Usage

``` r
lifemap(lm_obj, zoom = 4)
```

## Arguments

- lm_obj:

  lifemap_obj object used for data visualisation.

- zoom:

  The level of zoom for which values are displayed (if zoom = 0 only the
  nodes at the current zoom level or lower can have their values
  displayed).

## Value

A lifemap_obj object.
