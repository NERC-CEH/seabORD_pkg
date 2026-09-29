# Calculate 'pinfr' from 'phalf' for the distance-decay model

Calculate 'pinfr' from 'phalf' for the distance-decay model

## Usage

``` r
calc_dd_pinfr_from_phalf(phalf)
```

## Arguments

- phalf:

  A vector of numeric values containing the values of 'phalf'

## Value

A vector of length `length(q)` containing the values of `pinfr`
associated with `phalf`

## Details

"pinfr": the probability of being within the foraging range; "phalf":
the proportion of the UD within the foraging range that is within half
the foraging range This is optimized numerically
