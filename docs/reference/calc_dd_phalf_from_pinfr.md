# Calculate 'q' from 'p' for the distance-decay model

Calculate 'q' from 'p' for the distance-decay model

## Usage

``` r
calc_dd_phalf_from_pinfr(pinfr)
```

## Arguments

- pinfr:

  A vector of numeric values containing the values of 'pinfr'

## Value

A vector of length `length(pinfr)` containing the values of `phalf`
associated with `pinfr`

## Details

"pinfr": the probability of being within the foraging range; "phalf":
the proportion of the foraging range within which half of the UD lies
Mathematical derivation: Define q to be such that:
`[1 - exp(-beta * fr * phalf)] / [1 - exp(-beta * fr)] = 1/2` Since:
`beta = −log(1 − pinfr) / fr` We can immediately derive:
`phalf = log(1 - (pinfr/2)) / log(1 - pinfr)`
