# Set the median prey value across the region

SeabORD uses either uniformly distributed or mapped, variable prey
levels. If uniform, the same value for prey density is used for each
modelled cell in the region. If mapped, there is a median value used to
scale the prey availability. This function generates the median value
for each pair of seasonal runs in the simulation, between two
user-defined values.

## Usage

``` r
set_medianprey(seed, PmaxLim, Nreplicates)
```

## Arguments

- seed:

  The seed used to ensure results are reproducible (numeric).

- PmaxLim:

  The minimum and maximum allowed prey values (numeric).

- Nreplicates:

  The number of pairs of seasonal runs in the simulation (numeric).

## Value

The median prey values to use in each pair of simulations.

## Examples

``` r
set_medianprey(3427, c(150, 350), 20)
#>  [1] 157 169 177 182 198 208 213 225 233 241 254 265 273 287 290 308 316 326 334
#> [20] 344
```
