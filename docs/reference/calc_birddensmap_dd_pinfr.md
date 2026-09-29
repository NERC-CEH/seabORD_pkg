# Calculate bird density maps

Calculate bird density maps using the distance-decay function used in
SeabORD, via users specifying "proportion of UD lying within the
foraging range"

## Usage

``` r
calc_birddensmap_dd_pinfr(dmap, fr, pinfr, dmin = 1)
```

## Arguments

- dmap:

  A raster containing the distance by sea from each grid cell to the
  population of interest

- fr:

  Foraging range, in kilometres

- pinfr:

  The proportion of the UD that is assumed to lie within a distance `fr`
  of the population: a numeric value between 0 and 1

## Value

A raster containing the distance-decay map

### 20 Oct 2023: added "dmin" argument
