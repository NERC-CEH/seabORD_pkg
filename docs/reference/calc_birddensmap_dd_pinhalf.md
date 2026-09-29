# Calculate bird density maps used the distance-decay function used in SeabORD, via users specifying `phalf`

Calculate bird density maps used the distance-decay function used in
SeabORD, via users specifying `phalf`

## Usage

``` r
calc_birddensmap_dd_pinhalf(dmap, fr, pinhalf)
```

## Arguments

- dmap:

  A raster containing the distance by sea from each grid cell to the
  population of interest

- fr:

  Foraging range, in kilometres

- pinhalf:

  the proportion of the foraging range within which half of the UD lies

## Value

A raster containing the distance-decay map
