# Calculate distance restricted by obstructions

Calculate distance restricted by obstructions

## Usage

``` r
calc_dist_restricted(
  mymap,
  obspolys = NULL,
  targetcoords,
  FlightGridcorrection_3035,
  directions = 16,
  obspenalty = 1e+08,
  maxdist = 9000
)
```

## Arguments

- mymap:

  A raster in which grid cells on land are assumed to have missing (NA)
  values

- obspolys:

  Polygons associated with obstacles; if NULL there are assumed to be no
  obstacles other than land

- FlightGridcorrection_3035:

  The 'flight correction' layer required by gdistance to correct for
  latitude (see gdistance)

- obspenalty:

  Penalty value associated with crossing an obstruction; a positive
  number

- maxdist:

  Maximum distance, in km; above this value are fixed to zero

## Value

Raster, containing distance to target from each grid, avoiding
obstructions, in kilometres

## Details

Grid cells are treated as obstructed if (a) they have a missing (NA)
value in the map, or, if "obspolys" has been provided, the grid cell
lies within any of the polygons in "obspolys". Created 26 May 2024 based
on "make_fltdist_scen" in SeabORD, but (a) for a single colony and (b)
for obstruction either by land or other (e.g. footprints)
