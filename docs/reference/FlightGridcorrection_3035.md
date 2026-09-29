# FlightGridcorrection_3035: TransitionLayer Object

FlightGridcorrection_3035: TransitionLayer Object

## Usage

``` r
FlightGridcorrection_3035
```

## Format

An object of class `TransitionLayer` with the following components:

- class:

  The object is of class `TransitionLayer`, NEED EXPL

- dimensions:

  Dimensions of the data grid: 2200 rows and 1577 columns, with a total
  of 3,469,400 cells.

- resolution:

  The resolution of the raster grid: 1000 meters in both x and y
  directions.

- extent:

  Spatial extent of the grid defined by the minimum and maximum x and y
  coordinates. The extent is: xmin = 2661966, xmax = 4238966, ymin =
  2685159, ymax = 4885159.

- crs:

  Coordinate Reference System (CRS) used for the grid:
  `+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs`

- values:

  NEED EXPL

- matrix class:

  The data is stored in a sparse matrix format (`dsTMatrix`), which is
  efficient for handling large datasets with a significant number of
  zero values.

## Source

NEED EXPL
