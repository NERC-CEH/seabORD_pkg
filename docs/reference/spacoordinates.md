# Spatial Coordinates of Sites

A dataset containing spatial coordinates and related attributes for
various sites, including information on whether the site is marine,
whether data is available for different columns, and the spatial
proximity of flight paths.

## Usage

``` r
spacoordinates
```

## Format

A data frame with 23 columns:

- SITECODE:

  Character: The unique code identifying each site.

- CEF.include:

  Logical: Whether the site is included in the CEF dataset.

- Marine:

  Logical: Whether the site is marine.

- Longitude:

  Numeric: The longitude of the site.

- Latitude:

  Numeric: The latitude of the site.

- dat.CELLNO:

  Numeric: The cell number for the site in the data.

- dat.ATSEA:

  Logical: Whether the site is at sea.

- flt.LONG:

  Numeric: The longitude of the flight path.

- flt.LAT:

  Numeric: The latitude of the flight path.

- flt.CELLNO:

  Numeric: The cell number for the flight path.

- flt.ATSEA:

  Logical: Whether the flight path is at sea.

- flt.DIST:

  Numeric: The distance of the flight path from the site.

- flt.near:

  Logical: Whether the flight path is near the site.

- datxy.E:

  Numeric: The x-coordinate in the East direction for the site.

- datxy.N:

  Numeric: The y-coordinate in the North direction for the site.

- datxy.CELLNO:

  Numeric: The cell number for the site in the XY grid.

- datxy.ATSEA:

  Logical: Whether the XY coordinates are at sea.

- fltxy.E:

  Numeric: The x-coordinate in the East direction for the flight path.

- fltxy.N:

  Numeric: The y-coordinate in the North direction for the flight path.

- fltxy.CELLNO:

  Numeric: The cell number for the flight path in the XY grid.

- fltxy.ATSEA:

  Logical: Whether the XY coordinates for the flight path are at sea.

- fltxy.DIST:

  Numeric: The distance of the flight path in XY space.

- fltxy.near:

  Logical: Whether the flight path is near the site in XY space.

## Source

NEED EXPL
