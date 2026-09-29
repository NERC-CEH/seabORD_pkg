# SeabORD main function

A model to estimate the population consequences of displacement from
proposed offshore renewable energy developments for key seabird species

## Usage

``` r
seabord(
  Par,
  modPar,
  ordPar,
  switches,
  seamask,
  spadat1,
  spadat2,
  spdat,
  BrdData,
  FrgCompData,
  fltdist_base,
  FlightGridcorrection,
  ORDpoly
)
```

## Arguments

- Par:

  List - The main parameters controlling/defining this run

- modPar:

  List - Parameters relating to the model mode and computer environment

- ordPar:

  List - Input parameters relating to the ORDs

- switches:

  List - A set of switches/flags used to control optional features of
  the run

- seamask:

  Raster - land/sea grid, 1km expected.

- spadat1:

  description TBC

- spadat2:

  description TBC

- spdat:

  Species-specific parameters

- BrdData:

  description TBC

- FrgCompData:

  description TBC

- fltdist_base:

  Flight distance by sea, without ORDs. See user guide for details.

- FlightGridcorrection:

  Flight distance transition layer (gdistance)

- ORDpoly:

  The ORD footprints

## Value

A list containing tibbles
