# Package index

## Functions to run the model

- [`seabord()`](seabord.md) : SeabORD main function

## Main functions behind the seabORD model

- [`calc_adultbmchange()`](calc_adultbmchange.md) : Update adult body
  mass
- [`calc_adultdee()`](calc_adultdee.md) : Calculate Daily Energy
  Requirement
- [`calc_pSurvival()`](calc_pSurvival.md) : Probability of winter
  survival (mass-survival)
- [`calc_puffinmortality()`](calc_puffinmortality.md) : Puffin chick
  mortality from predation due to hunger
- [`calc_chickcare()`](calc_chickcare.md) : Parenting the chicks
- [`calc_othermortality()`](calc_othermortality.md) : Chick mortality as
  a result of other causes
- [`calc_unattendmortality()`](calc_unattendmortality.md) : Chick
  mortality as a result of adults not attending the nest
- [`calc_strategy()`](calc_strategy.md) : Calculate the foraging
  strategy for the timestep
- [`calc_foragecapture()`](calc_foragecapture.md) : Calculate the time
  taken to forage required amount
- [`memoised_calc_foragecapture()`](memoised_calc_foragecapture.md) :
  Memoized Version of calc_foragecapture

## Other functions behind seabORD

- [`set_seedvalues()`](set_seedvalues.md) : Set the seeds needed for
  reproducibility
- [`set_medianprey()`](set_medianprey.md) : Set the median prey value
  across the region
- [`set_initialbirdtype()`](set_initialbirdtype.md) : Create individual
  seabirds for the simulation
- [`set_initialchickstate()`](set_initialchickstate.md) : Set initial
  values for individual chicks
- [`set_initialbirdstate()`](set_initialbirdstate.md) : Set initial
  values for individual birds

## Datasets available

- [`example_1_lists`](example_1_lists.md) : example_1_lists
- [`example_lists_calibration`](example_lists_calibration.md) :
  example_lists_calibration
- [`example_scenario_output`](example_scenario_output.md) :
  example_scenario_output
- [`specieslist`](specieslist.md) : Species List Dataset
- [`spalist`](spalist.md) : SPA Site List Dataset
- [`CEF_colsize_table`](CEF_colsize_table.md) : CEF Size Table
- [`energeticsandpreydata`](energeticsandpreydata.md) : Energetics and
  Prey Data
- [`BrdData_example`](BrdData_example.md) : BrdData_example
- [`frgcompdata_example`](frgcompdata_example.md) : frgcompdata_example
- [`cef_coast_4326`](cef_coast_4326.md) : cef_coast_4326
- [`cef_coast_3035`](cef_coast_3035.md) : cef_coast_3035
- [`seamask_3035_example`](seamask_3035_example.md) :
  seamask_3035_example
- [`spacoordinates`](spacoordinates.md) : Spatial Coordinates of Sites
- [`UK9004171_bysea_3035`](UK9004171_bysea_3035.md) :
  UK9004171_bysea_3035
- [`FlightGridcorrection_3035`](FlightGridcorrection_3035.md) :
  FlightGridcorrection_3035: TransitionLayer Object
- [`offshorerenewabledevelopmentnames`](offshorerenewabledevelopmentnames.md)
  : offshorerenewabledevelopmentnames
- [`ORDpoly_example`](ORDpoly_example.md) : ORDpoly_example
- [`ORDpoly_example_wfold`](ORDpoly_example_wfold.md) :
  ORDpoly_example_wfold
- [`example_calibration_output`](example_calibration_output.md) :
  example_calibration_output
- [`DBS_map_example`](DBS_map_example.md) : DBS_map_example.R
- [`DBS_withORDs_example`](DBS_withORDs_example.md) :
  DBS_withORDs_example.R
- [`example_lists_calibration_dd`](example_lists_calibration_dd.md) :
  example_lists_calibration_dd
- [`frgcompdata_example_dd`](frgcompdata_example_dd.md) :
  frgcompdata_example_dd
- [`BrdData_example_dd`](BrdData_example_dd.md) : BrdData_example_dd
- [`UK9002491_bysea_3035`](UK9002491_bysea_3035.md) :
  UK9002491_bysea_3035
- [`example_lists_dd`](example_lists_dd.md) : example_lists_dd

## seabORD distance decay

- [`calc_birddensmap_dd_pinfr()`](calc_birddensmap_dd_pinfr.md) :
  Calculate bird density maps

- [`calc_birddensmap_dd_pinhalf()`](calc_birddensmap_dd_pinhalf.md) :

  Calculate bird density maps used the distance-decay function used in
  SeabORD, via users specifying `phalf`

- [`calc_dd_phalf_from_pinfr()`](calc_dd_phalf_from_pinfr.md) :
  Calculate 'q' from 'p' for the distance-decay model

- [`calc_dd_pinfr_from_phalf()`](calc_dd_pinfr_from_phalf.md) :
  Calculate 'pinfr' from 'phalf' for the distance-decay model

- [`calc_dist_restricted()`](calc_dist_restricted.md) : Calculate
  distance restricted by obstructions

- [`voptimise()`](voptimise.md) :

  Use numerical optimisation to give the value of `x` associated with
  each element of a vector `y`, where `y = fny(x)` for a specified
  function `fny`
