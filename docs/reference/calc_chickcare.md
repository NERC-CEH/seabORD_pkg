# Parenting the chicks

For each chick, get the food provided by each of the parent birds and
increase the chick body mass. Compare with the 'optimum chick' to
estimate relative body condition and update the chick state. Add up the
total hours of attendance for the timestep.

## Usage

``` r
calc_chickcare(parent, ChickState, Opt_BM_chick, Species)
```

## Arguments

- parent:

  A tibble containing the required variables from the adult birds
  (BirdID, PairID, feeding_mode, Ereq_intakef_c, colony_h, MF).

- ChickState:

  List holding the current state of all the chicks and metadata.

- Opt_BM_chick:

  The optimum body mass for the chick at this stage of the season (a
  chick that has received full requirements every day).

- Species:

  List holding the species-specific variables (uses daylength,
  BM_Chick_mortf and chick_mass_a)

## Value

The updated list holding the tibbles for the chick current state.
