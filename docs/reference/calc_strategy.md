# Calculate the foraging strategy for the timestep

Given information about one flight and the time available, this function
determines the foraging strategy chosen by an individual bird.

## Usage

``` r
calc_strategy(
  BM_condition,
  BM_adult_abdn,
  fmode,
  Fg,
  tf,
  tcapt,
  maxf,
  ts,
  tc,
  tm,
  ttotal
)
```

## Arguments

- BM_condition:

  The relative body condition of the bird which may influence the
  strategy

- BM_adult_abdn:

  The relative body condition at which the adult abandons the chick

- fmode:

  The feeding mode of the individual in the previous timestep
  (numeric())

- Fg:

  The total amount of food to be gathered across multiple flights within
  the timestep, (numeric())

- tf:

  The time it takes to fly the trip once, (numeric())

- tcapt:

  A look-up giving the amount of food captured after t minutes, (tibble,
  2 variables, tmin, captured_g)

- maxf:

  The maximum number of flights permitted, (integer)

- ts:

  The minimum time that must be spent at sea, (numeric())

- tc:

  The optimum time to be spent at the colony, (numeric())

- tm:

  The minimum time that can be spent at the colony (numeric())

- ttotal:

  The length of a 'day' for this species in hours, giving the maximum
  time in which to fit the flights, (integer)

## Value

A tibble with seven variables; the number of flights, total time spent
foraging, total time spent flying, total grams of food captured, the
time spent at the colony and at sea, and the feeding mode that was
followed.
