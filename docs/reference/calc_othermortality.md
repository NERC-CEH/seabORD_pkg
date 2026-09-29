# Chick mortality as a result of other causes

Chicks can be lost from other causes such as flooding, storms etc. This
function calculates the probability of death and then uses rbinom to
remove some chicks according to that probability.

## Usage

``` r
calc_othermortality(CoD, alive, seasonlength)
```

## Arguments

- CoD:

  Cause of death ("killed", "starved", "unattended", "parentdead",
  "abandoned", "other")

- alive:

  Flag to indicate if the chick is currently alive (1) or dead (0)

- seasonlength:

  Species parameter, number of timesteps in the breeding season

## Value

A revised chick state list.
