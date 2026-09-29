# Chick mortality as a result of adults not attending the nest

If a chick is left alone at the nest, there is an increased risk of
death from predation etc. This function takes the chicks who are not
fully attended by the adults, calculates the probability of death and
then uses rbinom to remove some of those chicks.

## Usage

``` r
calc_unattendmortality(CoD, alive, unattend_hrs, max_hrs)
```

## Arguments

- CoD:

  Cause of death

- alive:

  Flag to indicate if the chick is currently alive (1) or dead (0)

- unattend_hrs:

  Number of hrs the chick was left unattended

- max_hrs:

  Species parameter: Critical time threshold for unattendance at nest
  above which a chick is assumed to die through exposure or predation

## Value

A revised cause of death
