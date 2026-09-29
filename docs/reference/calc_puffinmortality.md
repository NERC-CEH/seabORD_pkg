# Puffin chick mortality from predation due to hunger

If a puffin chick is hungry, there is an increased risk of predation.
This function takes the 'hungry' chicks (based on relative body
condition), calculates the probability of death by predation and then
uses rbinom to remove some of those chicks.

## Usage

``` r
calc_puffinmortality(CoD, alive, BM_condition, BM_Chick_mortf)
```

## Arguments

- CoD:

  Cause of death ("killed", "starved", "unattended", "parentdead",
  "abandoned", "other")

- alive:

  Flag to indicate if the chick is currently alive (1) or dead (0)

- BM_condition:

  Body mass relative condition, 0-1

- BM_Chick_mortf:

  Critical mass below which chick is dead, 0-1

## Value

A revised cause of death
