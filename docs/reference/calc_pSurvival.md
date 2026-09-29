# Probability of winter survival (mass-survival)

Calculate the probability of survival over the whole year based on the
body mass of the individual relative to the population mean, species
expected survival and parameters. Baseline survival values may apply to
poor, moderate or good years.

## Usage

``` r
calc_pSurvival(sp, bmi, bm, basesurv, beta, sd = 0)
```

## Arguments

- sp:

  The species two-letter code (either from "KI", "GU", "KI" or "RA")

- bmi:

  Body mass for the individual adult bird (g)

- bm:

  Body mass of the adult birds alive at the end of the breeding season,
  mean (g)

- basesurv:

  Baseline survival

- beta:

  Mass-survival slope

- sd:

  Body mass of the adult birds alive at the end of the breeding season,
  standard deviation

## Value

A probability of survival

## Examples

``` r
  calc_pSurvival("KI", 345.9, 370.8, 0.8, 0.038, 0)
  calc_pSurvival("GU", 345.9, 370.8, 0.92, 1.03, 50)
```
