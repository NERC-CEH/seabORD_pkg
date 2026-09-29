# Set initial values for individual birds

Set the initial values for the birds in the simulation using species
parameters. All birds begin the simulation alive, part of a pair with a
live chick and in good condition.

## Usage

``` r
set_initialbirdstate(
  seed,
  N,
  tstep,
  BM_adult_mn,
  BM_adult_sd,
  adult_DEE_mn,
  adult_DEE_sd,
  assim_eff
)
```

## Arguments

- seed:

  A seed to ensure reproducibility (numeric).

- N:

  The number of birds to generate (numeric).

- tstep:

  The length of the timestep in hours (numeric).

- BM_adult_mn:

  Adult body mass, mean, g (numeric).

- BM_adult_sd:

  Adult body mass, standard deviation (numeric).

- adult_DEE_mn:

  Adult daily energy expenditure, mean, kJ (numeric).

- adult_DEE_sd:

  Adult daily energy expenditure, standard deviation, kJ (numeric).

- assim_eff:

  Assimilation efficiency (numeric).

## Value

A list containing a tibble holding the individual bird states at time t0
and a tibble with metadata for the documentation.

## Examples

``` r
set_initialbirdstate(123, 10, 24, 372.7, 33.6, 802, 196, 0.74)
#> $data
#> # A tibble: 10 × 10
#>    BirdID BM_adult_t0 BM_adult BM_condition is_alive season_stage is_chick_alive
#>     <int>       <dbl>    <dbl>        <dbl>    <dbl>        <dbl>          <dbl>
#>  1      1        354.     354.            1        1            0              1
#>  2      2        365.     365.            1        1            0              1
#>  3      3        425.     425.            1        1            0              1
#>  4      4        375.     375.            1        1            0              1
#>  5      5        377.     377.            1        1            0              1
#>  6      6        430.     430.            1        1            0              1
#>  7      7        388.     388.            1        1            0              1
#>  8      8        330.     330.            1        1            0              1
#>  9      9        350.     350.            1        1            0              1
#> 10     10        358.     358.            1        1            0              1
#> # ℹ 3 more variables: feeding_mode <dbl>, Ereq_adult <dbl>, CoD <chr>
#> 
#> $metadata
#> # A tibble: 10 × 3
#>    VarName        VarDescription                                        VarUnits
#>    <chr>          <chr>                                                 <chr>   
#>  1 BirdID         [Key] Individual unique identifier                    ""      
#>  2 BM_adult_t0    Body mass at time t0                                  "g"     
#>  3 BM_adult       Body mass at time t                                   "g"     
#>  4 BM_condition   Relative condition (0-1)                              ""      
#>  5 is_alive       State at time t, dead or alive                        "0/1"   
#>  6 season_stage   Breeding season stage at time t (TBC)                 ""      
#>  7 is_chick_alive Individual has live chick at time t                   "0/1"   
#>  8 feeding_mode   Feeding mode at time t (1=Provisioning, 2=Nest unatt… ""      
#>  9 Ereq_adult     Energy required t+1 based on expenditure at time t    "kJ"    
#> 10 CoD            Cause of death                                        ""      
#> 
```
