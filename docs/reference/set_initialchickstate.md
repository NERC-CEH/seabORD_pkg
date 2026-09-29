# Set initial values for individual chicks

Set the initial values for the chicks in the simulation using species
parameters. All chicks begin the season alive and in good condition.

## Usage

``` r
set_initialchickstate(
  seed,
  N,
  tstep,
  BM_chick_mn,
  BM_chick_sd,
  chick_DER,
  assim_eff
)
```

## Arguments

- seed:

  A seed to ensure reproducibility (numeric).

- N:

  The number of chicks to generate (numeric).

- tstep:

  The length of the time step in hours (numeric).

- BM_chick_mn:

  Chick body mass, mean, g (numeric).

- BM_chick_sd:

  Chick body mass, standard deviation (numeric).

- chick_DER:

  Chick daily energy requirement, mean, kJ (numeric).

- assim_eff:

  Assimilation efficiency (numeric).

## Value

A list containing two tibbles; one holding the individual chick states
at time t0 and the metadata for the documentation.

## Examples

``` r
set_initialchickstate(123, 10, 24.0, 36.0, 2.2, 525.7, 0.74)
#> $data
#> # A tibble: 5 × 6
#>   PairID BM_chick BM_condition is_chick_alive CoD   Ereq_chick
#>    <int>    <dbl>        <dbl>          <dbl> <chr>      <dbl>
#> 1      1     34.8        0.966              1 none        710.
#> 2      2     35.5        0.986              1 none        710.
#> 3      3     39.4        1.10               1 none        710.
#> 4      4     36.2        1.00               1 none        710.
#> 5      5     36.3        1.01               1 none        710.
#> 
#> $metadata
#> # A tibble: 6 × 3
#>   VarName        VarDescription                     VarUnits
#>   <chr>          <chr>                              <chr>   
#> 1 PairID         [Key] Individual unique identifier ""      
#> 2 BM_chick       Body mass at time t                "g"     
#> 3 BM_condition   Relative condition (0-1)           ""      
#> 4 is_chick_alive State at time t, dead or alive     "0/1"   
#> 5 CoD            Cause of death                     ""      
#> 6 Ereq_chick     Energy required at time t          "kJ"    
#> 
```
