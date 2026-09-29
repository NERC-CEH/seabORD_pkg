# Create individual seabirds for the simulation

This function creates a tibble holding one row per individual seabird.
The values stored in this tibble do not change through the simulation,
they describe characteristics that remain constant like species, sex and
home colony.

## Usage

``` r
set_initialbirdtype(seed, Colonies, thisSpecies, Pwfde, Pwfbe)
```

## Arguments

- seed:

  The seed used to ensure results are reproducible.

- Colonies:

  A tibble containing information about the colonies of interest in this
  simulation. Requires "code" with the colony codes and "ObsNpairs"

- thisSpecies:

  The two-letter code identifying the species of interest (only works
  with one species in v2.0.0).

- Pwfde:

  The probability of displacement across the population if encountering
  an ORD,where 0 means no displacement.

- Pwfbe:

  The probability across the population of experiencing a barrier effect
  when encountering an ORD. Only applies to birds who are displacement
  susceptible.

## Value

A list containing two tibbles: the tidy data and the metadata for
documentation.

## Examples

``` r
set_initialbirdtype(231, tibble::tibble(code="UK9004171", ObsNpairs=100), "Kw", 0.3, 1.0)
#> $data
#> # A tibble: 200 × 7
#>    BirdID Species colony    PairID MF     wfde  wfbe
#>     <int> <chr>   <chr>      <int> <chr> <int> <dbl>
#>  1      1 Kw      UK9004171      1 M         0     0
#>  2      2 Kw      UK9004171      1 F         0     0
#>  3      3 Kw      UK9004171      2 M         0     0
#>  4      4 Kw      UK9004171      2 F         0     0
#>  5      5 Kw      UK9004171      3 M         0     0
#>  6      6 Kw      UK9004171      3 F         0     0
#>  7      7 Kw      UK9004171      4 M         0     0
#>  8      8 Kw      UK9004171      4 F         0     0
#>  9      9 Kw      UK9004171      5 M         0     0
#> 10     10 Kw      UK9004171      5 F         0     0
#> # ℹ 190 more rows
#> 
#> $metadata
#> # A tibble: 7 × 3
#>   VarName VarDescription                                                VarUnits
#>   <chr>   <chr>                                                         <chr>   
#> 1 BirdID  [Key] Individual unique identifier                            ""      
#> 2 Species Two-letter code to identify the species                       ""      
#> 3 colony  Three-letter code to identify the home colony                 ""      
#> 4 PairID  Identification number for the pair, one male and one female   ""      
#> 5 MF      A letter, M or F, to indicate sex of this bird                ""      
#> 6 wfde    A flag, 0/1, to indicate if this bird is susceptible to disp… ""      
#> 7 wfbe    A flag, 0/1, to indicate if this bird is susceptible to barr… ""      
#> 
```
