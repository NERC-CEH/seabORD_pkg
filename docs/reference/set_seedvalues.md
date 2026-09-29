# Set the seeds needed for reproducibility

SeabORD includes stochastic methods that rely on the generation of
random numbers. To make the simulations reproducible we start the random
number sequence using a seed. This function used the main, user-supplied
seed to create a table of other seeds that are used to initialise other
functions. Each stream in the simulation uses a separate seed so that,
for example, changes to the number of live birds does not affect the
sequence in another part of the code. Adding new streams or changing the
number of replicates should not change previous runs significantly
(assuming the same starter seed).

## Usage

``` r
set_seedvalues(initialseed, nstreams, Nreplicates)
```

## Arguments

- initialseed:

  The user-supplied main seed.

- nstreams:

  The number of different streams required.

- Nreplicates:

  The number simulation pairs in the run.

## Value

A tibble with integer seed values for each stream and replicate
simulation pair.

## Examples

``` r
  set_seedvalues(2327, 10, 5)
#> # A tibble: 10 × 6
#>     seed  run1  run2  run3  run4  run5
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  6315  2289  9231  1488   837  2409
#>  2  5378  4671  8495     9  6010  4307
#>  3  3707  3815  7714  7941  1740  3904
#>  4  8570  5278  1430  6454   239  6217
#>  5  4769   690  2553  2856  5069  6318
#>  6   274  8514  2406  7470  4421  5354
#>  7   897   602  3724  9738  7396  6895
#>  8  6052  8077  3336  5505  1520  3239
#>  9  5749  1436  4285  9856  9320  2041
#> 10   739  8281  9146   134  6424  4045
  
```
