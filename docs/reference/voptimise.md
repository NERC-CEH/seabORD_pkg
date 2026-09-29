# Use numerical optimisation to give the value of `x` associated with each element of a vector `y`, where `y = fny(x)` for a specified function `fny`

Use numerical optimisation to give the value of `x` associated with each
element of a vector `y`, where `y = fny(x)` for a specified function
`fny`

## Usage

``` r
voptimise(y, fny, iv)
```

## Arguments

- y:

  A vector of numerical values

- fny:

  A function with a single argument `x` that produces the value of `y`
  associated with `x`

- iv:

  A vector of length two indicating the numerical range of values to
  optimize over

## Value

A vector of numeric values of lengt `y` containing the value of `x`
associated with each value of `y`
