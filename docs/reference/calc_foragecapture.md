# Calculate the time taken to forage required amount

Given the starting prey density and the competition for food, we need to
estimate how long it would take a bird to gather the food it needs.
Intake model: We assume that the relationship between prey quantity x
and intake rate (-dx/dt) is of the form dx/dt = -ax/(h+x) for parameters
a and h, the widely-used Michaelis-Menten model. This implies that the
prey quantity at time t is equal to

x(t,x_0)={x:(x-x_0+at+hlog(x)-h log(x_0)) = 0}

where x_0 denotes the prey quantity at time 0. This in turn implies that
the total prey consumed by foraging up to time t is equal to

y(t,x_0) = x_0 - x(t,x_0)

The solution to this cannot be written down analytically, but it can be
calculated numerically e.g. using a non-linear solver. This function
uses the solver, nleqslv, to create a look-up table for a given x_0, a
and h for a range of values of t.

## Usage

``` r
calc_foragecapture(x0, h, a, maxt)
```

## Arguments

- x0:

  Prey quantity at time 0, g

- h:

  Species IR_half (modified to account for neighbours), kg/km2

- a:

  Species maximum intake rate, kg/min

- maxt:

  The maximum number of minutes available

## Value

A tibble with two variables 'tmin' and 'captured' and 'maxt' rows.
