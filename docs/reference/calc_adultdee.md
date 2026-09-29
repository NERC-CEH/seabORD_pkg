# Calculate Daily Energy Requirement

This function calculates the energy expenditure timestep t for adult
birds, based on the activities carried out. This value is assumed to be
the energy requirement for the following time step, t+1.

DEE is the sum of proportion of total deployment time spent on each of
these activities multiplied by activity-specific energetic costs
available from the literature (Pennycuick 1987, 1989; Croll & McLaren
1993; Hilton et al.2000a; Enstipp et al. 2006; respective costs: 1168.91
kJ day-1; 7361.72kJ day-1; 810.28 kJ day-1; 1894.90 kJ day-1) and the
cost of warming food (Gremillet et al. 2003).

## Usage

``` r
calc_adultdee(
  alive,
  colony_h,
  flying_h,
  foraging_h,
  at_sea_h,
  energy_nest,
  energy_flight,
  energy_forage,
  energy_searest,
  energy_warming,
  assim_eff,
  daylength
)
```

## Arguments

- alive:

  Is the bird dead or alive? 0,1

- colony_h:

  Time spent at the colony, hours

- flying_h:

  Time spent flying, hours

- foraging_h:

  Time spent foraging, hours

- at_sea_h:

  Time spent resting at sea, hours

- energy_nest:

  Energy cost of nesting at colony, kJ per day

- energy_flight:

  Energy cost of flight, kJ per day

- energy_forage:

  Energy cost of foraging, kJ per day

- energy_searest:

  Energy cost of resting at sea, kJ per day

- energy_warming:

  Energy cost of warming food, kJ per day

- assim_eff:

  Assimilation efficiency

- daylength, :

  Length of this species' time step, hours

## Value

A revised adult DEE for time step
