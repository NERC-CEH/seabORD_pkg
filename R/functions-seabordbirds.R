################################################################################
#' @title Update adult body mass
#' @description Calculate body mass change. All adult birds update their body
#'   mass at the end of each day based on the energy they gain and expend
#'   foraging and in other activities. The model we use is an expanded version
#'   of that used in Daunt & Wanless (2008) and Wanless et al. (1997), which
#'   separates flight cost and foraging cost for each adult to derive total
#'   energy expenditure
#' @param alive Is the bird dead or alive? FALSE, TRUE
#' @param BM_adult Body mass of the adult bird, g
#' @param Egain_adult Energy actually acquired in the time step, kJ
#' @param Ereq_adult The full energy requirement for this time step, kJ
#' @param adult_mass_KG Energy density of the adult bird tissue, kJ per gram
#' @details
#' Adult body mass is updated according to:
#'
#' \deqn{
#' M_{t+1} = M_t + \frac{(E_{gain} - E_{req})}{KG}
#' }
#'
#' where \eqn{M_t} is the current body mass, \eqn{E_{gain}} is the energy
#' acquired during the time step, \eqn{E_{req}} is the energetic requirement,
#' and \eqn{KG} is the energy density of adult tissue.
#'
#' Positive energy balance results in mass gain, while negative energy balance
#' results in mass loss.
#'
#' @return A revised adult body mass
#' @references
#' Daunt & Wanless (2008); Wanless et al. (1997).
#' @examples
#' # Adult gains more energy than it expends
#' calc_adultbmchange(
#'   alive = TRUE,
#'   BM_adult = 400,
#'   Egain_adult = 1200,
#'   Ereq_adult = 1000,
#'   adult_mass_KG = 25
#' )
#'
#' # Adult expends more energy than it gains
#' calc_adultbmchange(
#'   alive = TRUE,
#'   BM_adult = 400,
#'   Egain_adult = 800,
#'   Ereq_adult = 1000,
#'   adult_mass_KG = 25
#' )
#'
#' @export
#'
calc_adultbmchange <- function(alive, BM_adult, Egain_adult, Ereq_adult,
                               adult_mass_KG){

  # Body mass increase
  # M(d+1) = M(d) +  [(Egain(d) - DEE(d)) / KG]

  if (alive){
    BM_adult <- BM_adult + ((Egain_adult - Ereq_adult)/adult_mass_KG)
  }

  return(BM_adult)
}


################################################################################
#' @title Calculate Daily Energy Requirement
#' @description This function calculates the energy expenditure timestep t for
#'   adult birds, based on the activities carried out. This value is assumed to
#'   be the energy requirement for the following time step, t+1.
#'
#'  DEE is the sum of proportion of total deployment time spent on each of these
#'  activities multiplied by activity-specific energetic costs available from
#'  the literature (Pennycuick 1987, 1989; Croll & McLaren 1993; Hilton et
#'  al.2000a; Enstipp et al. 2006; respective costs: 1168.91 kJ day-1; 7361.72kJ
#'  day-1; 810.28 kJ day-1; 1894.90 kJ day-1) and the cost of warming food
#'  (Gremillet et al. 2003).
#' @param alive Is the bird dead or alive? 0,1
#' @param colony_h Time spent at the colony, hours
#' @param flying_h Time spent flying, hours
#' @param foraging_h Time spent foraging, hours
#' @param at_sea_h Time spent resting at sea, hours
#' @param energy_nest Energy cost of nesting at colony, kJ per day
#' @param energy_flight Energy cost of flight, kJ per day
#' @param energy_forage Energy cost of foraging,  kJ per day
#' @param energy_searest Energy cost of resting at sea, kJ per day
#' @param energy_warming Energy cost of warming food, kJ per day
#' @param assim_eff Assimilation efficiency
#' @param daylength, Length of this species' time step, hours
#' @return A revised adult DEE for time step
#' @references
#' Pennycuick, C. J. (1987, 1989); Croll and McLaren (1993);
#' Hilton et al. (2000); Enstipp et al. (2006); and
#' Grémillet et al. (2003).
#' @examples
#' # Daily energy requirement for a living bird
#' Ereq_adult <- calc_adultdee(
#'   alive = TRUE,
#'   colony_h = 18,
#'   flying_h = 5.26,
#'   foraging_h = 11.23,
#'   at_sea_h = 1.5,
#'   energy_nest = 427.8,
#'   energy_flight = 1400.7,
#'   energy_forage = 1400.7,
#'   energy_searest = 400.6,
#'   energy_warming = 26,
#'   assim_eff = 0.74,
#'   daylength = 36
#' )
#'
#' Ereq_adult
#'
#' # A dead bird has no energetic requirements
#' Ereq_adult <- calc_adultdee(
#'   alive = FALSE,
#'   colony_h = 18,
#'   flying_h = 5.26,
#'   foraging_h = 11.23,
#'
#' @export
calc_adultdee <- function(alive, colony_h, flying_h, foraging_h, at_sea_h,
                          energy_nest, energy_flight, energy_forage,
                          energy_searest, energy_warming, assim_eff,
                          daylength){

  if (alive){

    adult_DEE <-
      (energy_nest * colony_h/daylength) +
      (energy_flight * flying_h/daylength) +
      (energy_forage * foraging_h/daylength) +
      (energy_searest * at_sea_h/daylength) +
      (energy_warming * daylength/24.0)

    Ereq_adult <- adult_DEE / assim_eff

  } else {

    Ereq_adult <- 0

  }

  return(Ereq_adult)
}



################################################################################
#' @title ilogit (used for calc_pSurvival)
#' @description The equation of logistic function or logistic curve is a common
#'   “S” shaped curve defined by the below equation. The logistic curve is also
#'   known as the sigmoid curve.
#'   yy = exp(y)/(1+exp(y))
#' @param y value
#' @noRd
ilogit <- function(y) {yy = exp(y)/(1+exp(y))}


#' @title logit (used for calc_pSurvival)
#' @description The logit function is the quantile function associated with the
#'   standard logistic distribution.
#' @param x value
#' @noRd
logit <- function(x) {xx = log(x/(1-x))}

#' @title Probability of winter survival (mass-survival)
#' @description Calculate the probability of survival over the whole year based
#'   on the body mass of the individual relative to the population mean, species
#'   expected survival and parameters. Baseline survival values may apply to
#'   poor, moderate or good years.
#' @param sp The species two-letter code (either from "KI", "GU", "KI" or "RA")
#' @param bmi Body mass for the individual adult bird (g)
#' @param bm Body mass of the adult birds alive at the end of the
#'   breeding season, mean (g)
#' @param sd Body mass of the adult birds alive at the end of the
#'   breeding season, standard deviation
#' @param basesurv Baseline survival
#' @param beta  Mass-survival slope
#' @details
#' For kittiwakes (`"KI"`), survival probability is calculated directly from
#' the difference between an individual's body mass and the population mean.
#'
#' For puffins, guillemots and razorbills, body mass is first standardised
#' using the population standard deviation before applying the survival
#' relationship.
#'
#' Individuals with body masses greater than the population mean therefore
#' receive higher survival probabilities, whereas individuals in poorer
#' condition receive lower probabilities.
#'
#' If the individual did not survive the breeding season (`bmi = 0` or
#' `NA`), the function returns a survival probability of zero.
#' @return A probability of survival
#' @examples
#'   calc_pSurvival("KI", 345.9, 370.8, 0.8, 0.038, 0)
#'   calc_pSurvival("GU", 345.9, 370.8, 0.92, 1.03, 50)
#' @export
#'
calc_pSurvival <- function (sp, bmi, bm, basesurv, beta, sd = 0) {

  if (bmi == 0 | is.na(bmi)){ # The bird didn't survive breeding season
    out <- 0
  } else { # Calculate survival over winter
    if (sp == "KI") {
      out <- ilogit(logit(basesurv) + ((bmi - bm)*beta))
    } else {
      out <- ilogit(logit(basesurv) + ((bmi - bm)/sd)*beta)
    }
  }
}


################################################################################
#' @title Puffin chick mortality from predation due to hunger
#' @description
#' Determines whether a puffin chick dies from predation associated with poor
#' body condition. Hungry chicks are assumed to spend more time near the burrow
#' entrance, increasing their vulnerability to predators.
#'
#' Chicks whose body condition falls below the hunger threshold are assigned a
#' probability of predation that increases as body condition declines. A
#' stochastic draw is then used to determine whether mortality occurs during the
#' current time step.
#'
#' @param CoD Current cause of death. Typically `"none"` if the chick is alive.
#' @param alive Indicator of whether the chick is currently alive (`1`) or dead
#'   (`0`).
#' @param BM_condition Relative chick body condition, expressed as a proportion
#'   of the expected body mass for its age.
#' @param BM_Chick_mortf Critical body condition threshold below which the chick
#'   dies from starvation.
#'
#' @details
#' Puffin chicks with a body condition below 0.7 are classified as hungry.
#' For hungry chicks, the probability of predation increases as body condition
#' declines from the hunger threshold towards the starvation threshold
#' (`BM_Chick_mortf`).
#'
#' Mortality is then determined through a binomial random draw. If mortality
#' occurs, the cause of death is updated to `"killed"`.
#'
#' @return
#' A character string giving the updated cause of death. Returns the original
#' value of `CoD` if the chick survives.
#'
#' @seealso [calc_chickcare()]
#'
#' @examples
#' # Healthy chick
#' calc_puffinmortality(
#'   CoD = "none",
#'   alive = 1,
#'   BM_condition = 0.9,
#'   BM_Chick_mortf = 0.5
#' )
#'
#' # Hungry chick
#' calc_puffinmortality(
#'   CoD = "none",
#'   alive = 1,
#'   BM_condition = 0.6,
#'   BM_Chick_mortf = 0.5
#' )
#'
#' # Explore outcomes under repeated simulations
#' mortality <- replicate(
#'   1000,
#'   calc_puffinmortality(
#'     CoD = "none",
#'     alive = 1,
#'     BM_condition = 0.6,
#'     BM_Chick_mortf = 0.5
#'   )
#' )
#'
#' table(mortality)
#'
#' @importFrom stats rbinom
#' @export
calc_puffinmortality <- function(CoD, alive, BM_condition, BM_Chick_mortf){

  if (alive) {
    BM_Chick_hungry  <- 0.7
    hungry <- ifelse(BM_condition < 0.7, 1, 0)
    Prob_death <- min((BM_Chick_hungry - BM_condition)/
                        (BM_Chick_hungry - BM_Chick_mortf), 1.0)
    Prob_death <- hungry * Prob_death
    unlucky <- stats::rbinom(n = 1, size = 1, Prob_death)
    if (unlucky==1) {CoD <- "killed"}
  }

  return(CoD)

}

################################################################################
#' @title Parenting the chicks
#' @description For each chick, get the food provided by each of the parent
#'   birds and increase the chick body mass. Compare with the 'optimum chick' to
#'   estimate relative body condition and update the chick state. Add up
#'   the total hours of attendance for the timestep.
#' @param parent A tibble containing the required variables from the adult birds
#'   (BirdID, PairID, feeding_mode, Ereq_intakef_c, colony_h, MF).
#' @param ChickState List holding the current state of all the chicks and
#'   metadata.
#' @param Opt_BM_chick The optimum body mass for the chick at this stage of the
#'   season (a chick that has received full requirements every day).
#' @param Species List holding the species-specific variables (uses daylength,
#'   BM_Chick_mortf and chick_mass_a)
#' @importFrom rlang .data
#' @importFrom tidyr starts_with ends_with
#' @importFrom dplyr bind_rows
#' @details
#' For each chick, food delivered by the male and female parent is summed and
#' converted into growth. The resulting body mass is compared with the expected
#' body mass for a chick of the same age to calculate relative body condition.
#'
#' Chicks may die through one of three pathways:
#' \itemize{
#'   \item \code{"starved"}: body condition falls below the mortality threshold.
#'   \item \code{"abandoned"}: one or both parents abandon the breeding attempt.
#'   \item \code{"parentdead"}: one or both parents die.
#' }
#'
#' In addition, the maximum period for which a chick is left unattended is
#' calculated from the combined colony attendance of both parents.
#'
#' @return The updated list holding the tibbles for the chick current state.
#' @export
#'
calc_chickcare <- function(parent, ChickState, Opt_BM_chick, Species){

  # Feed only live chicks!
  if (1 %in% ChickState$data$is_chick_alive) {

    ChickState$data <- ChickState$data %>% split(.$is_chick_alive)

    # Join the male parent contribution
    parentM <- dplyr::filter(parent, MF == "M")
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::left_join(parentM, by = "PairID") %>%
      dplyr::rename(feeding_mode_M = feeding_mode) %>%
      dplyr::rename(Ereq_intakef_c_M = Ereq_intakef_c) %>%
      dplyr::rename(colony_h_M = colony_h) %>%
      dplyr::select(-c(MF, BirdID))

    # Join the female parent contribution
    parentF <- dplyr::filter(parent, MF == "F")
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::left_join(parentF, by = "PairID") %>%
      dplyr::rename(feeding_mode_F = feeding_mode) %>%
      dplyr::rename(Ereq_intakef_c_F = Ereq_intakef_c) %>%
      dplyr::rename(colony_h_F = colony_h) %>%
      dplyr::select(-c(MF, BirdID))

    # Tot up the attendance
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::mutate(parenting_hrs = colony_h_M + colony_h_F) %>%
      dplyr::mutate(unattend_hrs = purrr::pmap_dbl(
        list(parenting_hrs, Species$data$daylength, 0.0),
        ~ ifelse({..1} < {..2}, {..2} - {..1}, {..3}))) %>%
      dplyr::select(-c("parenting_hrs")) %>%
      dplyr::select(!starts_with(c("colony")))

    # Total food received and increase in body mass
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::mutate(X = 0.5 * (Ereq_intakef_c_M + Ereq_intakef_c_F)) %>%
      dplyr::mutate(BM_increase = (Species$data$chick_mass_a*(X-0.6))/0.4) %>%
      dplyr::mutate(BM_chick = BM_chick + BM_increase) %>%
      dplyr::mutate(BM_condition = BM_chick/Opt_BM_chick) %>%
      dplyr::select(!ends_with(c("c_F","c_M"))) %>%
      dplyr::select(-c("X", "BM_increase"))

    # Update the chick modes
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::mutate(f = "none") %>%
      dplyr::mutate(f = purrr::pmap_chr(
        list(BM_condition, Species$data$BM_Chick_mortf, f),
        ~ ifelse({..1}<{..2}, "starved", {..3}))) %>%
      dplyr::mutate(f = purrr::pmap_chr(
        list(feeding_mode_M, feeding_mode_F, f),
        ~ ifelse(({..1}==3 | {..2}==3), "abandoned", {..3}))) %>%
      dplyr::mutate(f = purrr::pmap_chr(
        list(feeding_mode_M, feeding_mode_F, f),
        ~ ifelse(({..1}==4 | {..2}==4), "parentdead", {..3})))

    if (length(unique(ChickState$data$`1`$f)) > 1) {
      ChickState$data$`1` <- ChickState$data$`1` %>% split(.$f=="none")
      ChickState$data$`1`$`FALSE` <- ChickState$data$`1`$`FALSE` %>%
        dplyr::mutate(CoD = f) %>%
        dplyr::mutate(is_chick_alive = 0) %>%
        dplyr::mutate(BM_chick = NA) %>%
        dplyr::mutate(BM_condition = NA) %>%
        dplyr::mutate(Ereq_chick = NA) %>%
        dplyr::mutate(unattend_hrs = NA)
      ChickState$data$`1` <- bind_rows(ChickState$data$`1`$`TRUE`, ChickState$data$`1`$`FALSE`)
    }
    ChickState$data$`1` <- ChickState$data$`1` %>%
      dplyr::select(!starts_with(c("feeding_mode"))) %>%
      dplyr::select(-c("f"))

    # Reconstruct full tibble
    ChickState$data <- bind_rows(ChickState$data$`1`, ChickState$data$`0`)

    # > Update the metadata to reflect newly added variables to the ChickState tibble
    newmeta <- tidyr::tribble(
      ~VarName, ~VarDescription, ~VarUnits,
      "unattend_hrs", "Maximum length of time that the chick is not attended by either parent", "hours")
    ChickState$metadata <- bind_rows(ChickState$metadata, newmeta) %>%
      dplyr::filter(VarName %in% unique(names(ChickState$data)))

  }

  return(ChickState)

} # END calc_chickcare

################################################################################
#' @title Chick mortality as a result of other causes
#' @description Chicks can be lost from other causes such as flooding, storms
#'   etc. This function calculates the probability of death and then uses rbinom
#'   to remove some chicks according to that probability.A fixed seasonal mortality rate is distributed evenly across all model time
#'   steps, and a stochastic draw is used to determine whether mortality occurs  during the current time step.
#' @param CoD Current cause of death. Typically `"none"` if the chick is alive.
#' @param alive Indicator of whether the chick is currently alive (`1`) or dead
#'   (`0`).
#' @param seasonlength Number of time steps in the breeding season.
#'
#' @details
#' The function assumes a total background chick mortality rate of 5% across the
#' breeding season. This seasonal probability is converted into an equivalent
#' mortality probability for a single time step:
#'
#' \deqn{P(death) = 1 - (1 - 0.05)^{1/seasonlength}}
#'
#' Mortality is then determined using a binomial random draw. If mortality
#' occurs, the cause of death is updated to `"other"`.
#'
#' @return
#' A character string giving the updated cause of death. Returns the original
#' value of `CoD` if the chick survives.
#'
#' @seealso [calc_chickcare()], [calc_unattendmortality()]
#'
#' @examples
#' # Living chick exposed to background mortality
#' calc_othermortality(
#'   CoD = "none",
#'   alive = 1,
#'   seasonlength = 40
#' )
#'
#' # Repeat many times to estimate mortality frequency
#' mortality <- replicate(
#'   1000,
#'   calc_othermortality(
#'     CoD = "none",
#'     alive = 1,
#'     seasonlength = 40
#'   )
#' )
#'
#' table(mortality)
#'
#' @importFrom stats rbinom
#' @export
calc_othermortality <- function(CoD, alive, seasonlength){

  if (alive) {
    Prob_death <- 1.0 - ((1.0-0.05)^(1/seasonlength))
    unlucky <- stats::rbinom(n = 1, size = 1, Prob_death)
    if (unlucky==1) {CoD <- "other"}
  }

  return(CoD)

}

################################################################################
#' @title Chick mortality as a result of adults not attending the nest
#' @description
#' Determines whether a chick dies as a result of being left unattended by its
#' parents during a model time step. Longer periods of unattendance increase the
#' risk of mortality through mechanisms such as predation or exposure.
#'
#' The probability of death increases linearly with the duration of
#' unattendance, reaching 100% when the unattendance period equals or exceeds
#' the species-specific critical threshold.
#'
#' A stochastic draw is then used to determine whether mortality occurs.
#'
#' @param CoD Current cause of death. Typically `"none"` if the chick is alive.
#' @param alive Indicator of whether the chick is currently alive (`1`) or dead
#'   (`0`).
#' @param unattend_hrs Number of hours for which the chick was left unattended.
#' @param max_hrs Species-specific critical unattendance threshold (hours),
#'   above which mortality is assumed to occur.
#'
#' @details
#' The probability of mortality is calculated as:
#'
#' \deqn{P(death) = unattend\_hrs / max\_hrs}
#'
#' capped at a maximum value of 1.
#'
#' Mortality is then determined through a binomial random draw. If mortality
#' occurs, the cause of death is updated to `"unattended"`.
#'
#' @return
#' A character string giving the updated cause of death.
#' Returns the original value of `CoD` if the chick survives.
#'
#' @seealso [calc_chickcare()]
#'
#' @examples
#' # Chick attended by its parents
#' calc_unattendmortality(
#'   CoD = "none",
#'   alive = 1,
#'   unattend_hrs = 0,
#'   max_hrs = 18
#' )
#'
#' # Chick left unattended for a short period
#' calc_unattendmortality(
#'   CoD = "none",
#'   alive = 1,
#'   unattend_hrs = 6,
#'   max_hrs = 18
#' )
#'
#' # Chick left unattended for the maximum allowable time
#' calc_unattendmortality(
#'   CoD = "none",
#'   alive = 1,
#'   unattend_hrs = 18,
#'   max_hrs = 18
#' )
#'
#' @importFrom stats rbinom
#' @export
calc_unattendmortality <- function(CoD, alive, unattend_hrs, max_hrs) {

  if (alive) {
    lonely <- ifelse(unattend_hrs > 0, 1, 0)
    Prob_death <- lonely * min(unattend_hrs/max_hrs, 1.0)
    unlucky <- rbinom(n = 1, size = 1, Prob_death)
    if (unlucky==1) {CoD <- "unattended"}
  }
  return(CoD)
}

################################################################################
#' @title Calculate the foraging strategy for the timestep
#' @description
#' Determines the foraging strategy adopted by an individual bird during a
#' single model time step. The function evaluates a series of possible
#' behavioural responses based on the bird's energetic requirements, body
#' condition, available foraging time, and chick-rearing commitments.
#'
#' The selected strategy aims to balance self-maintenance and chick
#' provisioning while respecting the time constraints of the day.
#'
#' Birds first attempt to satisfy their food requirements while maintaining
#' optimal attendance at the colony. If this is not possible, increasingly
#' costly strategies are considered, including reduced chick attendance or
#' abandonment of the breeding attempt.
#'
#' The following strategies are evaluated in sequence:
#' \enumerate{
#'   \item Acquire all required food while maintaining optimal colony attendance.
#'   \item Acquire as much food as possible while maintaining optimal colony attendance.
#'   \item Acquire all required food while reducing colony attendance.
#'   \item Acquire as much food as possible while reducing colony attendance.
#' }
#' @param BM_condition The relative body condition of the bird which may influence the strategy
#' @param BM_adult_abdn The relative body condition at which the adult abandons the chick
#' @param fmode The feeding mode of the individual in the previous timestep (numeric())
#' @param Fg The total amount of food to be gathered across multiple flights
#'   within the timestep, (numeric())
#' @param tf The time it takes to fly the trip once, (numeric())
#' @param tcapt A look-up giving the amount of food captured after t minutes,
#'   (tibble, 2 variables, tmin, captured_g)
#' @param maxf The maximum number of flights permitted, (integer)
#' @param ts The minimum time that must be spent at sea, (numeric())
#' @param tc The optimum time to be spent at the colony, (numeric())
#' @param tm The minimum time that can be spent at the colony (numeric())
#' @param ttotal The length of a 'day' for this species in hours, giving the
#'   maximum time in which to fit the flights, (integer)
#' @details
#' Adult birds are classified into the following behavioural states:
#' \itemize{
#'   \item 1 = Alive with chick, in good body condition.
#'   \item 2 = Alive with chick, in moderate body condition.
#'   \item 3 = Alive with chick, close to the abandonment threshold.
#'   \item 4 = Alive without a chick.
#'   \item 5 = Dead.
#' }
#'
#' The function returns one of four feeding modes:
#' \itemize{
#'   \item 1 = Provisioning: the chick is attended and provisioned.
#'   \item 2 = Unattended: the chick is left unattended for part of the time step.
#'   \item 3 = Abandoned: the breeding attempt is abandoned.
#'   \item 4 = Dead: no strategy is required because the adult is dead.
#' }
#'
#' @return #' A tibble containing:
#' \itemize{
#'   \item \code{trips_n}: number of foraging trips.
#'   \item \code{foraging_h}: total time spent foraging (hours).
#'   \item \code{flying_h}: total time spent flying (hours).
#'   \item \code{forage_g}: total prey captured (g).
#'   \item \code{colony_h}: time spent at the colony (hours).
#'   \item \code{at_sea_h}: time spent at sea (hours).
#'   \item \code{feeding_mode}: behavioural strategy selected.
#' }
#' @importFrom dplyr slice_min
#' @seealso [calc_foragecapture()]
#' @examples
#' # Create a prey capture lookup table
#' tcapt <- calc_foragecapture(
#'   x0 = 260,
#'   h = 973,
#'   a = 4.369,
#'   maxt = 2160
#' )
#'
#' # Determine the foraging strategy for a healthy bird
#' out <- calc_strategy(
#'   BM_condition = 0.95,
#'   BM_adult_abdn = 0.70,
#'   fmode = 1,
#'   Fg = 100,
#'   tf = 0.25,
#'   tcapt = tcapt,
#'   maxf = 10,
#'   ts = 1,
#'   tc = 8,
#'   tm = 2,
#'   ttotal = 24
#' )
#' @export
#'
# > Adult birds are categorised into 5 behaviour groups
# 1 - Alive, with chick, [very healthy] <= condition <=1
# 2 - Alive, with chick, BM_adult_abdn <= condition < [very healthy]
# 3 - Alive, with chick, BM_adult_abdn <  condition
# 4 - Alive, no chick
# 5 - Dead.
# and there are 4 feeding modes
# 1 - Provisioning,
# 2 - Nest unattended,
# 3 - Nest abandoned,
# 4 - dead
calc_strategy <- function(BM_condition, BM_adult_abdn, fmode, Fg, tf, tcapt, maxf, ts, tc, tm, ttotal) {

  healthy <- BM_adult_abdn + 0.5*(1.0-BM_adult_abdn)

  if (fmode < 4) { # the bird is alive and needs a foraging strategy for the timestep...

    if (fmode < 3) { # the bird is feeding a chick

      # [Strategy 1] Can I capture all the required food while spending optimum time at the colony?

      # How long would it take to capture 100% of food requirement?
      fmins <- findInterval(Fg/1:maxf, tcapt$captured_g, all.inside = TRUE) + 1

      # if we have n flights, how long will it take to fly there and back through the day?
      # how long needed to capture all the food? Is this a valid strategy?

      #--> [former calc_optNf]

      Tbl <- tibble(n = seq(maxf), f = fmins) %>%
        dplyr::mutate(TFlighthrs = tf * n) %>%
        dplyr::mutate(TForagehrs = (f/60.0) * n) %>%
        dplyr::mutate(TTimehrs = TForagehrs + TFlighthrs + ts + tc) %>%
        dplyr::mutate(ValidN = TTimehrs < ttotal) %>%
        dplyr::filter(ValidN == TRUE)

      # Choose the best one
      chosen <- Tbl %>% dplyr::select(-c(f,ValidN,TTimehrs)) %>% dplyr::slice_min(n)

      if (nrow(chosen) > 0){ # YES Strategy 1 followed

        # This bird foraged successfully and attends the chick fully
        sparetime_h = pmax(0, ttotal - tc - ts - chosen$TForagehrs - chosen$TFlighthrs)

        strategy <- tibble(
          trips_n = chosen$n,
          foraging_h = chosen$TForagehrs,
          flying_h = chosen$TFlighthrs,
          forage_g = Fg,
          colony_h = tc + (0.5*sparetime_h),
          at_sea_h = ts + (0.5*sparetime_h),
          feeding_mode = 1
        )

        # This was successful, so return and go to the next bird
        return(strategy)
      }

      # NO! Try something else ...

      if (BM_condition >= healthy) {

        # [Strategy 2] Can I acquire some food while still spending optimum time at the colony?

        Tbl <- tibble(n = seq(maxf)) %>%
          dplyr::mutate(TFlighthrs = tf * n) %>%
          dplyr::mutate(TSpenthrs = TFlighthrs + ts + tc) %>%
          dplyr::mutate(TForagemins = pmax(0,floor(60.0*(ttotal - TSpenthrs)))) %>%
          dplyr::mutate(foragemins = pmax(1,floor(TForagemins / n))) %>%
          dplyr::mutate(TForagegms = n * tcapt$captured_g[foragemins]) %>%
          dplyr::mutate(TForagehrs = TForagemins / 60.0) %>%
          dplyr::mutate(ValidN1 = TForagegms > 0)  %>%
          dplyr::mutate(ValidN2 = TSpenthrs <= ttotal)  %>%
          dplyr::filter(ValidN1 == TRUE & ValidN2 == TRUE) %>%
          dplyr::select(-c(TSpenthrs, TForagemins, foragemins, ValidN1, ValidN2))

        # Choose the best one - maximise the food for the time allowed
        chosen <- Tbl %>%  dplyr::slice_max(TForagegms)  %>% dplyr::slice_min(n)

        if (nrow(chosen) > 0){ # YES Strategy 2 followed

          # This bird foraged successfully and attends the chick fully
          sparetime_h = pmax(0, ttotal - tc - ts - chosen$TForagehrs - chosen$TFlighthrs)

          strategy <- tibble(
            trips_n = chosen$n,
            foraging_h = chosen$TForagehrs,
            flying_h = chosen$TFlighthrs,
            forage_g = chosen$TForagegms,
            colony_h = tc + sparetime_h,
            at_sea_h = ts,
            feeding_mode = 1
          )

          # This was successful, so return and go to the next bird
          return(strategy)
        }
        # NO! Try something else ...
      }
    }

    # [Strategy 3] Can I capture all of the required food while spending less than optimum time at the colony?

    Tbl <- tibble(n = seq(maxf)) %>%
      dplyr::mutate(minspertrip = findInterval(Fg/n,tcapt$captured_g, all.inside = F) + 1) %>%
      dplyr::mutate(TForagehrs = n * minspertrip / 60.0) %>%
      dplyr::mutate(TFlighthrs = n * tf) %>%
      dplyr::mutate(TSpenthrs = TFlighthrs + TForagehrs + ts + tm) %>%
      dplyr::mutate(ValidN = TSpenthrs < ttotal) %>%
      dplyr::filter(ValidN == TRUE) %>%
      dplyr::select(-c(minspertrip, ValidN))

    # Choose the best one - minimising time away from nest
    chosen <- Tbl %>%  dplyr::slice_min(TSpenthrs)  %>% dplyr::slice_min(n) %>% dplyr::select(-TSpenthrs)

    if (nrow(chosen) > 0){ # YES Strategy 3 followed

      # This bird foraged successfully but spent less than optimum time at the nest
      sparetime_h = pmax(0, ttotal - tm - ts - chosen$TForagehrs - chosen$TFlighthrs)

      strategy <- tibble(
        trips_n = chosen$n,
        foraging_h = chosen$TForagehrs,
        flying_h = chosen$TFlighthrs,
        forage_g = Fg,
        colony_h = tm + sparetime_h,
        at_sea_h = ts,
        feeding_mode = ifelse(BM_condition >= BM_adult_abdn, 2, 3)
      )

      # This was successful, so return and go to the next bird
      return(strategy)
    }

    # NO! Try something else ...

    # [Strategy 4] Capture as much food as possible while spending minimum time at the colony

    Tbl <- tibble(n = seq(maxf)) %>%
      dplyr::mutate(TFlighthrs = n * tf) %>%
      dplyr::mutate(TSpenthrs = TFlighthrs + ts + tm) %>%
      dplyr::mutate(TForagemins = pmax(0,floor(60.0*(ttotal - TSpenthrs)))) %>%
      dplyr::mutate(foragemins = pmax(1,floor(TForagemins / n))) %>%
      dplyr::mutate(TForagegms = n * tcapt$captured_g[foragemins]) %>%
      dplyr::mutate(TForagehrs = TForagemins / 60.0) %>%
      dplyr::mutate(ValidN = TForagegms > 0)  %>%
      dplyr::filter(ValidN == TRUE) %>%
      dplyr::select(-c(TSpenthrs, TForagemins, foragemins, ValidN))

    # Choose the best one, maximising food
    chosen <- Tbl %>%  dplyr::slice_max(TForagegms)  %>% dplyr::slice_min(n)

    if (nrow(chosen) > 0){

      # This bird foraged successfully but spent less than optimum time at the nest
      sparetime_h = pmax(0, ttotal - tm - ts - chosen$TForagehrs - chosen$TFlighthrs)

      strategy <- tibble(
        trips_n = chosen$n,
        foraging_h = chosen$TForagehrs,
        flying_h = chosen$TFlighthrs,
        forage_g = chosen$TForagegms,
        colony_h = tm + sparetime_h,
        at_sea_h = ts,
        feeding_mode = ifelse(BM_condition >= BM_adult_abdn, 2, 3)
      )

      # This was successful, so return and go to the next bird
      return(strategy)
    }

    # NO!  catch-all - hopefully no birds reach here?
    strategy <- tibble(trips_n = NA, foraging_h = NA, flying_h = NA, forage_g = NA, colony_h = NA,
                       at_sea_h = NA, feeding_mode = 5 # Look for this as a sign of trouble!
    )
    return(strategy)

  } else {

    # No strategy needed, stay at fmode = 4
    strategy <- tibble(trips_n = NA, foraging_h = NA, flying_h = NA, forage_g = NA,
                       colony_h = NA, at_sea_h = NA, feeding_mode = 4)
    return(strategy)
  }

}

################################################################################
#' @title Calculate the time taken to forage required amount
#' @description Given the starting prey density and the competition for food, we
#'   need to estimate how long it would take a bird to gather the food it needs.
#'   Intake model: We assume that the relationship between prey quantity x and
#'   intake rate (-dx/dt) is of the form dx/dt = -ax/(h+x) for parameters a and
#'   h, the widely-used Michaelis-Menten model (Holling, 1959). This implies that the prey
#'   quantity at time t is equal to
#'
#'   x(t,x_0)=\{x:(x-x_0+at+hlog(x)-h log(x_0)) = 0\}
#'
#'   where x_0 denotes the prey quantity at time 0. This in turn implies that
#'   the total prey consumed by foraging up to time t is equal to
#'
#'   y(t,x_0) = x_0 - x(t,x_0)
#'
#'   The solution to this cannot be written down analytically, but it can be
#'   calculated numerically e.g. using a non-linear solver. This function uses
#'   the solver, nleqslv, to create a look-up table for a given x_0, a and h
#'   for a range of values of t.
#' @param x0 Prey quantity at time 0, g
#' @param a  Species maximum intake rate, kg/min
#' @param h  Species IR_half (modified to account for neighbours), kg/km2
#' @param maxt The maximum number of minutes available
#' @return A tibble with two variables 'tmin' and 'captured' and 'maxt' rows.
#' @examples
#' # Example prey patch
#' out <- calc_foragecapture(
#'   x0 = 260,
#'   h = 973,
#'   a = 4.369,
#'   maxt = 2160
#' )
#' @references
#' Holling, C. S. (1959). Some Characteristics of Simple Types of
#' Predation and Parasitism. \emph{The Canadian Entomologist},
#' 91(7), 385--398. doi:10.4039/Ent91385-7
#' @export
calc_foragecapture <- function(x0, h, a, maxt){

  # The function to solve
  fn <- function(x, x0, a, t, h) {
    if (x > 0) {
      x - x0 + a*t + h*log(x) - h*log(x0)
    } else {
      10^10
    }
  }

  # Make a blank look-up 'table' (vector)
  captured <- rep(0, maxt)

  # From 1 minute, calculate the food captured until it is all gone or we run
  # out of time for the full timestep,
  t <- 1
  while (captured[t] < x0 & t <= maxt) {

    # Find a solution
    sol <- nleqslv::nleqslv(x0, fn=fn, jac=NULL, x0 = x0, a = a, t = t, h = h)

    # Action depending on the returned code
    if (sol$termcd < 3) {
      captured[t] <- x0 - sol$x
    } else {
      captured[t] <- x0
    }
    t <- t + 1
  }

  # If all the prey has been captured before maxt, just fill in the rest
  if (t < maxt) captured[-seq_len(t)] <- x0

  # Return the look-up
  tibble(tmin = seq_len(maxt), captured_g = captured)

}

####################################################################################
################### other functions ################################################
####################################################################################
################################################################################
#' @title Sample forage destinations
#'
#' @description For a given seabird normalised suitability raster, sample
#'   foraging destinations according to the supplied probabilities.
#'
#' @param Prast A raster containing values that sum to 1, e.g. bird suitability
#'   map for a colony or SPA.
#' @param Nb The number of birds (rows in the output tibble)
#' @param Nt The number of destinations per bird, usually the number of
#'   timesteps in a season.
#'
#' @return A tibble with Nb rows and Nt columns, containing grid cell numbers
#'   drawn from the supplied raster.
#' @noRd
select_destinations <- function (Prast, Nb, Nt) {

  x <- getValues(Prast)

  # set any NA cells in raster to zero
  x[is.na(x)]<-0

  # Sample full season with replacement
  cells <- sample(length(x), size = Nb*Nt, replace = T, prob = x)
  cells <- matrix(cells, nrow = Nb, byrow = TRUE)
  cells <- tibble::as_tibble(cells, .name_repair = "minimal")

  # Add names
  names(cells) <- paste0("t", seq(Nt))

  return(cells)
}

################################################################################
#' @title Define the appropriate stage for the bird
#'
#' @description For future expansion: SeabORD currently models only the
#'   chick-rearing stage of the year but there is a plan to extend this to
#'   other part of the season. Not yet known how this will be modelled but this
#'   function is set up to be called per bird so could include chick weight or
#'   day since hatching for example. Alternative would be to change the 'stage'
#'   for all birds based only on day of the year.
#'
#' @param ss Current stage. Could be an integer or a string.
#'
#' @return A revised value. Could be an integer or a string.
#'
#' @examples
#'   calc_season_stage(0)
#'
#' @noRd
calc_season_stage <- function(ss) {

  out <- 0

  out
}

#' @title Simulate the number of birds in each grid cell with and without wind farms
#'
#' @description Simulate the number of birds in each grid cell at each time step in the baseline
#'   and with one or more wind farms present, under a specific displacement rate
#' @param absabunmap Map of absolute abundance, as a vector: a vector of length equal to the number
#'   of grid cells giving the number of birds in each grid cell
#' @param fpmat A matrix, with number of rows equal to the length of `absabunmap` (i.e. the number
#'   of grid cells) and number of columns containing the  number of wind farms, which specifies the
#'   situation of each grid cell in relation to the wind farm in that column: values are either 0,
#'   1 or 2. "1" indicates grid cells that are contained in the wind farm footprint+border. "2"
#'   indicates grid cells that are in the surrounding buffer into which displacement occurs. "0"
#'   indicates grid cells that are in neither of these.
#' @param disprate Displacement rate: proportion of birds displaced, a numeric value between 0 and 1.
#' @param ntimesteps Number of time steps; a positive integer
#'
#' @importFrom stats rmultinom
#'
#' @return A list with two entries, `baseline` and `impacted`, each of which is a matrix of
#'   dimension `[length(absabunmap), ntimesteps]`, indicating the number of birds simulated to be
#'   in each grid cell in each timestep, both without wind farms (`baseline`) and with wind farms
#'   (`impacted`)
#'
#' @noRd

sim_nbirds_wwf_pertimestep <- function(absabunmap, fpmat, disprate, ntimesteps){

  nsimbirds <- as.list(NULL) ## initialize output

  ## BASELINE

  ## BL1. Sum of baseline abundance map
  abunmapsum <- sum(absabunmap)

  ## BL2. Normalized abundance map
  relabunmap <- absabunmap / abunmapsum

  ## BL3. Baseline multinomial simulation - *STOCHASTIC*
  nsimbirds$baseline <- rmultinom(ntimesteps, size = abunmapsum, prob = relabunmap)

  ## IMPACTED

  nsimbirds$impacted <- nsimbirds$baseline ## initialize "impacted" to be the same as "baseline"

  if (any(fpmat > 0)) {

    for(i in 1:ncol(fpmat)){ ## loop over wind farms, and calculate effect of displacement for each windfarm in turn

      ## DETERMINISTIC CALCULATIONS

      ## ID1. Identifiers of which grid squares are within footprint+border of this wind farm
      idi_from <- which(fpmat[,i] == 1)

      ## ID2. Identifiers of which grid squares are within buffer of this wind farm
      idi_to <- which(fpmat[,i] == 2)

      ## ID3. Total of competition map across the buffer for this windfarm
      sto <- sum(relabunmap[idi_to])

      ## CHECK
      if(sto == 0){ stop("Total mean abundance from non-focal colonies in buffer area is zero - displacement calculations impossible") }

      ## ID4. Probability for each destination grid cell for birds that are displaced from each wind farm
      ## This is simply the competition map, but only for the buffer, and rescaled to sum to one across the buffer
      pto <- relabunmap[idi_to] / sto

      for(j in 1:ntimesteps){

        ## STOCHASTIC SIMULATIONS

        ## IS1 - *STOCHASTIC*. Simulate number of birds to displace away from each cell in this windfarm footprint+border

        nfrom <- rbinom(n = length(idi_from), size = nsimbirds$baseline[idi_from,j], prob = disprate)

        ## IS2. Calculate total number of birds displaced away from this windfarm at this timestep
        nfrom_total <- sum(nfrom)

        if(nfrom_total > 0){

          ## IS3 - *STOCHASTIC*. Simulate number of birds displaced into each grid cell at each time step
          ## "pto" = probability of going to each grid cell if displaced
          ## "nfrom_total" = total number of birds displaced away from the windfarm at each timestep
          nto <- as.numeric(rmultinom(n = 1, size = nfrom_total, prob = pto))

          if(sum(nfrom) != sum(nto)){ stop("Error! - sums of 'nfrom' and 'nto' do not match") }

          ## IS4. Removed displaced birds from numbers for the cell they are leaving
          nsimbirds$impacted[idi_from,j] <- nsimbirds$impacted[idi_from,j] - nfrom

          ## IS5. Add displaced birds on to the numbers for the cell they are moving to
          nsimbirds$impacted[idi_to,j] <- nsimbirds$impacted[idi_to,j] + nto

        }
      }
    }
  }
  ## Output

  nsimbirds
}


################################################################################
#' @title Simulate from a set of bernoulli distributions
#'
#' @description Simulate the elements of a matrix y[i,j] ~ Binomial(n[i,j], p)
#' @param x A matrix of sample sizes
#' @param p Probability of success; a single numeric value between 0 and 1
#'
#' @return A matrix of simulated values, of the same dimension as `x`
#'
#' @noRd
rbinom_mm <- function(x, p){
  t(apply(x, 1,
          function(u, m, p){ rbinom(n = length(u), size = u, prob = p)},
          p = p)) }




