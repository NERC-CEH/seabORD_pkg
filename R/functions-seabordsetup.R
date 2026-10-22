####################################################################################################
## FUNCTIONS for SeabORD v2.0.x
## Author: UKCEH
## Date: From May 2020
##

# The functions in this file run once either at the start of a set of simulations or at the start of
# a season. They set starting values.
#
# set_seedvalues
# set_medianprey
# set_initialbirdtype
# set_initialbirdstate
# set_initialchickstate

################################################################################
#' @title Set the seeds needed for reproducibility
#'
#' @description SeabORD includes stochastic methods that rely on the generation
#' of random numbers. To make the simulations reproducible we start the random
#' number sequence using a seed. This function used the main, user-supplied seed
#' to create a table of other seeds that are used to initialise other functions.
#' Each stream in the simulation uses a separate seed so that, for example,
#' changes to the number of live birds does not affect the sequence in another
#' part of the code. Adding new streams or changing the number of replicates
#' should not change previous runs significantly (assuming the same starter
#' seed).
#'
#' @param initialseed The user-supplied main seed.
#' @param nstreams The number of different streams required.
#' @param Nreplicates The number simulation pairs in the run.
#' @return A tibble with integer seed values for each stream and replicate
#'   simulation pair.
#'
#' @examples
#'   set_seedvalues(2327, 10, 5)
#'
#' @export
#'

set_seedvalues <- function(initialseed, nstreams, Nreplicates) {

  # Initialise the main random number sequence with user-supplied value
  set.seed(initialseed)

  # Number of different random streams we use in the model
  #  1. Master seed for replicate
  #  2. set_initialbirdtype
  #  3. set_initialbirdstate
  #  4. set_initialchickstate
  #  5. set_flightlist
  #  6. final survival
  #  7. Attendance
  #  8. Median prey values
  #  9. Collision risk
  # 10.

  # Fill a tibble with integer seed values
  seedmat <- matrix(0, nstreams, Nreplicates + 1)
  seedmat[, 1] <- sample.int(10000, size = nstreams, replace = FALSE)
  for (i in 1:nstreams) {
    set.seed(seedmat[i, 1, drop = TRUE])
    seedmat[i, 2:(Nreplicates + 1)] <- sample.int(10000, size = Nreplicates, replace = FALSE)
  }
  # Convert to a tibble and name the columns
  seedmat <- tidyr::as_tibble(seedmat, .name_repair = "minimal")
  names(seedmat) <- c("seed", paste0("run", 1:Nreplicates))

  # Use seedmat[seedstream, paste0("run",simrun), drop=TRUE] to access a seed value
  seedmat
}

################################################################################
#' @title Create individual seabirds for the simulation
#'
#' @description This function creates a tibble holding one row per individual
#'   seabird. The values stored in this tibble do not change through the
#'   simulation, they describe characteristics that remain constant like species,
#'   sex and home colony.
#'
#' @param seed The seed used to ensure results are reproducible.
#' @param Colonies A tibble containing information about the colonies of
#'   interest in this simulation. Requires "code" with the colony codes and
#'     "ObsNpairs"
#' @param thisSpecies The two-letter code identifying the species of interest
#'   (only works with one species in v2.0.0).
#' @param Pwfde The probability of displacement across the population if
#'   encountering an ORD,where 0 means no displacement.
#' @param Pwfbe The probability across the population of experiencing a barrier
#'   effect when encountering an ORD. Only applies to birds who are displacement
#'   susceptible.
#' @return A list containing two tibbles: the tidy data and the metadata for
#' documentation.
#'
#' @importFrom tibble tibble
#'
#' @examples
#' set_initialbirdtype(231, tibble::tibble(code="UK9004171", ObsNpairs=100), "Kw", 0.3, 1.0)
#'
#' @export
#'

set_initialbirdtype <- function(seed, Colonies, thisSpecies, Pwfde, Pwfbe) {

  # Reset the random number generator to ensure repeatability
  set.seed(seed)

  # Total number of individual birds in this run
  Npairs <- Colonies %>% dplyr::pull("ObsNpairs")
  N <- 2 * sum(Npairs)

  # Each bird has a unique ID and a pair ID
  BirdID <- 1:N
  Species <- rep(thisSpecies, N)
  PairID <- rep(1:sum(Npairs), each = 2)

  # Assign male or female (may be used in future release)
  MF <- rep(c("M", "F"), sum(Npairs))

  # Colony assignment
  colony <- rep(Colonies$code, times = 2 * Npairs)

  # ORD susceptibility
  # WFDE (wind farm displacement effect) 1=avoid, 0=ignore
  # WFBE (wind farm barrier effect) 1=avoid, 0=ignore

  # Randomly assign the appropriate Wind Farm Displacement Effect (WFDE)
  # where a WFDE of 0 means the bird ignores the WF
  wfde <- rbinom(N, 1, Pwfde)
  N1 <- sum(wfde)

  # Randomly assign the appropriate Wind Farm Barrier Effect (WFBE) to birds
  # that are displacement-susceptible, where a WFBE of 0 means the bird ignores the WF
  wfbe <- rep(0, N)
  wfbe[wfde == 0] <- 0
  wfbe[wfde == 1] <- rbinom(N1, 1, Pwfbe)

  # Create the tibble
  BirdType <- tibble::tibble(BirdID, Species, colony, PairID, MF, wfde, wfbe)

  # Create the metadata tibble for the documentation
  BirdTypeTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "BirdID", "[Key] Individual unique identifier", "",
    "Species", "Two-letter code to identify the species", "",
    "colony", "Three-letter code to identify the home colony", "",
    "PairID", "Identification number for the pair, one male and one female", "",
    "MF", "A letter, M or F, to indicate sex of this bird", "",
    "wfde", "A flag, 0/1, to indicate if this bird is susceptible to displacement", "",
    "wfbe", "A flag, 0/1, to indicate if this bird is susceptible to barrier effects", ""
  )

  # Return a list containing the human-friendly and the code-friendly tibbles
  list("data" = BirdType, "metadata" = BirdTypeTable)
}

################################################################################
#' @title Set initial values for individual birds
#'
#' @description Set the initial values for the birds in the simulation using
#'  species parameters. All birds begin the simulation alive, part of a pair
#'  with a live chick and in good condition.
#'
#' @param seed A seed to ensure reproducibility (numeric).
#' @param N The number of birds to generate (numeric).
#' @param tstep The length of the timestep in hours (numeric).
#' @param BM_adult_mn Adult body mass, mean, g (numeric).
#' @param BM_adult_sd Adult body mass, standard deviation (numeric).
#' @param adult_DEE_mn Adult daily energy expenditure, mean, kJ (numeric).
#' @param adult_DEE_sd Adult daily energy expenditure, standard deviation, kJ (numeric).
#' @param assim_eff Assimilation efficiency (numeric).
#' @return A list containing a tibble holding the individual bird states at time
#'   t0 and a tibble with metadata for the documentation.
#'
#' @importFrom stats rnorm
#'
#' @examples
#' set_initialbirdstate(123, 10, 24, 372.7, 33.6, 802, 196, 0.74)
#'
#' @export
#'

set_initialbirdstate <- function(seed, N, tstep, BM_adult_mn, BM_adult_sd,
                                 adult_DEE_mn, adult_DEE_sd, assim_eff) {

  # Reset the random number generator to ensure repeatability
  set.seed(seed)

  BirdID <- 1:N
  BM_adult_t0 <- rnorm(N, BM_adult_mn, BM_adult_sd)
  BM_adult <- BM_adult_t0
  BM_condition <- rep(1, N)
  is_alive <- rep(1, N)
  is_chick_alive <- rep(1, N)
  feeding_mode <- rep(1, N)
  season_stage <- rep(0, N)
  Ereq_adult <- (rnorm(N, adult_DEE_mn, adult_DEE_sd) * (tstep / 24)) / assim_eff
  CoD <- rep("none", N)

  # Create the tibble for the code
  BirdState <- tibble::tibble(
    BirdID, BM_adult_t0, BM_adult, BM_condition, is_alive, season_stage,
    is_chick_alive, feeding_mode, Ereq_adult, CoD
  )

  # Create the metadata tibble for the documentation
  BirdStateTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "BirdID", "[Key] Individual unique identifier", "",
    "BM_adult_t0", "Body mass at time t0", "g",
    "BM_adult", "Body mass at time t", "g",
    "BM_condition", "Relative condition (0-1)", "",
    "is_alive", "State at time t, dead or alive", "0/1",
    "season_stage", "Breeding season stage at time t (TBC)", "",
    "is_chick_alive", "Individual has live chick at time t", "0/1",
    "feeding_mode", "Feeding mode at time t (1=Provisioning, 2=Nest unattended, 3=Nest abandoned, 4=dead)", "",
    "Ereq_adult", "Energy required t+1 based on expenditure at time t", "kJ",
    "CoD", "Cause of death", ""
  )

  # Return a list containing the human-friendly and the code-friendly tibbles
  list("data" = BirdState, "metadata" = BirdStateTable)
}

################################################################################
#' @title Set initial values for individual chicks
#'
#' @description Set the initial values for the chicks in the simulation using
#'  species parameters. All chicks begin the season alive and in good
#'  condition.
#'
#' @param seed A seed to ensure reproducibility (numeric).
#' @param N The number of chicks to generate (numeric).
#' @param tstep The length of the time step in hours (numeric).
#' @param BM_chick_mn Chick body mass, mean, g (numeric).
#' @param BM_chick_sd Chick body mass, standard deviation (numeric).
#' @param chick_DER Chick daily energy requirement, mean, kJ (numeric).
#' @param assim_eff Assimilation efficiency (numeric).
#' @return A list containing two tibbles; one holding the individual chick
#'   states at time t0 and the metadata for the documentation.
#'
#' @importFrom stats rnorm
#'
#' @examples
#' set_initialchickstate(123, 10, 24.0, 36.0, 2.2, 525.7, 0.74)
#'
#' @export
#'

set_initialchickstate <- function(seed, N, tstep, BM_chick_mn, BM_chick_sd, chick_DER, assim_eff) {

  # Reset the random number generator to ensure repeatability
  set.seed(seed)

  # Number of chicks (one per pair of adults)
  Nc <- N / 2

  PairID <- 1:Nc
  BM_chick <- rnorm(Nc, BM_chick_mn, BM_chick_sd)
  BM_condition <- BM_chick / BM_chick_mn
  is_chick_alive <- rep(1, Nc)
  Ereq_chick <- (rep(chick_DER, Nc) * (tstep / 24)) / assim_eff
  CoD <- rep("none", Nc)

  # Create the tibble for the code
  ChickState <- tibble::tibble(PairID, BM_chick, BM_condition, is_chick_alive, CoD, Ereq_chick)

  # Create the metadata tibble for the documentation
  ChickStateTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "PairID", "[Key] Individual unique identifier", "",
    "BM_chick", "Body mass at time t", "g",
    "BM_condition", "Relative condition (0-1)", "",
    "is_chick_alive", "State at time t, dead or alive", "0/1",
    "CoD", "Cause of death", "",
    "Ereq_chick", "Energy required at time t", "kJ"
  )

  # Return a list containing the human-friendly and the code-friendly tibbles
  list("data" = ChickState, "metadata" = ChickStateTable)
}


