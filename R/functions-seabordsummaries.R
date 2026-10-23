####################################################################################################
## FUNCTIONS for SeabORD v2.0.x
## Author: UKCEH
## Date: From January 2021
##

# The functions in this file are for handling summary information, seasonal sums etc
#
# >> calc_summaryby()
# x calc_finalby()
# > dasummary()
# > dcsummary()
# > dfsummary()
# > yasummary()
# > create_yearsheet()
# > create_stepsheeta()
# > create_stepsheetc()
# > create_stepsheetf()
# > update_steplist()
# > create_flightsheet()
# > create_summarylist()
# > update_summarylist()
# > update_flightrecord()

################################################################################
#' @title Summarise the season pair for adult birds
#'
#' @description At the end of the season pair of runs, calculate summary
#'   values for adult bird variables.
#'
#' @param data Tibble holding variables from both seasons. Can be grouped.
#' @param by Grouping variable(s). Will be added to existing grouping.
#' @param r Replicate run number
#'
#' @importFrom dplyr group_by group_vars across contains n relocate
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom tibble tibble
#'
#' @return A list; adults and survival. Survival has 3 tibbles for poor,
#'   moderate and good seasons.
#'
#' @noRd

calc_summaryby <- function(data, by = NULL, r){

  # Helper functions
  pcsurv <- NA; ndead <- NA
  pcsurv <- function(x,y){100.0*sum(x, na.rm = TRUE)/y}
  ndead <- function(x,y){y - sum(x, na.rm = TRUE)}

  # Group further?
  data <- data %>% group_by({{by}}, .add = TRUE)
  gvars <- group_vars(data)

  # Body mass summary
  adults <- data %>%
    dplyr::summarise(
      N = n(),
      across(c(contains("BM")),
             .fns = list(mn = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)),.names = "{.col}.{.fn}"),
      .groups = "drop"
    ) %>% dplyr::mutate(Rep = r) %>% relocate(Rep)

  # P(survival) and Survived summary
  survival <- map(c("_1","_2","_3"), ~ data %>% dplyr::select(contains({{.x}}), all_of(gvars)) %>%
                    dplyr::summarise(
                      N = n(),
                      across(contains("pSurvival"),
                             .fns = list(mn = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)), .names = "{.col}.{.fn}"),
                      across(contains("Survived"),
                             .fns = list(pc = pcsurv, ndead = ndead), n(),.names = "{.col}.{.fn}"),
                      .groups = "drop"
                    )
  )
  names(survival) <- c("1", "2", "3")

  # Calculate the additional mortality per group
  survival <- map(names(survival), ~ survival[[.x]] %>%
                    dplyr::mutate(AdditionalMort =
                                    100*(.data[[paste0("Survived_",.x,".scen.ndead")]] -
                                           .data[[paste0("Survived_",.x,".base.ndead")]])/N, .after = N) %>%
                    dplyr::mutate(Rep = r) %>% relocate(Rep)
  )

  # Rename for clarity: 1:3 = 'poor', 'moderate' and 'good' years
  names(survival) <- c("poor", "mod", "good")

  # Return nested list
  list(adults = adults, survival = survival)
}

################################################################################
#' @title Summarise a time step through the season for adult birds
#'
#' @description At the end of a time step record summary values for adult bird
#'   variables.
#'
#' @param data BirdState$data
#' @param r The replicate number
#' @param s The season type, base or scen
#' @param t The current time step
#' @param p The median prey for the year
#'
#' @importFrom dplyr n starts_with ends_with across everything bind_cols filter
#' @importFrom forcats fct_relabel fct_count
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom tibble tibble
#'
#' @return A tibble with one row (or empty tibble)
#'
#' @noRd

dasummary <- function (data, r, s, t, p) {

  npop <- nrow(data)

  if (nrow(data) > 0) {

    # Summarise
    d1 <- data %>% dplyr::filter(is_alive == 1) %>% dplyr::summarise(
      N_alive_ad = n(),
      N_dead_ad = npop - n())

    d2 <- data %>% dplyr::filter(is_alive == 1) %>%
      dplyr::select(c(starts_with("BM"),
                      ends_with("_n"), ends_with("_h"), ends_with("_g"))) %>%
      dplyr::summarise(
        across(everything(),
               .fns = list(mn = \(x) mean(x, na.rm = TRUE),
                           sd = \(x) sd(x, na.rm = TRUE)),
               .names = "{.col}.{.fn}"))

    d3 <- data$feeding_mode %>%
      forcats::as_factor() %>%
      fct_relabel(~ paste0("mode.", .x)) %>%
      fct_count(sort = FALSE, prop = F) %>%
      pivot_wider(names_from = f, values_from = n)

    Tbl <- bind_cols(tibble(Rep=r,Prey=p,Season=s,t=t),d1,d2,d3) %>%
      dplyr::mutate(AdultsSurvivingBS = N_alive_ad/npop)

    # Return
    Tbl

  } else { # return an empty tibble

    # Return
    tibble()

  }

}

################################################################################
#' @title Summarise each time step through the season for chicks
#'
#' @description At the end of a time step, record summary values for
#'   chick variables.
#'
#' @param data ChickState$data
#' @param r The replicate number
#' @param s The season type, base or scen
#' @param t The current time step
#' @param p The median prey for the year
#'
#' @importFrom dplyr bind_cols contains n across
#' @importFrom forcats fct_relabel fct_count
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom stats sd
#' @importFrom tibble tibble
#'
#' @return A tibble with one row (or empty tibble)
#' @noRd

dcsummary <- function (data, r, s, t, p) {

  if (nrow(data) > 0) {

    # Summarise
    d1 <- data %>% dplyr::filter(is_chick_alive == 1) %>%
      dplyr::summarise(
        N_alive_ch = n(),
        N_dead_ch = nrow(data) - n())

    d2 <- data %>% dplyr::filter(is_chick_alive == 1) %>%
      dplyr::summarise(
        across(contains("BM"),
               .fns = list(mn = \(x) mean(x, na.rm = TRUE),
                           sd = \(x) sd(x, na.rm = TRUE)),
               .names = "{.col}.{.fn}"))

    d3 <- data$CoD %>%
      forcats::as_factor() %>%
      fct_relabel(~ paste0("CoD.", .x)) %>%
      fct_count(sort = FALSE, prop = F) %>%
      pivot_wider(names_from = f, values_from = n)

    Tbl <- bind_cols(tibble(Rep=r, Prey=p, Season=s, t=t), d1, d2, d3) %>%
      dplyr::mutate(ChicksPerNest = N_alive_ch/nrow(data))

    # Return
    Tbl

  } else { # return an empty tibble

    # Return
    tibble()

  }
}

################################################################################
#' @title Summary of flight information at the end of each season
#'
#' @description At the end of each breeding season, record summary values for
#'   flight variables.
#'
#' @param data FlightRecord$data
#' @param r The replicate number
#' @param s The season type, base or scen
#' @param t The current time step
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols contains n across
#' @importFrom tibble tibble
#'
#' @return A tibble with one row (or empty tibble)
#' @noRd

dfsummary <- function (data, r, s, t) {

  data <- data %>% dplyr::filter(!is.na(Tot_basickm))

  if (nrow(data) > 0) {

    # Summarise
    d1 <- data %>%
      dplyr::summarise(
        across(contains("Tot"),
               .fns = list(mn = \(x) mean(x, na.rm = TRUE),
                           sd = \(x) sd(x, na.rm = TRUE)),
               .names = "{.col}.{.fn}"))

    d2 <- data %>%
      dplyr::summarise(
        across(contains("GT0"),
               .fns = list(sm = \(x) sum(x, na.rm = TRUE)),
               .names = "{.col}.{.fn}"))

    # d2 <- data$DispGT0 %>%
    #     forcats::as_factor() %>%
    #     fct_relabel(~ paste0("Disp.", .x)) %>%
    #     fct_count(sort = TRUE, prop = F) %>%
    #     pivot_wider(names_from = f, values_from = n)
    #
    # d3 <- data$BarrGT0 %>%
    #     forcats::as_factor() %>%
    #     fct_relabel(~ paste0("Barr.", .x)) %>%
    #     fct_count(sort = TRUE, prop = F) %>%
    #     pivot_wider(names_from = f, values_from = n)

    Tbl <- bind_cols(tibble(Rep=r, Season=s, t=t), d1, d2)

    # Return
    Tbl

  } else { # return an empty tibble

    tibble()

  }
}

################################################################################
#' @title Summarise a year for adult birds
#'
#' @description At the end of a simulated year record summary values for
#'   individual adult bird variables.
#'
#' @param data A grouped tibble with a row for each adult bird holding body mass
#'   and survival values.
#' @param r The replicate number
#' @param s The season type, base or scen
#' @param p The median prey for the year
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols contains n across
#' @importFrom tibble tibble
#'
#' @return A tibble with one row (or empty tibble)
#' @noRd

yasummary <- function (data, r, s, p) {

  npop <- nrow(data)
  data <- data %>% dplyr::filter(is_alive == 1)

  if (nrow(data) > 0) {

    # Summarise
    d1 <- data %>% dplyr::summarise(
      N = n(),
      BM_adult_t0.mn = mean(BM_adult_t0, na.rm=T),
      BM_adult_t0.sd = sd(BM_adult_t0, na.rm=T),
      BM_adult.mn = mean(BM_adult, na.rm=T),
      BM_adult.sd = sd(BM_adult, na.rm=T),
      Survived_poor = sum(Survived_1),
      Survived_modr = sum(Survived_2),
      Survived_good = sum(Survived_3),
      .groups = "drop")

    Tbl <- bind_cols(tibble(Rep = r, Prey = p, Season = s), d1) %>%
      dplyr::mutate(AdultsSurvivingYr = Survived_modr/npop)

    # Return
    Tbl

  } else { # return an empty tibble

    # Return
    tibble()

  }

}

################################################################################
#' @title Create a blank log sheet for recording the season summary for adults
#'
#' @description At the end of each season, SeabORD records summary values for
#'   adult bird variables, grouped by colony, status (alive or dead) and the
#'   status of the chick
#'
#' @param bycol Include output grouped by colony? (Logical)
#' @param bych Grouped by status of chick? (logical)
#'
#' @importFrom magrittr %>%
#'
#' @return A list containing two tibbles; one holding the individual blank
#' recording sheet and one for the metadata.
#'
#' @examples
#'   create_stepsheeta()
#' @noRd
create_stepsheeta <- function(bycol = FALSE, bych = FALSE) {

  # Tibble
  Adults <- tibble::tibble(
    Rep = numeric(),
    Prey = numeric(),
    Season = character(),
    t = numeric(),
    N_alive_ad = numeric(),
    N_dead_ad = numeric(),
    BM_adult_t0.mn = numeric(),
    BM_adult_t0.sd = numeric(),
    BM_adult.mn = numeric(),
    BM_adult.sd = numeric(),
    BM_condition.mn = numeric(),
    BM_condition.sd = numeric(),
    trips_n.mn = numeric(),
    trips_n.sd = numeric(),
    flying_h.mn = numeric(),
    flying_h.sd = numeric(),
    foraging_h.mn = numeric(),
    foraging_h.sd = numeric(),
    colony_h.mn = numeric(),
    colony_h.sd = numeric(),
    at_sea_h.mn = numeric(),
    at_sea_h.sd = numeric(),
    forage_g.mn = numeric(),
    forage_g.sd = numeric(),
    mode.1 = numeric(),
    mode.2 = numeric(),
    mode.3 = numeric(),
    mode.4 = numeric(),
    AdultsSurvivingBS = numeric(),
    .rows = 0
  )

  if (bych) {
    Adults <- Adults %>%
      dplyr::mutate(is_parent = numeric()) %>%
      dplyr::relocate(is_parent, .after = t)
  }

  if (bycol) {
    Adults <- Adults %>%
      dplyr::mutate(colony = character()) %>%
      dplyr::relocate(colony, .after = t)
  }

  # Metadata
  AdultsTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number", "",
    "Prey", "Median prey available", "g/area",
    "Season", "Season type, baseline or scenario", "",
    "t", "Time step", "",
    "colony", "Grouped by colony", "",
    "N_alive_ad", "Number of live adult birds in group", "",
    "N_dead_ad", "Number of dead adult birds in group", "",
    "BM_adult_t0.mn", "Initial adult bird body mass, mean", "g",
    "BM_adult_t0.sd", "Initial adult bird body mass, standard deviation", "g",
    "BM_adult.mn", "Adult body mass (live birds only), mean", "g",
    "BM_adult.sd", "Adult body mass (live birds only), standard deviation", "g",
    "BM_condition.mn", "Adult body condition relative to initial, mean", "g",
    "BM_condition.sd", "Adult body condition relative to initial, standard deviation", "g",
    "is_alive", "Adults alive or not", "(1 or 0)",
    "is_parent", "Does the adult have a live chick?", "(1 or 0)",
    "trips_n.mn", "Number of flights per day, mean", "",
    "trips_n.sd", "Number of flights per day, standard deviation", "",
    "flying_h.mn", "Time spent flying per time step, mean", "hours",
    "flying_h.sd", "Time spent flying per time step, standard deviation", "hours",
    "foraging_h.mn", "Time spent foraging per time step, mean", "hours",
    "foraging_h.sd", "Time spent foraging per time step, standard deviation", "hours",
    "colony_h.mn" , "Time spent at the colony per time step, mean", "hours",
    "colony_h.sd", "Time spent at the colony per time step, standard deviation", "hours",
    "at_sea_h.mn", "Time spent at sea per time step, mean", "hours",
    "at_sea_h.sd", "Time spent at sea per time step, standard deviation", "hours",
    "forage_g.mn", "Total forage acquired per time step, mean", "g",
    "forage_g.sd", "Total forage acquired per time step, standard deviation", "g",
    "nchicks", "Number of live chicks in this group","",
    "mode.1", "Number of birds in this group actively feeding a chick", "",
    "mode.2", "Number of birds in this group not fully attending nest", "",
    "mode.3", "Number of birds in this group who have abandoned nest", "",
    "mode.4", "Number of birds in this group who are dead (as a check on totals)", "",
    "AdultsSurvivingBS","Proportion of adult birds surviving the breeding season",""
  )

  AdultsTable <- AdultsTable %>% dplyr::filter(VarName %in% names(Adults))

  # Return - list containing the human-friendly and the code-friendly tibbles
  list(data = Adults, metadata = AdultsTable)
}

################################################################################
#' @title Create a blank log sheet for recording the season summary for chicks
#'
#' @description At the end of each season, SeabORD records summary values for
#'   chick variables, grouped by colony, status (alive or dead) and cause of
#'   death.
#'
#' @param bycol Include output grouped by colony? (Logical)
#'
#' @importFrom magrittr %>%
#'
#' @return A list containing two tibbles; one holding the individual blank
#'   recording sheet and one for the metadata.
#'
#' @examples
#'   create_stepsheetc()
#' @noRd
create_stepsheetc <- function(bycol = FALSE) {

  chicks <- tibble::tibble(
    Rep = numeric(),
    Prey = numeric(),
    Season = character(),
    t = numeric(),
    N_alive_ch = numeric(),
    N_dead_ch = numeric(),
    BM_chick.mn = numeric(),
    BM_chick.sd = numeric(),
    BM_condition.mn = numeric(),
    BM_condition.sd = numeric(),
    CoD.none = numeric(),
    CoD.killed = numeric(),
    CoD.starved = numeric(),
    CoD.unattended = numeric(),
    CoD.parentdead = numeric(),
    CoD.abandoned = numeric(),
    CoD.other = numeric(),
    ChicksPerNest = numeric(),
    .rows = 0
  )

  if (bycol) {
    chicks <- chicks %>%
      dplyr::mutate(colony = character())  %>%
      dplyr::relocate(colony, .after = t)
  }

  chicksTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number", "",
    "Prey", "Median prey available", "g/area",
    "Season", "Season type, baseline or scenario", "",
    "t", "Time step", "",
    "colony", "Grouped by colony", "",
    "N_alive_ch", "Number of live chicks in group", "",
    "N_dead_ch", "Number of chicks in group", "",
    "BM_chick.mn", "Chick body mass, mean", "g",
    "BM_chick.sd", "Chick body mass, standard deviation", "g",
    "BM_condition.mn", "Chick body condition, mean", "",
    "BM_condition.sd", "Chick body condition, standard deviation", "",
    "CoD.none", "Cause of death: none, chick is alive", "",
    "CoD.killed", "Cause of death: killed (predated)", "",
    "CoD.starved", "Cause of death: body mass too low", "",
    "CoD.unattended", "Cause of death: parents absent", "",
    "CoD.parentdead", "Cause of death: at least one parent dead", "",
    "CoD.abandoned", "Cause of death: abandoned by parents", "",
    "CoD.other", "Cause of death: other mishap (e.g. flooding)", "",
    "ChicksPerNest","Mean number of chicks per nest surviving",""
  )

  chicksTable <- chicksTable %>% dplyr::filter(VarName %in% names(chicks))

  # Return a list containing the human-friendly and the code-friendly tibbles
  list(data = chicks, metadata = chicksTable)
}

################################################################################
#' @title Create a blank log sheet for recording the season summary for flights
#'
#' @description At the end of each season, SeabORD records summary values for
#'   flights, optionally by colony.
#'
#' @param bycol Include output grouped by colony? (Logical)
#'
#' @importFrom magrittr %>%
#'
#' @return A list containing two tibbles; one holding the individual blank
#'   recording sheet and one for the metadata.
#'
#' @examples
#'   create_stepsheetf()
#' @noRd

create_stepsheetf <- function(bycol = FALSE) {

  flights <- tibble::tibble(
    Rep = numeric(),
    Season = character(),
    TotN_C_risk.mn = numeric(),
    TotN_C_risk.sd = numeric(),
    TotN_None.mn = numeric(),
    TotN_None.sd = numeric(),
    TotN_D_only.mn = numeric(),
    TotN_D_only.sd = numeric(),
    TotN_B_only.mn = numeric(),
    TotN_B_only.sd = numeric(),
    TotN_BD.mn = numeric(),
    TotN_BD.sd = numeric(),
    TotN_trips.mn = numeric(),
    TotN_trips.sd = numeric(),
    Tot_basickm.mn = numeric(),
    Tot_basickm.sd = numeric(),
    Tot_extrakm.mn = numeric(),
    Tot_extrakm.sd = numeric(),
    DispGT0.sm = numeric(),
    BarrGT0.sm = numeric(),
    .rows = 0
  )

  if (bycol) {
    flights <- flights %>%
      dplyr::mutate(colony = character())  %>%
      dplyr::relocate(c(Season, colony))
  }

  flightsTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number","",
    "Season", "Season type, baseline or scenario", "",
    "colony", "Grouped by colony", "",
    "TotN_C_risk.mn","Number of 'days' where there was a collision risk, mean","",
    "TotN_C_risk.sd","Number of 'days' where there was a collision risk, sd","",
    "TotN_None.mn","Number of 'days' where there was no ORD interaction, mean","",
    "TotN_None.sd","Number of 'days' where there was no ORD interaction, sd","",
    "TotN_D_only.mn","Number of 'days' where there was displacement only, mean","",
    "TotN_D_only.sd","Number of 'days' where there was displacement only, sd","",
    "TotN_B_only.mn","Number of 'days' where there was a barrier effect only, mean","",
    "TotN_B_only.sd","Number of 'days' where there was a barrier effect only, sd","",
    "TotN_BD.mn","Number of 'days' where there was a barrier and displacement, mean","",
    "TotN_BD.sd","Number of 'days' where there was a barrier and displacement, sd","",
    "TotN_trips.mn","Total number of flights taken per day, mean","",
    "TotN_trips.sd","Total number of flights taken, sd","",
    "Tot_basickm.mn","Distance flown without ORD present, mean","",
    "Tot_basickm.sd","Distance flown without ORD present, sd","",
    "Tot_extrakm.mn","Extra distance flown because of ORDs, mean","",
    "Tot_extrakm.sd","Extra distance flown because of ORDs, sd","",
    "DispGT0.sm","Birds displaced at least once","",
    "BarrGT0.sm","Birds encountered a barrier at least once",""
  )

  flightsTable <- flightsTable %>%
    dplyr::filter(VarName %in% names(flights))

  # Return a list containing the human-friendly and the code-friendly tibbles
  list(data = flights, metadata = flightsTable)
}



################################################################################
#' @title Create a blank log sheet for recording the season's flights per bird
#'
#' @description Every timestep the seabirds fly out to a different foraging
#'   location. The flight might cross an ORD footprint with a collision risk or
#'   the bird might be displaced or be unimpeded. SeabORD records the number of
#'   times each interaction occurs per season.
#'
#' @param N The total number of individual birds at the start of the season.
#' @return A list containing two tibbles; one holding the individual blank
#' recording sheet and one for the metadata.
#'
#' @examples
#' create_flightsheet(1000)

#' @noRd
create_flightsheet <- function(N) {

  BirdID <- 1:N
  TotN_C_risk <- rep(0, N)
  TotN_None <- rep(0, N)
  TotN_D_only <- rep(0, N)
  TotN_B_only <- rep(0, N)
  TotN_BD <- rep(0, N)
  TotN_trips <- rep(0, N)
  Tot_basickm <- rep(0, N)
  Tot_extrakm <- rep(0, N)
  DispGT0 <- rep(0, N)
  BarrGT0 <- rep(0, N)

  FlightRecord <- tibble::tibble(
    BirdID, TotN_C_risk, TotN_None, TotN_D_only, TotN_B_only, TotN_BD,
    TotN_trips, Tot_basickm, Tot_extrakm, DispGT0, BarrGT0
  )

  FlightRecordTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "BirdID", "[Key] Individual unique identifier", "",
    "TotN_C_risk", "Number of flight where there was a collision risk", "",
    "TotN_None", "Number of flight where there was no ORD interaction", "",
    "TotN_D_only", "Number of flight where there was displacemnt only", "",
    "TotN_B_only", "Number of flight where there was a barrier effect only", "",
    "TotN_BD", "Number of flight where there was a barrier and displacement", "",
    "TotN_trips", "Total number of flights taken", "",
    "Tot_basickm", "Distance flown without ORD present", "",
    "Tot_extrakm", "Extra distance flown because of ORDs", "",
    "DispGT0", "Flag to indicate if the bird has been displaced at least once", "",
    "BarrGT0", "Flag to indicate if the bird has encountered a barrier at least once", ""
  )

  # Return a list containing the human-friendly and the code-friendly tibbles
  list(data = FlightRecord, metadata = FlightRecordTable)
}

################################################################################
#' @title Create a blank log sheet for recording the year summary
#'
#' @description Function called at the start of a SeabORD run to create blank
#'   tibbles to hold summary information on each year
#'
#' @param bycol Include output grouped by colony? (Logical)
#' @param bysus Include output grouped by susceptibility? (Logical)
#'
#' @importFrom magrittr %>%
#'
#' @return A list containing two tibbles; one holding the individual blank
#' recording sheet and one for the metadata.
#'
#' @examples
#' create_yearsheet()
#'
#' @noRd
create_yearsheet <- function(bycol = FALSE, bysus = TRUE) {

  # Tibble
  SeasonRecord <- tidyr::tibble(
    Rep = numeric(),
    Prey = numeric(),
    Season = character(),
    is_alive = numeric(),
    N = numeric(),
    BM_adult_t0.mn = numeric(),
    BM_adult_t0.sd = numeric(),
    BM_adult.mn = numeric(),
    BM_adult.sd = numeric(),
    Survived_poor = numeric(),
    Survived_modr = numeric(),
    Survived_good = numeric(),
    AdultsSurvivingYr = numeric(),
    .rows = 0
  )

  if (bycol) {
    SeasonRecord <- SeasonRecord %>%
      dplyr::mutate(SPA = character())  %>%
      dplyr::relocate(.after = Season)
  }

  if (bysus) {
    SeasonRecord <- SeasonRecord %>%
      dplyr::mutate(wfbe = numeric()) %>% dplyr::relocate(wfbe, .after = N) %>%
      dplyr::mutate(wfde = numeric()) %>% dplyr::relocate(wfde, .after = N)
  }

  # Metadata
  SeasonRecordTable <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number","",
    "Prey", "Median prey available", "g/area",
    "Season", "Season type, baseline or scenario", "",
    "SPA", "Grouped by colony", "",
    "is_alive", "Alive or not at end of breeding season", "(1 or 0)",
    "N","Number of birds in this group","",
    "wfde", "Susceptible to displacement", "(1 or 0)",
    "wfbe", "Susceptible to barrier effect", "(1 or 0)",
    "BM_adult_t0.mn", "Initial adult bird body mass, mean", "g",
    "BM_adult_t0.sd", "Initial adult bird body mass, standard deviation", "g",
    "BM_adult.mn", "Adult body mass (live birds only), mean", "g",
    "BM_adult.sd", "Adult body mass (live birds only), standard deviation", "g",
    "Survived_poor", "Bird survived winter?, if a poor year", "",
    "Survived_modr", "Bird survived winter?, if a moderate year", "",
    "Survived_good", "Bird survived winter?, if a good year", "",
    "AdultsSurvivingYr","Proportion of adult birds surviving a moderate year (including winter)",""
  )

  SeasonRecordTable <- SeasonRecordTable %>%
    dplyr::filter(VarName %in% names(SeasonRecord))

  # Return a list containing the human-friendly and the code-friendly tibbles
  list(data = SeasonRecord, metadata = SeasonRecordTable)
}

################################################################################
#' @title Create a blank log sheet for recording the output summary
#'
#' @description Function called at the start of a SeabORD run to create blank
#'   tibbles to hold summary information on each season pair (or 'replicate')
#'
#' @param bycol Include output grouped by colony? (Logical)
#' @param byi Include output grouped by displacement or barrier type? Logical
#' @param byall Include output grouped by all effects (Logical)
#' @return A list containing tibbles
#'
#' @importFrom dplyr contains
#' @importFrom magrittr %>%
#'
#' @examples
#'   create_summarylist()
#' @noRd
create_summarylist <- function(bycol = FALSE, byi = FALSE, byall = FALSE){

  # Data for adult birds -----------------------------------------------------
  AdultData <- tibble::tibble(
    Rep = numeric(),
    N = numeric(),
    BM_adult_t0.base.mn = numeric(),
    BM_adult_t0.base.sd = numeric(),
    BM_adult.base.mn = numeric(),
    BM_adult.base.sd = numeric(),
    BM_adult_t0.scen.mn = numeric(),
    BM_adult_t0.scen.sd = numeric(),
    BM_adult.scen.mn = numeric(),
    BM_adult.scen.sd = numeric(),
    .rows = 0
  )
  if (byall) {
    AdultData <- AdultData %>%
      dplyr::mutate(TotN_BD = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_B_only = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_D_only = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_None.scen = numeric(), .after = Rep)
  }
  if (byi) {
    AdultData <- AdultData %>%
      dplyr::mutate(i = logical(), .after = Rep)
  }
  if (bycol) {
    AdultData <- AdultData %>%
      dplyr::mutate(colony = character(), .after = Rep)
  }

  # Metadata
  AdultMetadata <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number","",
    "colony","Designated colony","",
    "i", "Logical (T/F) indicating if members fit the criteria for the group", "",
    "TotN_None.scen","Total number of 'days' in the season in which the bird had no interaction with ORDs in the scenario season","",
    "TotN_D_only" ,"Total number of 'days' in the season in which the bird was only displaced by ORDs","",
    "TotN_B_only" ,"Total number of 'days' in the season in which the bird was only barriered by ORDs","",
    "TotN_BD","Total number of 'days' in the season in which the bird was both displaced and barriered by ORDs","",
    "N","Number of adult birds in the group","",
    "BM_adult_t0.base.mn","Initial adult bird body mass for the baseline season, mean", "g",
    "BM_adult_t0.base.sd","Initial adult bird body mass for the baseline season, sd", "g",
    "BM_adult.base.mn", "End of baseline breeding season body mass for adults, mean", "g",
    "BM_adult.base.sd","End of baseline breeding season body mass for adults, sd", "g",
    "BM_adult_t0.scen.mn","Initial adult bird body mass for the scenario season, mean", "g",
    "BM_adult_t0.scen.sd","Initial adult bird body mass for the scenario season, sd", "g",
    "BM_adult.scen.mn", "End of scenario breeding season body mass for adults, mean", "g",
    "BM_adult.scen.sd","End of scenario breeding season body mass for adults, sd", "g",
  )
  AdultMetadata <- dplyr::filter(AdultMetadata, VarName %in% names(AdultData))

  # To be returned
  adults <- list(data = AdultData, metadata = AdultMetadata)

  # Data for chicks ----------------------------------------------------------
  ChickData <- tibble::tibble(
    "Rep" = numeric(),
    "N" = numeric(),
    .rows = 0
  )
  if (byall) {
  }
  if (byi) {
  }
  if (bycol) {
    ChickData <- ChickData %>%
      dplyr::mutate("colony" = character(), .after = N)
  }

  # Metadata
  ChickMetadata <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number","",
    "N","Number of chicks","",
    "colony","Designated colony",""
  )
  ChickMetadata <- dplyr::filter(ChickMetadata, VarName %in% names(ChickData))

  # To be returned
  chicks <- list(data = ChickData, metadata = ChickMetadata)

  # Data for yearly survival -------------------------------------------------
  SurvivalData <- tibble::tibble(
    Rep = numeric(),
    N = numeric(),
    AdditionalMort = numeric(),
    pSurvival_1.base.mn = numeric(),
    pSurvival_1.base.sd = numeric(),
    pSurvival_1.scen.mn = numeric(),
    pSurvival_1.scen.sd = numeric(),
    Survived_1.base.pc = numeric(),
    Survived_1.base.ndead = numeric(),
    Survived_1.scen.pc = numeric(),
    Survived_1.scen.ndead = numeric(),
    pSurvival_2.base.mn = numeric(),
    pSurvival_2.base.sd = numeric(),
    pSurvival_2.scen.mn = numeric(),
    pSurvival_2.scen.sd = numeric(),
    Survived_2.base.pc = numeric(),
    Survived_2.base.ndead = numeric(),
    Survived_2.scen.pc = numeric(),
    Survived_2.scen.ndead = numeric(),
    pSurvival_3.base.mn = numeric(),
    pSurvival_3.base.sd = numeric(),
    pSurvival_3.scen.mn = numeric(),
    pSurvival_3.scen.sd = numeric(),
    Survived_3.base.pc = numeric(),
    Survived_3.base.ndead = numeric(),
    Survived_3.scen.pc = numeric(),
    Survived_3.scen.ndead = numeric(),
    .rows = 0
  )
  if (byall) {
    SurvivalData <- SurvivalData %>%
      dplyr::mutate(TotN_BD = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_B_only = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_D_only = numeric(), .after = Rep) %>%
      dplyr::mutate(TotN_None.scen = numeric(), .after = Rep)
  }
  if (byi) {
    SurvivalData <- SurvivalData %>%
      dplyr::mutate(i = logical(), .after = Rep)
  }
  if (bycol) {
    SurvivalData <- SurvivalData %>%
      dplyr::mutate(colony = character(), .after = N)
  }

  # Metadata
  SurvivalMetadata <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "Rep", "Replicate run number","",
    "colony","Designated colony","",
    "i", "Logical (T/F) indicating if members fit the criteria for the group", "",
    "TotN_None.scen","Total number of 'days' in the season in which the bird had no interaction with ORDs in the scenario season","",
    "TotN_D_only" ,"Total number of 'days' in the season in which the bird was only displaced by ORDs","",
    "TotN_B_only" ,"Total number of 'days' in the season in which the bird was only barriered by ORDs","",
    "TotN_BD","Total number of 'days' in the season in which the bird was both displaced and barriered by ORDs","",
    "N","Number of adult birds","",
    "AdditionalMort","Additional mortality due to offshore renewable developments","% change",
    "pSurvival_1.base.mn","Probability of survival over winter in a poor year for the baseline season, mean","",
    "pSurvival_1.base.sd","Probability of survival over winter in a poor year for the baseline season, sd","",
    "pSurvival_1.scen.mn","Probability of survival over winter in a poor year for the scenario season, mean","",
    "pSurvival_1.scen.sd","Probability of survival over winter in a poor year for the scenario season, sd","",
    "pSurvival_2.base.mn","Probability of survival over winter in a moderate year for the baseline season, mean","",
    "pSurvival_2.base.sd","Probability of survival over winter in a moderate year for the baseline season, sd","",
    "pSurvival_2.scen.mn","Probability of survival over winter in a moderate year for the scenario season, mean","",
    "pSurvival_2.scen.sd","Probability of survival over winter in a moderate year for the scenario season, sd","",
    "pSurvival_3.base.mn","Probability of survival over winter in a good year for the baseline season, mean","",
    "pSurvival_3.base.sd","Probability of survival over winter in a good year for the baseline season, sd","",
    "pSurvival_3.scen.mn","Probability of survival over winter in a good year for the scenario season, mean","",
    "pSurvival_3.scen.sd","Probability of survival over winter in a good year for the scenario season, sd","",
    "Survived_1.base.pc","Adult birds surviving in a poor year for the baseline year","%",
    "Survived_1.base.ndead","Number of dead birds in a poor year at the end of the baseline year","",
    "Survived_1.scen.pc","Adult birds surviving in a poor year for the scenario year","%",
    "Survived_1.scen.ndead","Number of dead birds in a poor year at the end of the scenario year","",
    "Survived_2.base.pc","Adult birds surviving in a moderate year for the baseline year","%",
    "Survived_2.base.ndead","Number of dead birds in a moderate year at the end of the baseline year","",
    "Survived_2.scen.pc","Adult birds surviving in a moderate year for the scenario year","%",
    "Survived_2.scen.ndead","Number of dead birds in a moderate year at the end of the scenario year","",
    "Survived_3.base.pc","Adult birds surviving in a good year for the baseline year","%",
    "Survived_3.base.ndead","Number of dead birds in a good year at the end of the baseline year","",
    "Survived_3.scen.pc","Adult birds surviving in a good year for the scenario year","%",
    "Survived_3.scen.ndead","Number of dead birds in a good year at the end of the scenario year",""
  )
  SurvivalMetadata <- dplyr::filter(SurvivalMetadata, VarName %in% names(SurvivalData))

  # To be returned
  survival <- list(poor = SurvivalData %>% dplyr::select(!(contains("_2") | contains("_3"))),
                   mod = SurvivalData %>% dplyr::select(!(contains("_1") | contains("_3"))),
                   good = SurvivalData %>% dplyr::select(!(contains("_1") | contains("_2"))),
                   metadata = SurvivalMetadata)

  # RETURN
  list(adults = adults, chicks = chicks, survival = survival)

}

################################################################################
#' @title Update the output step list with a new set of results
#'
#' @description Function called at the end of a SeabORD season to add a new row
#'   of results with information on a season
#'
#' @param inlist The SeabORD output list containing previous output
#' @param newdata New output to be added to the full list
#'
#' @importFrom dplyr bind_rows
#'
#' @return A revised full list with an extra row in the appropriate tibbles
#'
#' @noRd
update_steplist <- function(inlist, newdata){

  inlist$data <- bind_rows(inlist$data, newdata)

  # RETURN
  return(inlist)

}

################################################################################
#' @title Update the output summary list with a new set of results
#'
#' @description Function called at the end of a SeabORD run to add a new row of
#'   results with summary information on a season pair (or 'replicate')
#'
#' @param inlist The SeabORD output list containing previous output
#' @param newdata New output to be added to the full list
#' @return A revised full list with an extra row in the appropriate tibbles
#'
#' @noRd

update_summarylist <- function(inlist, newdata){

  # Adults
  inlist$adults$data <- bind_rows(inlist$adults$data, newdata$adults)

  # Chicks
  inlist$chicks$data <- bind_rows(inlist$chicks$data, newdata$chicks)

  # Survival in a poor year
  inlist$survival$poor <- bind_rows(inlist$survival$poor, newdata$survival$poor)

  # Survival in a moderate year
  inlist$survival$mod <- bind_rows(inlist$survival$mod, newdata$survival$mod)

  # Survival in a good year
  inlist$survival$good <- bind_rows(inlist$survival$good, newdata$survival$good)

  inlist$survival$metadata <- inlist$survival$metadata %>%
    dplyr::filter(VarName %in% unique(
      c(names(newdata$survival$poor),
        names(newdata$survival$mod),
        names(newdata$survival$good))))

  # RETURN
  return(inlist)

}

################################################################################
#' @title Update the flight record
#'
#' @description Function called once per timestep to keep a record of the number
#'   of flights that involve displacement, collision, barrier effects etc. for
#'   the whole season.
#'
#' @param frd The current FlightRecord$data tibble
#' @param tfs New TodaysFlight tibble to be added to the running totals
#'
#' @return A revised FlightRecord tibble
#'
#' @noRd
update_flightrecord <- function(frd, tfs){

  frd <- frd %>%

    dplyr::left_join(
      tfs[c('BirdID', 'BarrierOut', 'BarrierRetn', 'Displaced',
            'CollisionRisk', 'Outwardm', 'AdditionalOutwardm',
            'Returnm', 'AdditionalReturnm', 'trips_n')], by = "BirdID") %>%

    dplyr::mutate(TotN_C_risk = purrr::pmap_dbl(
      list(CollisionRisk, TotN_C_risk),
      ~ ifelse({..1}==1, {..2}+1, {..2}))) %>%

    dplyr::mutate(TotN_None = purrr::pmap_dbl(
      list(Displaced, BarrierOut, BarrierRetn, TotN_None),
      ~ ifelse({..1}+{..2}+{..3}==0, {..4}+1, {..4}))) %>%

    dplyr::mutate(TotN_D_only = purrr::pmap_dbl(
      list(Displaced, BarrierOut, BarrierRetn, TotN_D_only),
      ~ ifelse(({..1}==1)&({..2}+{..3}==0), {..4}+1, {..4}))) %>%

    dplyr::mutate(TotN_B_only = purrr::pmap_dbl(
      list(Displaced, BarrierOut, BarrierRetn, TotN_B_only),
      ~ ifelse(({..1}==0)&({..2}+{..3}>0), {..4}+1, {..4}))) %>%

    dplyr::mutate(TotN_BD = purrr::pmap_dbl(
      list(Displaced, BarrierOut, BarrierRetn, TotN_BD),
      ~ ifelse(({..1}==1)&({..2}+{..3}>0), {..4}+1, {..4}))) %>%

    dplyr::mutate(TotN_trips = TotN_trips + trips_n) %>%

    dplyr::mutate(Tot_basickm = Tot_basickm + 0.001*(Outwardm + Returnm)*trips_n) %>%

    dplyr::mutate(Tot_extrakm = Tot_extrakm + 0.001*(AdditionalOutwardm + AdditionalReturnm)*trips_n) %>%

    dplyr::mutate(DispGT0 = purrr::pmap_dbl(
      list(TotN_D_only, TotN_BD), ~ ifelse(({..1}+{..2}>0), 1, 0))) %>%

    dplyr::mutate(BarrGT0 = purrr::pmap_dbl(
      list(TotN_B_only, TotN_BD), ~ ifelse(({..1}+{..2}>0), 1, 0))) %>%

    dplyr::select(-c('BarrierOut', 'BarrierRetn', 'Displaced',
                     'CollisionRisk', 'Outwardm', 'AdditionalOutwardm',
                     'Returnm', 'AdditionalReturnm', 'trips_n'))

  return(frd)

}
