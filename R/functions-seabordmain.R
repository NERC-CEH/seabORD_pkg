####################################################################################################
## FUNCTIONS for SeabORD v2.0.x
## Author: UKCEH
## Date: From May 2020

#'
#' @title Run a SeabORD simulation
#'
#' @description A model to estimate the population consequences of displacement
#'   from proposed offshore renewable energy developments for key seabird species.
#'   The function performs one or more stochastic simulation replicates and returns summaries of adult, chick, flight and survival metrics.
#' @details
#' Before running this function, all model inputs must be prepared.
#' A complete description of the required input objects is provided in:
#' \code{vignette("C_KI_inputs", package = "seabORD")}
#'
#' @param Par List - The main parameters controlling/defining this run
#' @param modPar List - Parameters relating to the model mode and computer environment
#' @param ordPar List -  Input parameters relating to the ORDs
#' @param switches List -  A set of switches/flags used to control optional features of the run
#' @param seamask Raster - land/sea grid, 1km expected.
#' @param spadat1 Tibble - Information relating to the colony/SPA
#' @param spadat2 Tibble - Information relating to the colony/SPA
#' @param spdat Dataframe - Species-specific parameters
#' @param BrdData Raster - Bird distribution map - derived from GPS data or failing that, a distance-decay model
#' @param FrgCompData Raster - Competition distribution map - derived from GPS data or failing that, a distance-decay model
#' @param fltdist_base Raster - Flight distance by sea, without ORDs. See user guide for details.
#' @param FlightGridcorrection Raster - Flight distance transition layer (gdistance) corrected for distortions relating to the CRS
#' @param ORDpoly Multipolygon (simple features) - The ORD footprints
#'
#' @importFrom raster ncol nrow raster calc cellStats crs extent ncell projectRaster values
#' @importFrom stats sd
#' @importFrom methods as
#' @importFrom dplyr any_of bind_cols bind_rows contains group_by last_col left_join pull tally
#' @importFrom tibble as_tibble
#' @importFrom sp coordinates
#' @importFrom readr write_csv
#' @importFrom magrittr %>%
#'
#' @return A list containing tibbles
#' @export
seabord <- function(Par, modPar, ordPar, switches, seamask, spadat1, spadat2,
                    spdat, BrdData, FrgCompData, fltdist_base,
                    FlightGridcorrection, ORDpoly) {

  ##============================================================================
  ## SECTION -- Switches and internals values --

  # > Dev: Switch values
  val.ver_no <- "2.0.0"

  # > Dev: Miscellaneous values currently fixed but may become parameters
  fixedVals <- list(
    "flights_max" = 6
  )

  # > Runtime values to save
  thisRun <- list()

  ##============================================================================
  ## SECTION -- Initialise/transform based on input --

  # Is this being run locally/in serial?
  if (switches$environment %in% c("serial","CEF","CEFtest","test")) {

    # > What sort of run is this?
    if (switches$modelmode == "scenario") {
      thisRun$baseonly <- FALSE
      thisRun$seasonlist <- c('base','scen')
      thisRun$refname <- paste0(modPar$reference, "_",
                                modPar$Nreplicates, "pair",
                                ifelse(modPar$Nreplicates == 1, "", "s"))
    } else { # if calibration:
      thisRun$baseonly <- TRUE
      thisRun$seasonlist <- c('base')
      thisRun$refname <- paste0(modPar$reference, "_",
                                modPar$Nreplicates, "pair",
                                ifelse(modPar$Nreplicates == 1, "", "s"))
    }

    # > Output folder name (not used in the cef)
    thisRun$stamp <- gsub("\\.", "_", make.names(paste0(thisRun$refname,"_", Sys.time())))

    # Create a folder for outputs for this whole run - not including rep number or time -
    # just date (so that each parallel run doesn't put a separate one out seconds apart:
    if (switches$modelmode == "scenario"){
      thisRun$refname2 <- paste0(modPar$reference,"_",paste(ordPar$include_ORDs, collapse = "_"))   # add ORD short names being used to the folder name  # add ORD short names being used to the folder name
    } else {
      thisRun$refname2 <- paste0(modPar$reference)
    }
    thisRun$stamp2 <- gsub("\\.", "_", make.names(paste0(thisRun$refname2,"_", Sys.Date())))
    modPar$foldername <- thisRun$stamp2

  } else { # beginning of parallel specification

    if (switches$modelmode == "scenario") {
      thisRun$baseonly <- FALSE
      thisRun$seasonlist <- c('base','scen')
      thisRun$refname <- paste0(modPar$reference,"_rep",modPar$Nparallel,"_")
    } else {  # if running baseline only for calibration
      thisRun$baseonly <- TRUE
      thisRun$seasonlist <- c('base')
      thisRun$refname <- paste0(modPar$reference,"_rep",modPar$Nparallel,"_")
    }

    # > Output folder name (not used in the cef)
    thisRun$stamp <- gsub("\\.", "_", make.names(paste0(thisRun$refname,"_", Sys.time())))

    # Create a folder for outputs for this whole run - not including rep number or time -
    # just date (so that each parallel run doesn't put a separate one out seconds apart:
    if (switches$modelmode == "scenario"){
      thisRun$refname2 <- paste0(modPar$reference,"_",paste(ordPar$include_ORDs, collapse = "_"))   # add ORD short names being used to the folder name
    } else {
      thisRun$refname2 <- paste0(modPar$reference)
    }
    thisRun$stamp2 <- gsub("\\.", "_", make.names(paste0(thisRun$refname2,"_", Sys.Date())))
  }

  if (switches$environment %in% c("CEF", "CEFtest")) {
    # Do not create the folder unless file output is specified
    if (any(switches$printdaily,
            switches$printseason,
            switches$printpair,
            switches$printfinal,
            switches$savebirdflightmap)) {
      dir.create(file.path(modPar$outputdir, thisRun$stamp2), showWarnings = FALSE)
    }
  } else {
    dir.create(file.path(modPar$outputdir, thisRun$stamp2), showWarnings = FALSE)
  }

  # > Make a reference grid for the Raster*
  base_grid <- raster::raster()
  crs(base_grid) <- crs(seamask)
  extent(base_grid) <- extent(seamask)
  raster::ncol(base_grid) <- raster::ncol(seamask)
  raster::nrow(base_grid) <- raster::nrow(seamask)

  # > Set up the colony tibble
  Colony <- transform_sbcolony(n = Par$Npairspercol, f = Par$Nscalefactor,
                               spadat1, spadat2)

  # > Set up the Species tibble
  Species <- transform_sbspecies(spdat)

  # Turn Colony$data into a SpatialPointsDataFrame
  coordinates(Colony$data) <- ~ Easting + Northing
  crs(Colony$data) <- crs(base_grid)

  # > Record the cell number for each colony
  Colony$data$StartGridIdx <- raster::cellFromXY(base_grid, coordinates(Colony$data))

  # > Record the coordinates of the cell centre for each colony point
  Colony$data$StartCoords <- raster::xyFromCell(base_grid, Colony$data$StartGridIdx)

  # > Update the Colony metadata
  newmeta <- tidyr::tribble(
    ~VarName, ~VarDescription, ~VarUnits,
    "StartGridIdx", "Cell number in the region grid", "",
    "StartCoords", "xy coordinated for the StartGridIdx", "degrees")
  Colony$metadata <- bind_rows(Colony$metadata, newmeta) %>% dplyr::distinct()

  # > Create 'PreyAvailable_rel', the base prey map; actual values set per season.
  # Note this version assumes uniform prey only
  PreyAvailable_rel <- calc(seamask, fun = function(x) {x[x == 0] <- 1; return(x)})

  #-----------------------------------------------------------------------------
  # > Clean the forage competition map

  # How many birds are there in the region not being modelled?
  thisRun$NOtherBirds <- FrgCompData %>% cellStats('sum')
  # Project to seabORD default
  FrgCompData <- FrgCompData %>% projectRaster(to = seamask)
  # Remove any negative values
  FrgCompData <- calc(FrgCompData, fun = function(x) {x[x < 0] <- 0; return(x)})
  # Remove any land cell or unreachable cells
  FrgCompData[is.na(seamask)] <- NA
  # Then make sure the totals are correct
  FrgCompData <- thisRun$NOtherBirds *
    FrgCompData / raster::cellStats(FrgCompData, 'sum')

  #-----------------------------------------------------------------------------
  # > Clean the bird density data

  # Project to seabORD default
  BrdData <- BrdData %>% projectRaster(to = seamask)
  # Remove any land cell or unreachable cells
  BrdData[is.na(seamask)] <- NA
  # Set non-forage sites to NA
  BrdData <- calc(BrdData, fun = function(x) {x[x == 0] <- NA; return(x)})
  # Renormalise
  BrdData <- BrdData/raster::cellStats(BrdData,"sum")
  # Name the layers
  names(BrdData) <- paste(Par$colonies,Par$thisSpecies, sep = "_")

  #-----------------------------------------------------------------------------
  # Calculate typical bird density
  # How many forage sites? This assumes 1 SPA and 1km grid.
  nfs <- freq((BrdData+FrgCompData)>0, digits=0,useNA='no')
  thisRun$totareakm2 <- nfs[2]

  # Maximum number of birds in the region
  thisRun$NBirdsRegion <- 2.0*(sum(Par$Npairspercol)) + thisRun$NOtherBirds

  # Estimated number of birds per km2
  thisRun$popbirdsperkm2 <- thisRun$NBirdsRegion/thisRun$totareakm2

  # Check the bird density data matches the required colonies
  # ok <- nlayers(BrdData) == dim(Colony$data@data)[1]

  # > Site Selection data
  SiteSelection <- BrdData %>% calc(fun = function(r) {r[is.na(r)] <- 0; return(r)})
  SiteSelection <- as_tibble(SiteSelection[]) %>% cumsum()
  names(SiteSelection) <- names(BrdData)

  ##============================================================================
  ## SECTION -- ORDs --

  if (switches$modelmode == "scenario") {

    # Add the footprint border
    ORDpolyborder <- ORDpoly %>% sf::st_buffer(ordPar$FootprintBorder*1000.0)

    # Add the footprint buffer
    ORDpolybuffer <- ORDpolyborder %>% sf::st_buffer(ordPar$BufferZone*1000.0)

    # Which cells are in the ORD
    seamasksg <- as(seamask, "SpatialGrid")
    ORDpolybordersg <- as(ORDpolyborder, "Spatial")
    ORDpolybuffersg <- as(ORDpolybuffer, "Spatial")
    inORDborder <- sp::over(ORDpolybordersg, seamasksg, returnList = TRUE)
    inORDbuffer <- sp::over(ORDpolybuffersg, seamasksg, returnList = TRUE)

    ## Calculations about obstructed distance by sea
    # We know the choice of colonies and the ORDs so we can generate the
    # obstructed distance by sea. These rasters only need to be calculated once
    # per set of conditions (colonies, ORDs and borders). In SeabORD outside the
    # CEF, these files would be saved locally and imported when needed, but this
    # isn't as straightforward in the CEF - partly because we don't know where
    # they could be saved (securely) and partly because of the number of
    # combinations as users supply their own footprints.

    fltdist_scen <- make_fltdist_scen(Colony, inORDborder, ordPar, seamask, FlightGridcorrection)

    # Check
    # p <- ggplot() + ggspatial::layer_spatial(fltdist_scen[[3]] - fltdist_base[[3]]) +
    #   scale_fill_viridis_c(option = "D", direction = -1, na.value = "transparent") +
    #   ggtitle("Distance, km") +  ylim(3600000, 4100000) + xlim(3300000, 3700000)
    # p

  } else {

    ORDpolyborder <- NULL
    ORDpolybuffer <- NULL
    inORDborder <- NULL
    inORDbuffer <- NULL
    fltdist_scen <- NULL

  }

  ## Could add a 'makezones()' function here, as used in SeabORD-M
  #> Zone 1/2 = Area unaffected by ORDs
  #> Zone 3 = area 'behind' and ORD, where dist_base not equal to dist_scen (ie bird was barriered)
  #> Zone 4 = inside a footprint
  #> zone 5 = more complex (eg in footprint B, barriered by A)

  ##============================================================================
  ## SECTION -- Create the structures for saving summary statistics --

  # Summary of all adult birds at the end of the season
  output_a0 <- create_stepsheeta(bycol = switches$bycol, bych = FALSE)

  # Summary of all adult birds at the end of the season
  output_f0 <- create_stepsheetf(bycol = switches$bycol)

  # Summary of all chicks at the end of the season
  output_c0 <- create_stepsheetc(bycol = switches$bycol)

  # Summary of all chicks at the end of the season
  output_y0 <- create_yearsheet(bycol = switches$bycol, bysus = switches$bysus)

  # Summarise all birds together - regional output
  output_i0 <- create_summarylist(bycol = switches$bycol, byi = FALSE, byall = FALSE)

  # Grouped by DB 0 & 0 = Birds never directly impacted
  output_i1 <- create_summarylist(bycol = switches$bycol, byi = TRUE, byall = FALSE)

  # Grouped by DB 1 or 1 = Birds directly impacted in some way at least once
  output_i2 <- create_summarylist(bycol = switches$bycol, byi = TRUE, byall = FALSE)

  # Grouped by DB 1 & 0 = Birds displaced at least once, never barriered
  output_i3 <- create_summarylist(bycol = switches$bycol, byi = TRUE, byall = FALSE)

  # Grouped by DB 0 & 1 = Birds barriered at least once, never displaced
  output_i4 <- create_summarylist(bycol = switches$bycol, byi = TRUE, byall = FALSE)

  # Grouped by DB 1 & 1 = Birds barriered and displaced at least once
  output_i5 <- create_summarylist(bycol = switches$bycol, byi = TRUE, byall = FALSE)

  # Group by all impacts separately
  output_i6 <- create_summarylist(bycol = switches$bycol, byi = FALSE, byall = TRUE)

  #> Seeds: Set up a number of random number sequence seeds **Needs to be checked
  seedmat <- set_seedvalues(modPar$initialseed, 10, modPar$Nreplicates)

  #> Prey - Select and set the median regional prey value for each run in the set
  # These are inputs now
  #Pmedian <- set_medianprey(seedmat[8, 1, drop = TRUE], Par$PmaxLim, modPar$Nreplicates)

  #> Many parameters apply to a day of 24 hours but some time steps are not 24 hrs
  #  so we need a conversion factor to apply throughout the model
  TimeFactor <- Species$data$daylength / 24.0
  TimeVals <- list(
    "colony_opt_h" = Species$data$daylength / 2.0,
    "colony_min_h" = 1.0 * TimeFactor,
    "at_sea_min_h" = 1.0 * TimeFactor
  )

  ##============================================================================
  ## SECTION -- Flight Path distances --

  FPathDists <- tabularaster::as_tibble(fltdist_base, cell = TRUE, dim = TRUE,
                                        value = TRUE, xy = TRUE) %>%
    dplyr::relocate(dimindex) %>%
    dplyr::mutate(Start = Colony$data$StartGridIdx[dimindex]) %>%
    dplyr::rename(GridID = cellindex) %>%
    dplyr::rename(ColonyNo = dimindex)  %>%
    dplyr::rename(Dist.base = cellvalue) %>%
    dplyr::relocate(Dist.base,.after = last_col()) %>%
    # Remove land cells
    dplyr::filter(!is.na(Dist.base))

  if (switches$modelmode == "scenario") {

    tmp <- tabularaster::as_tibble(fltdist_scen, cell = TRUE, dim = TRUE,
                                   value = TRUE, xy = TRUE) %>%
      dplyr::relocate(dimindex) %>%
      dplyr::mutate(Start = Colony$data$StartGridIdx[dimindex]) %>%
      dplyr::rename(GridID = cellindex) %>%
      dplyr::rename(ColonyNo = dimindex)  %>%
      dplyr::rename(Dist.scen = cellvalue) %>%
      dplyr::relocate(Dist.scen,.after = last_col())

    FPathDists <- FPathDists %>%
      dplyr::left_join(tmp,
                       by = c("ColonyNo", "GridID", "x", "y", "Start"),
                       keep = FALSE)

    FPathDists$isinORD <- "none"
    for (fpt in seq(length(ordPar$include_ORDs))) {
      FPathDists$isinORD[FPathDists$GridID %in% inORDborder[[fpt]]] <-  ordPar$include_ORDs[fpt]
    }

  } else {

    # baseline run only, so make scen the same as base
    FPathDists$Dist.scen <- FPathDists$Dist.base
    FPathDists$isinORD <- "none"

  }

  # What is the extra distance to a cell through barrier effects?
  # Note this refers ONLY to journeys to the same cell with and without barriers, it does not
  # take displacement into account
  FPathDists <- FPathDists %>%
    dplyr::mutate(ExtraKm = round(1000*(Dist.scen - Dist.base))/1000)

  ##============================================================================
  ## SECTION -- SeabORD flight routines

  ## SUBSECTION -- Replicate season pairs --

  for (simrun in seq_len(modPar$Nreplicates)) {

    if (!switches$silent) {
      print.noquote(paste(simrun, "/", modPar$Nreplicates, "...", date()))
    }

    #> Set/Reset the random number streams

    #?? to do? Is this necessary in R (it was in Matlab)

    #> Set/Reset the core tibbles

    # for summary values
    YearBirds <- list()

    # Create the basic bird dataset - this does not change during the pair
    BirdType <- set_initialbirdtype(
      seedmat[2, paste0("run", simrun), drop = TRUE],
      Colony$data@data,
      Par$thisSpecies,
      Par$Prob_Displacement,
      Par$Prob_Barrier
    )

    #> Flight destinations

    # Get the list of default forage locations for this pair
    out <- vector("list", length(Par$colonies))
    z0 <- BirdType$data %>% dplyr::select(BirdID, colony, wfde)
    for (c in seq(length(Par$colonies))){
      Prast <- BrdData[[paste(Par$colonies[c],Par$thisSpecies, sep = "_")]]
      Prast[Colony$data$StartGridIdx] <- NA
      z1 <- z0 %>% dplyr::filter(colony == Par$colonies[c])
      z2 <- select_destinations(Prast, nrow(z1), Species$data$seasonlength)
      z3 <- bind_cols(z1,z2) #%>% dplyr::select(-colony)
      out[[c]] <- z3
    }
    FlightListA <- out %>% bind_rows()

    # Make an alternative FlightList for the scenario run *** Needs rewriting with pmap!
    FlightListB <- FlightListA

    # If there are ORDs, find new destinations [this needs to be rewritten to be more efficient]

    if (switches$modelmode == "scenario") {

      insidefoot <- unlist(inORDborder)

      for (fpt in seq(length(ordPar$include_ORDs))) {

        # Find the cells ONLY in the buffer and not in ANY footprint
        ORDbuf <- inORDbuffer[[fpt]] %in% insidefoot
        ORDbuf <- inORDbuffer[[fpt]][!ORDbuf]

        # for each colony
        for (c in seq(length(Par$colonies))){

          # Get the in-buffer selection probabilities
          Prast <- base_grid; values(Prast) <- 0
          Prast[ORDbuf] <- BrdData[[paste(Par$colonies[c],Par$thisSpecies, sep = "_")]][ORDbuf]
          Prast <- Prast/raster::cellStats(Prast,"sum")

          # Now check if a susceptible bird is going to be displaced, pick a new site
          for (r in seq(nrow(FlightListA))) {
            if (FlightListA[r,'colony'] == Par$colonies[c] && FlightListA[r,'wfde'] == 1) {
              for (ts in seq(Species$data$seasonlength)+3){
                if(FlightListA[r,ts] %in% inORDborder[[fpt]]) {
                  FlightListB[r,ts] = select_destinations(Prast, 1, 1)
                }
              }
            }
          }
        }
      }

      # FlightListA contains the destinations in the baseline season
      FlightListA <- FlightListA %>% dplyr::select(-c(colony, wfde))

      # FlightListB contains the destinations in the scenario season
      FlightListB <- FlightListB %>% dplyr::select(-c(colony, wfde))

    }

    #> Competition from 'other' birds (colonies not simulated) -----------------

    if (switches$modelmode == "scenario") {

      # Note: if there are several overlapping ORDs this may need checking/revising?
      fpmat <- array(dim=c(ncell(seamask), length(ordPar$include_ORDs)))
      for (fp in seq(from=1, to = length(ordPar$include_ORDs))) {
        fpgrd <- base_grid
        values(fpgrd) <- 0
        fpgrd[inORDbuffer[[fp]]] <- 2
        fpgrd[inORDborder[[fp]]] <- 1
        fpmat[,fp] <- values(fpgrd)
      }

    } else {
      fpmat <- array(dim=c(ncell(seamask), 1))
      fpmat[,1] <- rep(0,ncell(seamask))
    }

    absabunmap <- FrgCompData %>% calc(fun = function(r) {r[is.na(r)] <- 0; return(r)}) %>% values()
    ForageComp <- sim_nbirds_wwf_pertimestep(
      absabunmap = absabunmap,
      fpmat = fpmat,
      disprate = Par$Prob_Displacement,
      ntimesteps = Species$data$seasonlength)
    names(ForageComp) <- thisRun$seasonlist

    #> Set the prey level for this particular season pair-----------------------
    PreyAvailable <- switch(Par$PreyType,
                            "Uniform" = PreyAvailable_rel*Par$Pmedian[simrun],
                            "Map" = PreyAvailable_rel*2.0*Par$Pmedian[simrun]
    )

    ## SUBSECTION -- For each simulated breeding season -- =====================

    # This version of SeabORD models the breeding season only. Future versions
    # may be extended to encompass a full year. If so, we might add additional
    # loops here, e.g. pre- and post- breeding season activities, OR convert
    # this loop to be a full year and model the changing lifecycle changes
    # within that loop.

    #--> Two seasons, with and without ORDs if modelmode == scenario ------------>

    for (season in thisRun$seasonlist) {

      # Set/Reset the flight record for the season
      FlightRecord <- create_flightsheet(pull(tally(BirdType$data)))

      # Reset the foraging site map
      BirdFlightMap <- base_grid
      BirdFlightMap[] <- 0

      # Set/Reset the bird states
      BirdState <- set_initialbirdstate(
        seedmat[3, paste0("run", simrun), drop = TRUE],
        dplyr::pull(tally(BirdType$data)), Species$data$daylength,
        Species$data$BM_adult_mn, Species$data$BM_adult_sd,
        Species$data$adult_DEE_mn, Species$data$adult_DEE_sd,
        Species$data$assim_eff
      )

      # Save the default variables for use later (obsolete now?)
      BSdefa <- names(BirdState$data)

      # Set/Reset the chick states
      ChickState <- set_initialchickstate(
        seedmat[4, paste0("run", simrun), drop = TRUE],
        dplyr::pull(tally(BirdType$data)), Species$data$daylength,
        Species$data$BM_chick_mn, Species$data$BM_chick_sd, Species$data$chick_DER,
        Species$data$assim_eff
      )

      # Save the default variables for use later (obsolete now?)
      BSdefc <- names(ChickState$data)

      # Reset the optimum 'Superchick' (for calculating relative body condition)
      Opt_BM_chick <- Species$data$BM_chick_mn

      ## SUBSECTION -- Time-step loop -- =======================================

      #--> A season is a number of time steps, species-dependent number ------->

      for (tstep in seq_len(Species$data$seasonlength)) {

        # Display tracker
        if (!switches$silent) {
          print.noquote(paste(tstep, "/", Species$data$seasonlength, date()))
        }

        # Optimum times are set once per species but puffins are different...
        if (Species$data$SID == "Pu") {
          if (tstep > 5) {
            TimeVals$colony_opt_h <- 1.0*TimeFactor
          } else {
            TimeVals$colony_opt_h <- Species$data$daylength / 2.0
          }
        }

        if (sum(BirdState$data$is_alive) > 0) {

          # ====================================================================
          # Find this timestep's flights (TodaysFlights)
          # > Set up today's flight table

          # Distance KEY:
          # UnobstructedKm = the distance to the *first choice* location, unhindered.
          # DirectKm = the distance to the *final* location, unhindered.
          # BarrieredKm = the distance to the *final* location, with ORDs present.
          # ActualKm = the distance actually flown (one way), depending on susceptibility.
          # ExtraKm = The difference between the obstructed and unobstructed distance to
          #           the DESTINATION cell. Could be first or second choice. If not 0 then
          #           we know the bird was barriered or faced a collision risk.
          #

          TodaysFlights <- BirdType$data %>%
            dplyr::select(BirdID, colony, wfbe, wfde) %>%
            dplyr::semi_join(BirdState$data[BirdState$data$is_alive == 1, ], by = "BirdID") %>%
            dplyr::mutate(tstep = tstep) %>%
            dplyr::inner_join(Colony$data@data[c("code", "StartGridIdx")], by = c("colony" = "code")) %>%
            dplyr::inner_join(FlightListA[c("BirdID",paste0("t",tstep))], by = "BirdID") %>%
            dplyr::rename(FirstChoice = paste0("t",tstep)) %>%
            dplyr::left_join(FPathDists[,c('GridID', 'Start', 'Dist.base')],
                             by = c("StartGridIdx" = "Start","FirstChoice"="GridID" )) %>%
            dplyr::rename(UnobstructedKm = Dist.base)

          if (season == "base") {
            TodaysFlights <- TodaysFlights %>%
              dplyr::mutate(Destination = FirstChoice) %>%
              dplyr::mutate(ActualKm = UnobstructedKm) %>%
              dplyr::mutate(ExtraKm = 0.0) %>%
              dplyr::mutate(FirstChoiceIsIn = "none")

          } else {

            # Potentially displaced by...
            TodaysFlights <- TodaysFlights %>%
              dplyr::left_join(FPathDists[,c('GridID', 'Start', 'isinORD')],
                               by = c("StartGridIdx" = "Start", "FirstChoice"="GridID")) %>%
              dplyr::rename(FirstChoiceIsIn = isinORD)

            # Fill in the scenario distances, but note the footprint cells will be NA!
            TodaysFlights <- TodaysFlights %>%
              dplyr::inner_join(FlightListB[c("BirdID",paste0("t",tstep))], by = "BirdID") %>%
              dplyr::rename(Destination = paste0("t",tstep)) %>%
              dplyr::left_join(FPathDists[,c('GridID', 'Start', 'Dist.base', 'Dist.scen', 'ExtraKm')],
                               by = c("StartGridIdx" = "Start", "Destination"="GridID" )) %>%
              dplyr::rename(DestnDirectKm = Dist.base) %>%
              dplyr::rename(DestnBarrieredKm = Dist.scen)

            # How far does each bird actually fly, one way?
            TodaysFlights <- TodaysFlights %>%
              dplyr::mutate(ActualKm = purrr::pmap_dbl(
                list(wfde, wfbe, DestnDirectKm, DestnBarrieredKm),
                ~ ifelse({..1}==0, {..3}, ifelse({..2}==0,{..3},{..4})))
              ) %>%
              dplyr::mutate(ActualKm = round(ActualKm, 3))
          }

          # Displaced?
          TodaysFlights <- TodaysFlights %>%
            dplyr::mutate(Displaced = !(FirstChoice == Destination))

          # > Look for barriered birds
          TodaysFlights <- TodaysFlights %>%
            dplyr::mutate(BarrierOut = purrr::pmap_lgl(
              list(wfbe, ExtraKm), ~ ifelse(({..1}==1 & {..2}>0 & !is.na({..2})), TRUE, FALSE))) %>%
            dplyr::mutate(BarrierRetn = BarrierOut)

          # > Look for birds at risk of collision with turbines
          TodaysFlights <- TodaysFlights %>%
            dplyr::mutate(CollisionRisk = purrr::pmap_lgl(
              list(wfbe, ExtraKm), ~ ifelse(({..1}==0 & {..2}>0 | is.na({..2})), TRUE, FALSE)))

          # > Find the distance flown for the outward & return trips $*$*$*$
          # Allowing for future expansion where out and return are different
          # AdditionalOutwardm = the difference between the baseline and the scenario for
          # a SINGLE trip today

          TodaysFlights <- TodaysFlights %>%
            dplyr::mutate(Outwardm = 1000.0 * ActualKm) %>%
            dplyr::mutate(AdditionalOutwardm = 1000.0 * (ActualKm - UnobstructedKm)) %>%
            dplyr::mutate(Returnm = 1000.0 * ActualKm) %>%
            dplyr::mutate(AdditionalReturnm = AdditionalOutwardm)

          # > Total distance flown and time spent flying on ONE flight
          TodaysFlights <- TodaysFlights %>%
            dplyr::mutate(Flighthrs = (Outwardm + Returnm) / (3600.0*Species$data$flight_msec))

          # Record the foraged locations for display
          i <- raster::aggregate(TodaysFlights$Destination > 0,
                                 by = list(c=TodaysFlights$Destination), FUN=sum)
          BirdFlightMap[i$c] <- BirdFlightMap[i$c] + i$x

          # Todays competition map
          TodaysForageComp <- base_grid
          values(TodaysForageComp) <- ForageComp[[season]][,tstep]

          # ==========================================================================
          out_daystep <- seabord_daystep(Species = Species,
                                         Nscalefactor = Par$Nscalefactor,
                                         popbirdsperkm2 = thisRun$popbirdsperkm2,
                                         TimeVals = TimeVals,
                                         TodaysFlights = TodaysFlights,
                                         TodaysForageComp = TodaysForageComp,
                                         PreyAvailable = PreyAvailable,
                                         BirdType = BirdType,
                                         BirdState = BirdState,
                                         ChickState = ChickState,
                                         Opt_BM_chick = Opt_BM_chick,
                                         base_grid = base_grid,
                                         fixedVals = fixedVals)

          # Setting dead chicks to NA at the end of the season so that those that died on day 30 are recorded
          # correctly for individual plotting purposes:
          if (0 %in% ChickState$data$is_chick_alive) {
            ChickState$data <- ChickState$data %>% split(.$is_chick_alive==0)
            ChickState$data$`TRUE` <- ChickState$data$`TRUE` %>%
              dplyr::mutate(BM_chick = NA) %>%
              dplyr::mutate(BM_condition = NA) %>%
              dplyr::mutate(Ereq_chick = NA)    %>%
              dplyr::mutate(unattend_hrs = NA)
            ChickState$data <- bind_rows(ChickState$data$`TRUE`, ChickState$data$`FALSE`) %>%
              dplyr::arrange(PairID)
          }


          BirdState <- out_daystep$BirdState
          ChickState <- out_daystep$ChickState
          Opt_BM_chick <- out_daystep$Opt_BM_chick

          # ==========================================================================

          # Update the flight record for the season
          TodaysFlights <- TodaysFlights %>%
            dplyr::left_join(BirdState$data[c("BirdID","trips_n")], by = c("BirdID"))
          FlightRecord$data <-  update_flightrecord(FlightRecord$data, TodaysFlights)

          # Write out if high level debugging needed
          # Warning! Don't do this if multiple replicate pairs!
          if (switches$printdaily) {

            birds <- dplyr::left_join(BirdType$data, BirdState$data,
                                      by = c("BirdID","PairID")) %>%
              dplyr::select(-any_of("Destination")) %>%
              dplyr::left_join(TodaysFlights[c("BirdID",
                                               "FirstChoice", "UnobstructedKm",
                                               "Flighthrs")], by = c("BirdID"))
            if (!season == "base") {
              birds <- birds %>%
                dplyr::left_join(TodaysFlights[c("BirdID","Destination", "DestnDirectKm",
                                                 "DestnBarrieredKm","ActualKm",
                                                 "FirstChoiceIsIn", "Displaced", "ExtraKm",
                                                 "BarrierOut", "CollisionRisk",
                                                 "AdditionalOutwardm")], by = c("BirdID"))
            }

            birdsmetadata <- bind_rows(BirdType$metadata, BirdState$metadata, ChickState$metadata, tidyr::tribble(
              ~VarName, ~VarDescription, ~VarUnits,
              "FirstChoice","The cell chosen initially","",
              "UnobstructedKm","The distance by sea to the FirstChoice, without ORDs","km",
              "Flighthrs","The time taken to fly one round trip to Destination and back","hrs",
              "Destination","The actual cell visited this time step","",
              "DestnDirectKm","The distance by sea to Destination, without ORDs","km",
              "DestnBarrieredKm","The distance by sea to Destination, with ORDs present","km",
              "ActualKm","The distance to fly one round trip to Destination and back for this bird. Depends on the wfde and wfbe for the bird.","km",
              "FirstChoiceIsIn","The original cell was in this footprint","",
              "Displaced","Was the bird actually displaced this time step?","T/F",
              "ExtraKm","The difference in distance by sea to Destination with and without ORDs present. This indicates if a barrier effect might apply or if a bird could be exposed to collision","",
              "BarrierOut","Did this bird experience a barrier effect today?","T/F",
              "CollisionRisk","Did this bird experience a collision risk today?","T/F",
              "AdditionalOutwardm","The difference in distance by sea (one way) experienced by this bird due to the ORDs. Includes displacement and barrier effects.","m")
            ) %>%
              dplyr::distinct()

            if (switches$environment %in% c("serial", "CEF", "CEFtest", "test")) {
              dir.create(file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp), showWarnings = FALSE)
              write_csv(birds, file = file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp,
                                                paste0("Birds_", season, "_day_", formatC(tstep, width=3, flag="0"),".csv")))

              write_csv(ChickState$data, file = file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp,
                                                          paste0("Chicks_", season, "_day_", formatC(tstep, width=3, flag="0"),".csv")))
              if (tstep == 1) write_csv(birdsmetadata, file = file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp, "00-METADATA.csv"))
            }

            if(switches$environment == "parallel"){
              dir.create(file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp), showWarnings = FALSE)
              write_csv(birds, file = file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp,
                                                paste0("Birds_", season,"_rep",modPar$Nparallel, "_day_", formatC(tstep, width=3, flag="0"),".csv")))

              write_csv(ChickState$data, file = file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp,
                                                          paste0("Chicks_", season,"_rep",modPar$Nparallel, "_day_", formatC(tstep, width=3, flag="0"),".csv")))
              if (tstep == 1) write_csv(birdsmetadata, file = file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp, "00-METADATA.csv"))
            }
          }

          # > At sea survey calculation here if required (see SeabORD-M)
          SurveyDay <- FALSE
          if (SurveyDay) {}

        }

      } # (next tstep) or end of the season ------------------------------------

      # > Calculate end of season metrics --------------------------------------

      # Adult end of season summary
      a0 <- dasummary(BirdState$data, simrun, season, tstep, Par$Pmedian[simrun])
      output_a0 <- update_steplist(output_a0, a0)

      # Adult flight end of season summary
      f0 <- dfsummary(FlightRecord$data, simrun, season, tstep)
      output_f0 <- update_steplist(output_f0, f0)

      # Chick end of season summary
      c0 <- dcsummary(ChickState$data, simrun, season, tstep, Par$Pmedian[simrun])
      output_c0 <- update_steplist(output_c0, c0)

      # > Calculate end of Year summary ----------------------------------------

      # BM_adult in the following needs to be YearBirds[[base]]
      if (season == "base") {
        meanbm <- mean(BirdState$data$BM_adult, na.rm = T)
      } else {
        meanbm <- mean(YearBirds$base$BM_adult, na.rm = T)
      }

      # Mass survival - Which birds will survive over winter?
      YearBirds[[season]] <- BirdState$data %>%
        dplyr::select(BirdID, BM_adult_t0, BM_adult, is_alive) %>%
        dplyr::mutate(pSurvival = purrr::map(
          BM_adult, .f = calc_pSurvival,
          sp = Par$thisSpecies,
          bm = meanbm,
          basesurv = c(Species$data$basesurv_poor,
                       Species$data$basesurv_modr,
                       Species$data$basesurv_good),
          beta = Species$data$beta,
          sd = sd(BM_adult, na.rm = T))
        ) %>%
        dplyr::mutate(Survived = purrr::map(pSurvival, rbinom, n=3, size=1))
      YearBirds[[season]] <- tidyr::unnest_wider(YearBirds[[season]], col = pSurvival, names_sep = "_")
      YearBirds[[season]] <- tidyr::unnest_wider(YearBirds[[season]], col = Survived, names_sep = "_")

      # Individual birds end of Season State
      YearBirds[[season]] <- YearBirds[[season]]  %>%
        dplyr::left_join(BirdType$data[c("BirdID","wfde","wfbe")], by = c("BirdID")) %>%
        dplyr::left_join(FlightRecord$data, by = 'BirdID')

      # Save the end of year population data
      if (switches$bysus) {
        indat <- YearBirds[[season]] %>% group_by(is_alive, wfde, wfbe)
      } else {
        indat <- YearBirds[[season]] %>% group_by(is_alive)
      }
      y0 <- yasummary(indat, simrun, season, Par$Pmedian[simrun])
      output_y0 <- update_steplist(output_y0, y0)


      if (switches$printseason) {

        if (switches$environment %in% c("serial", "CEF", "CEFtest", "test")) {
          birds <- dplyr::left_join(BirdType$data, BirdState$data, by = c("BirdID","PairID")) %>%
            dplyr::left_join(YearBirds[[season]],
                             by = c("BirdID","BM_adult_t0","BM_adult", "is_alive"), keep = FALSE)
          write_csv(birds, file = file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp,
                                            paste0("Birds_season_", season, formatC(simrun, width=5, flag="0"),".csv")))
          write_csv(ChickState$data, file = file.path(modPar$outputdir, thisRun$stamp2, thisRun$stamp,
                                                      paste0("Chicks_season_", season, formatC(simrun, width=5, flag="0"),".csv")))
        }

        if (switches$environment == "parallel"){
          birds <- dplyr::left_join(BirdType$data, BirdState$data, by = c("BirdID","PairID")) %>%
            dplyr::left_join(YearBirds[[season]],
                             by = c("BirdID","BM_adult_t0","BM_adult", "is_alive"), keep = FALSE)
          dir.create(file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp), showWarnings = FALSE)
          write_csv(birds, file = file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp,
                                            paste0("Birds_", season, formatC(as.numeric(modPar$Nparallel), width=3, flag="0"),".csv")))
          write_csv(ChickState$data, file = file.path(modPar$outputdir,thisRun$stamp2, thisRun$stamp,
                                                      paste0("Chicks_", season, formatC(as.numeric(modPar$Nparallel), width=3, flag="0"),".csv")))
        }
      }
    } # (next season) or end of the baseline or scenario season

    #--> Calculate end of pair metrics ------------------------------------------>

    # We have YearBirds$base and, optionally, YearBirds$scen.
    EndSummary <- BirdType$data %>%
      left_join(YearBirds$base, by = c("BirdID", "wfde", "wfbe")) %>%
      dplyr::select(-any_of(c("TotN_C_risk", "TotN_D_only", "TotN_B_only",
                              "TotN_BD", "Tot_extrakm", "DispGT0", "BarrGT0")))

    if (switches$modelmode == "scenario"){

      #> ADULTS

      # We have output from two matching seasons - now find the differences
      EndSummary <-  EndSummary %>%
        left_join(YearBirds$scen, by = c("BirdID","wfde","wfbe"),
                  suffix = c(".base", ".scen"))

      EndSummary <- EndSummary %>%
        dplyr::mutate(TotN_None.diff = TotN_None.scen - TotN_None.base) %>%
        dplyr::mutate(TotN_trips.diff = TotN_trips.scen - TotN_trips.base) %>%
        dplyr::mutate(Flightkm.diff = (Tot_basickm.scen + Tot_extrakm) - Tot_basickm.base)

      # Summarise all birds together - regional output
      i0r <- calc_summaryby(EndSummary, r = simrun)
      output_i0 <- update_summarylist(output_i0, i0r)
      if (switches$bycol) {
        i0c <- calc_summaryby(EndSummary, by = colony, r = simrun)
        output_i0 <- update_summarylist(output_i0, i0c)
      }

      # Grouped by DB 0 & 0
      # Birds never directly impacted
      EndSummary$i <- (EndSummary$DispGT0==0 & EndSummary$BarrGT0==0)
      i1r <- calc_summaryby(EndSummary, by = i, r = simrun)
      output_i1 <- update_summarylist(output_i1, i1r)
      if (switches$bycol) {
        i1c <- calc_summaryby(group_by(EndSummary,colony),by = i, r = simrun)
        output_i1 <- update_summarylist(output_i1, i1c)
      }

      # Grouped by DB 1 or 1
      # Birds directly impacted in some way at least once
      EndSummary$i <- (EndSummary$DispGT0==1 | EndSummary$BarrGT0==1)
      i2r <- calc_summaryby(EndSummary, by = i, r = simrun)
      output_i2 <- update_summarylist(output_i2, i2r)
      if (switches$bycol) {
        i2c <- calc_summaryby(group_by(EndSummary,colony),by = i, r = simrun)
        output_i2 <- update_summarylist(output_i2, i2c)
      }

      # Grouped by DB 1 & 0
      # Birds displaced at least once, never barriered
      EndSummary$i <- (EndSummary$DispGT0==1 & EndSummary$BarrGT0==0)
      i3r <- calc_summaryby(EndSummary, by = i, r = simrun)
      output_i3 <- update_summarylist(output_i3, i3r)
      if (switches$bycol) {
        i3c <- calc_summaryby(group_by(EndSummary,colony), by = i, r = simrun)
        output_i3 <- update_summarylist(output_i3, i3c)
      }

      # Grouped by DB 0 & 1
      # Birds barriered at least once, never displaced
      EndSummary$i <- (EndSummary$DispGT0==0 & EndSummary$BarrGT0==1)
      i4r <- calc_summaryby(EndSummary, by = i, r = simrun)
      output_i4 <- update_summarylist(output_i4, i4r)
      if (switches$bycol) {
        i4c <- calc_summaryby(group_by(EndSummary,colony), by = i, r = simrun)
        output_i4 <- update_summarylist(output_i4, i4c)
      }

      # Grouped by DB 1 & 1
      # Birds barriered and displaced at least once
      EndSummary$i <- (EndSummary$DispGT0==1 & EndSummary$BarrGT0==1)
      i5r <- calc_summaryby(EndSummary, by = i, r = simrun)
      output_i5 <- update_summarylist(output_i5, i5r)
      if (switches$bycol) {
        i5c <- calc_summaryby(group_by(EndSummary, colony), by = i, r = simrun)
        output_i5 <- update_summarylist(output_i5, i5c)
      }

      # Group by all impacts separately
      EndSummary <- EndSummary %>% group_by(TotN_None.scen, TotN_D_only,
                                            TotN_B_only, TotN_BD)
      i6r <- calc_summaryby(EndSummary, r = simrun)
      output_i6 <- update_summarylist(output_i6, i6r)
      if (switches$bycol) {
        i6c <- calc_summaryby(EndSummary, by = colony, r = simrun)
        output_i6 <- update_summarylist(output_i6, i6c)
      }

    } else {


    }

  }

  ##============================================================================
  ## SECTION -- Output & close --

  ## SUBSECTION -- Output --

  if (switches$minout){

    # Prepare a single tibble with only the essentials (primarily for the CEF)
    Ta <- output_a0$data %>%
      dplyr::select(any_of(c('Rep', 'Prey', 'Season',
                             'colony', 'N_alive_ad', 'N_dead_ad',
                             'BM_adult_t0.mn', 'BM_adult_t0.sd',
                             'BM_adult.mn', 'BM_adult.sd',
                             'AdultsSurvivingBS')))

    Tc <- output_c0$data %>%
      dplyr::select(any_of(c('Rep', 'Prey', 'Season',
                             'colony', 'N_alive_ch', 'N_dead_ch',
                             'BM_chick.mn', 'BM_chick.sd',
                             'ChicksPerNest')))

    Ty <- output_y0$data %>%
      dplyr::select(any_of(c('Rep','Prey','Season','colony','Survived_modr',
                             'AdultsSurvivingYr')))

    result <- dplyr::left_join(Ta, Ty, by = c('Rep', 'Prey', 'Season')) %>%
      dplyr::left_join(Tc, by = c('Rep', 'Prey', 'Season'))


    # ... and the metadata
    resultmeta <- bind_rows(output_a0$metadata,
                            output_y0$metadata,
                            output_c0$metadata) %>%
      dplyr::filter(VarName %in% names(result)) %>% dplyr::distinct()

    # Return
    SeabORDSummary <- list(
      result = result,
      resultmeta = resultmeta
    )

  } else {

    # Create a list of the output for the whole run
    SeabORDSummary <- list(switches = switches,
                           Parameters = Par,
                           ordPar = ordPar,
                           modPar = modPar,
                           thisRun = thisRun,
                           output_f0 = output_f0,
                           output_c0 = output_c0
    )

    if (switches$debugmode > 0) {   # NOTE: will need to remove this as debugmode likely going to be defunct
      SeabORDSummary$BirdFlightMap <- BirdFlightMap
      SeabORDSummary$output_a0 <- output_a0
      SeabORDSummary$output_y0 <- output_y0
    } else {
      SeabORDSummary$output_a0$data <- output_a0$data %>%
        dplyr::select(!(contains("mode")))
      SeabORDSummary$output_a0$metadata <- output_a0$metadata %>%
        dplyr::filter(VarName %in% names(SeabORDSummary$output_a0$data))
      SeabORDSummary$output_y0$data <- output_y0$data %>%
        dplyr::select(!(contains("poor") | contains("good")))
      SeabORDSummary$output_y0$metadata <- output_y0$metadata %>%
        dplyr::filter(VarName %in% names(SeabORDSummary$output_y0$data))
      SeabORDSummary$BirdFlightMap <- BirdFlightMap
    }

    # IF there are ORDs, add in the other tables
    # Note: chick tibbles are incomplete so not included yet !!

    if (switches$modelmode == "scenario") {
      if (switches$debugmode > 0) {

        SeabORDSummary$output_i0 <- output_i0
        SeabORDSummary$output_i1 <- output_i1
        SeabORDSummary$output_i2 <- output_i2
        SeabORDSummary$output_i3 <- output_i3
        SeabORDSummary$output_i4 <- output_i4
        SeabORDSummary$output_i5 <- output_i5
        SeabORDSummary$output_i6 <- output_i6

      } else {

        SeabORDSummary$output_i0$adults <- output_i0$adults
        SeabORDSummary$output_i0$survival$data <- output_i0$survival$mod
        SeabORDSummary$output_i0$survival$metadata <- output_i0$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i0$survival$data))
        SeabORDSummary$output_i1$adults <- output_i1$adults
        SeabORDSummary$output_i1$survival$data <- output_i1$survival$mod
        SeabORDSummary$output_i1$survival$metadata <- output_i1$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i1$survival$data))
        SeabORDSummary$output_i2$adults <- output_i2$adults
        SeabORDSummary$output_i2$survival$data <- output_i2$survival$mod
        SeabORDSummary$output_i2$survival$metadata <- output_i2$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i2$survival$data))
        SeabORDSummary$output_i3$adults <- output_i3$adults
        SeabORDSummary$output_i3$survival$data <- output_i3$survival$mod
        SeabORDSummary$output_i3$survival$metadata <- output_i3$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i3$survival$data))
        SeabORDSummary$output_i4$adults <- output_i4$adults
        SeabORDSummary$output_i4$survival$data <- output_i4$survival$mod
        SeabORDSummary$output_i4$survival$metadata <- output_i4$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i4$survival$data))
        SeabORDSummary$output_i5$adults <- output_i5$adults
        SeabORDSummary$output_i5$survival$data <- output_i5$survival$mod
        SeabORDSummary$output_i5$survival$metadata <- output_i5$survival$metadata %>%
          dplyr::filter(VarName %in% names(SeabORDSummary$output_i5$survival$data))
      }

    }
  }

  if (switches$savebirdflightmap == T) {

    # change zeros to NAs to trim to extent of flights so that x and y limits
    # are scaled according to which colony is being simulated:
    BFM_zeros <- reclassify(BirdFlightMap, cbind(0, NA))
    BFM_trim <- trim(BFM_zeros)
    p <- ggplot() + ggspatial::layer_spatial(SeabORDSummary$BirdFlightMap) +
      scale_fill_viridis_c(option = "B", direction = -1, na.value = "transparent") +
      ggtitle("Foraging locations") +  ylim(BFM_trim@extent@ymin, BFM_trim@extent@ymax) + xlim(BFM_trim@extent@xmin, BFM_trim@extent@xmax)

    if(switches$environment == "parallel"){
      ggsave(file.path(modPar$outputdir,thisRun$stamp2, paste0("BFlightMap_", gsub("\\.", "_", make.names(paste0(thisRun$refname,"_", Sys.Date()))),".png")),
             p, device="png",width=9, height=9)
    } else {
      ggsave(file.path(modPar$outputdir,thisRun$stamp2, paste0("BFlightMap_", gsub("\\.", "_", make.names(paste0(thisRun$refname,"_", Sys.Date()))),".png")),
             p, device="png",width=9, height=9)
    }
  }

  if (switches$saverds) {
    if (switches$environment =="parallel"){
      saveRDS(SeabORDSummary, file.path(outputdir,thisRun$stamp2, paste0("sb_out_", gsub("\\.", "_", make.names(paste0(thisRun$refname,"_", Sys.time()))),".rds")))
    }
  }


  ## Return the list
  return(SeabORDSummary)

  ## SUBSECTION -- Close --
}
