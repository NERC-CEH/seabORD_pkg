#' Load species-specific data
#'
#' Retrieves species-specific energetics and prey data from the
#' `energeticsandpreydata` dataset included with the package.
#'
#' @param species_code Character string giving the species code of
#'   interest. Supported values are:
#'   \itemize{
#'     \item `"KI"`: Black-legged Kittiwake
#'     \item `"GU"`: Common Guillemot
#'     \item `"RA"`: Razorbill
#'     \item `"PU"`: Atlantic Puffin
#'   }
#'   Defaults to `"KI"`.
#'
#' @return
#' A data frame containing the species-specific energetics and prey
#' parameters used by seabORD. If the supplied `species_code` is not
#' present in the dataset, an empty data frame is returned.
#'
#' @details
#' This function provides convenient access to the built-in species
#' parameter dataset used by seabORD. Species-specific data for all four
#' seabird species modelled by the package are stored in
#' `energeticsandpreydata`.
#'
#' @examples
#' # Black-legged Kittiwake
#' ki_data <- get_spdat("KI")
#'
#' # Common Guillemot
#' gu_data <- get_spdat("GU")
#'
#' # Razorbill
#' ra_data <- get_spdat("RA")
#'
#' # Atlantic Puffin
#' pu_data <- get_spdat("PU")
#'
#' @export
get_spdat <- function( species_code = "KI") {

  dplyr::filter(
    seabORD::energeticsandpreydata,
    Code == species_code
  )

}


#' Create a colony location point
#'
#' Creates an `sf` point object from longitude and latitude coordinates
#' and projects it to ETRS89 / LAEA Europe (EPSG:3035).
#'
#' @param lon Longitude in decimal degrees (WGS84).
#' @param lat Latitude in decimal degrees (WGS84).
#'
#' @return
#' An `sf` point object in EPSG:3035.
#'
#' @examples
#' # Whinnyfold colony
#' colony <- set_colony_point(
#'   lon = -1.85849,
#'   lat = 57.388807
#' )
#'
#' colony
#'
#' @export
set_colony_point <- function(lon, lat) {

  colony <- sf::st_as_sf(
    data.frame(
      lon = lon,
      lat = lat
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  sf::st_transform(
    colony,
    crs = 3035
  )

}

#' Create buffer polygons around ORDs
#'
#' @param ORDpolys An `sf` polygon or multipolygon object containing
#'   ORD footprints.
#' @param bufferwidth Buffer width in kilometres.
#'
#' @return An `sf` object containing buffered ORD polygons.
#'
#' @export
set_ORD_buffer <- function(
    ORDpolys,
    bufferwidth = 2
) {

  ORDpolys %>%
    sf::st_buffer(bufferwidth * 1000) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(
      ord_id = paste0("ord", dplyr::row_number())
    )
}

#' Calculate the percentage of a DD map within ORD buffer zones
#'
#' Calculates the proportion of a density decay (DD) map that falls
#' within Offshore Renewable Development (ORD) footprints and their
#' associated buffer zones.
#'
#' @param DD_map A normalised density decay `RasterLayer`.
#'
#' @param ORDpolys_with_buffer An `sf` polygon object containing ORD
#'   footprints and buffer zones.
#'
#' @return A numeric value giving the percentage of the DD map contained
#'   within the supplied ORD polygons and buffers.
#'
#' @details
#' The DD map is masked using the supplied polygons and the values
#' remaining within the masked area are summed.
#'
#' If the DD map is normalised to sum to one, the returned value can be
#' interpreted as the percentage of total density contained within the
#' ORD footprints and their buffers.
#'
#' @examples
#' \dontrun{
#' ORDpolys_with_buffer <- set_ORD_buffer(
#'   ORDpolys,
#'   bufferwidth = 2
#' )
#'
#' calc_pc_in_ORD_buffer(
#'   DD_map,
#'   ORDpolys_with_buffer
#' )
#' }
#'
#' @export
calc_pc_in_ORD_buffer <- function(
    DD_map,
    ORDpolys_with_buffer
) {

  maskedmap <- raster::mask(
    DD_map,
    ORDpolys_with_buffer
  )

  100 * raster::cellStats(
    maskedmap,
    stat = "sum"
  )
}


#' Summarise the impact of ORDs on a density decay map
#'
#' Calculates the percentage of a density decay (DD) map contained
#' within ORD footprints and buffer zones, together with the percentage
#' affected by ORD barrier ("shadow") effects.
#'
#' @param DD_map A normalised density decay `RasterLayer`.
#'
#' @param ORDpolys_with_buffer An `sf` polygon object containing ORD
#'   footprints and associated buffer zones.
#'
#' @param DBS_map A `RasterLayer` containing Distance By Sea values
#'   calculated without ORDs.
#'
#' @param DBS_withORDs A `RasterLayer` containing Distance By Sea values
#'   calculated with ORDs present.
#'
#' @param smalltol Numeric threshold used to identify meaningful
#'   increases in Distance By Sea. Defaults to `0.01`.
#'
#' @return
#' A data frame with two columns:
#' \describe{
#'   \item{pc_in_ORD_plus_buffer}{
#'     Percentage of the DD map contained within ORD footprints and
#'     their buffer zones.
#'   }
#'   \item{pc_in_ORD_shadow}{
#'     Percentage of the DD map located within areas where Distance By
#'     Sea increases by more than `smalltol` due to the presence of
#'     ORDs.
#'   }
#' }
#'
#' @details
#' If the density decay map is normalised to sum to one, both outputs
#' can be interpreted directly as percentages of the total density
#' distribution.
#'
#' @examples
#' \dontrun{
#' summarise_ORD_impact(
#'   DD_map = DD_map,
#'   ORDpolys_with_buffer = ORDpolys_with_buffer,
#'   DBS_map = DBS_map,
#'   DBS_withORDs = DBS_withORDs
#' )
#' }
#'
#' @export
summarise_ORD_impact <- function(
    DD_map,
    ORDpolys_with_buffer,
    DBS_map,
    DBS_withORDs,
    smalltol = 0.01
) {

  data.frame(
    pc_in_ORD_plus_buffer =
      100 * sum(
        unlist(
          raster::extract(
            DD_map,
            ORDpolys_with_buffer
          )
        ),
        na.rm = TRUE
      ),

    pc_in_ORD_shadow =
      100 * sum(
        DD_map[
          (DBS_withORDs - DBS_map) > smalltol
        ],
        na.rm = TRUE
      )
  )

}



#' Check calibration input consistency
#'
#' Verifies that the number of calibration replicates matches the
#' number of prey values supplied in `Par$Pmedian`.
#'
#' @param Par A list of seabORD parameters containing `Pmedian`.
#' @param modPar A list of model parameters containing
#'   `Nreplicates`.
#'
#' @return Invisibly returns `TRUE` if the inputs are valid.
#'
#' @details
#' For calibration runs, the number of replicates specified in
#' `modPar$Nreplicates` should equal the number of prey values
#' supplied in `Par$Pmedian`. A mismatch may lead to simulation
#' failures or unexpected behaviour.
#'
#' @examples
#' check_calibration_inputs(
#'   Par = Par_example,
#'   modPar = modPar_example
#' )
#'
#' @export
check_calibration_inputs <- function(
    Par,
    modPar
) {

  n_prey <- length(Par$Pmedian)
  n_reps <- modPar$Nreplicates

  if (n_reps != n_prey) {
    message(
      paste0(
        "Input mismatch: modPar$Nreplicates = ",
        n_reps,
        " but length(Par$Pmedian) = ",
        n_prey,
        ". These values should be identical for calibration runs."
      )
    )
  }

  message(
    "Calibration inputs valid: ",
    n_reps,
    " replicate(s) and ",
    n_prey,
    " prey value(s)."
  )

  invisible(TRUE)

}

#' Create a prey calibration sequence in Par
#'
#' Generates a sequence of prey values spanning a specified range and
#' assigns it to `Par$Pmedian`. The number of values generated is
#' determined by `modPar$Nreplicates`.
#'
#' @param Par A seabORD parameter list.
#' @param modPar A seabORD model parameter list containing
#'   `Nreplicates`.
#' @param Pmin Minimum prey value.
#' @param Pmax Maximum prey value.
#'
#' @return The updated `Par` object with `Pmedian` populated.
#'
#' @examples
#' Par_example <- set_prey_sequence_in_par(
#'   Par = Par_example,
#'   modPar = modPar_example,
#'   Pmin = 150,
#'   Pmax = 200
#' )
#'
#' @export
set_prey_sequence_in_par <- function(
    Par,
    modPar,
    Pmin,
    Pmax
) {

  if (modPar$Nreplicates > 1) {

    Par$Pmedian <- seq(
      from = Pmin,
      to = Pmax,
      by = (Pmax - Pmin) /
        (modPar$Nreplicates - 1)
    )

  } else {

    Par$Pmedian <- Pmax

  }

  Par

}
