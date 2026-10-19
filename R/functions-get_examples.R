#' Create RasterLayer from metadata
#'
#' @param metadata List containing raster metadata.
#'
#' @return A RasterLayer object.
#' @export
raster_from_metadata <- function(metadata) {

  if (!is.list(metadata)) {
    stop("'metadata' must be a list.", call. = FALSE)
  }

  required_fields <- c(
    "n_rows", "n_cols",
    "x_min", "x_max",
    "y_min", "y_max",
    "crs"
  )

  missing_fields <- setdiff(required_fields, names(metadata))

  if (length(missing_fields) > 0) {
    stop(
      "Missing metadata fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  numeric_fields <- c(
    "n_rows", "n_cols",
    "x_min", "x_max",
    "y_min", "y_max"
  )

  invalid_fields <- numeric_fields[
    !vapply(
      metadata[numeric_fields],
      function(x) is.numeric(x) && length(x) == 1 && !is.na(x),
      logical(1)
    )
  ]

  if (length(invalid_fields) > 0) {
    stop(
      "These fields must be non-missing numeric scalars: ",
      paste(invalid_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(metadata$crs) || is.na(metadata$crs) || metadata$crs == "") {
    stop("'crs' must be supplied.", call. = FALSE)
  }

  raster::raster(
    nrows = metadata$n_rows,
    ncols = metadata$n_cols,
    xmn   = metadata$x_min,
    xmx   = metadata$x_max,
    ymn   = metadata$y_min,
    ymx   = metadata$y_max,
    crs   = metadata$crs
  )
}

#' Create a crop extent around a colony
#'
#' @param colony An sf point object.
#' @param km_to_left Distance (km) west.
#' @param km_to_right Distance (km) east.
#' @param km_up Distance (km) north.
#' @param km_down Distance (km) south.
#'
#' @return A RasterExtent object.
#'
#' @export
create_crop_extent_colony <- function(
    colony,
    km_to_left = 30,
    km_to_right = 50,
    km_up = 30,
    km_down = 70
) {

  raster::extent(
    sf::st_bbox(colony)$xmin - (km_to_left * 1000),
    sf::st_bbox(colony)$xmin + (km_to_right * 1000),
    sf::st_bbox(colony)$ymin - (km_down * 1000),
    sf::st_bbox(colony)$ymin + (km_up * 1000)
  )
}

#' Load Example Seamask Raster
#'
#' Reconstructs the example seamask raster distributed with the package.
#' The function creates a raster from the stored metadata, populates it
#' with the example seamask values, and assigns the layer name
#' `"seamask_3035"`.
#'
#' @return A `RasterLayer` object containing the example seamask data.
#'
#' @details
#' The example seamask is stored internally as a list containing:
#' \itemize{
#'   \item `metadata`: raster dimensions, extent and coordinate reference system.
#'   \item `matrix`: raster cell values.
#' }
#'
#' The raster is reconstructed using
#' [raster_from_metadata()] and then populated with the stored values.
#'
#' @examples
#' seamask <- get_example_seamask()
#'
#' # Inspect raster properties
#' seamask
#'
#' # Plot seamask
#' raster::plot(seamask)
#'
#' @export
get_example_seamask <- function(){

  #load the data
  example_data_seamask <- seabORD::seamask_3035_example

  #make a raster with the right dimension/proj/extents etc, you can use the built in function raster_from_metadata()
  seamask_example <- seabORD::raster_from_metadata(example_data_seamask$metadata)

  #fill it with the data available
  seamask_example <- raster::setValues(seamask_example, #the raster
                                       example_data_seamask$matrix) #the values

  #Set the name of the layer
  names(seamask_example) <- "seamask_3035"

  return(seamask_example)

}

#' Load an example SPA site record
#'
#' Retrieves the coordinates and associated information for a single site
#' from the example `spacoordinates` dataset included with the package.
#'
#' @param site_code Character string giving the site code to extract.
#'   Defaults to `"UK9004171"`.
#'
#' @return A tibble containing the matching row(s) from
#'   `spacoordinates`. If the supplied `site_code` is not found, an empty
#'   tibble is returned.
#'
#' @details
#' This function provides convenient access to a single example site from
#' the package dataset. It is primarily intended for demonstrations,
#' tutorials, and testing workflows. Site designation information for the
#' same SPA can be obtained with [get_example_spadat2()].
#'
#' @examples
#' # Load the default example site
#' spa_site <- get_example_spadat1()
#'
#' # Load a specific site
#' spa_site <- get_example_spadat1("UK9004171")
#'
#' # Retrieve the corresponding SPA information
#' spa_info <- get_example_spadat2("UK9004171")
#'
#' @seealso
#' [get_example_spadat2()] for retrieving SPA designation information for
#' the same site.
#'
#' @export
get_example_spadat1 <- function(site_code = "UK9004171") {

  tibble::as_tibble(
    dplyr::filter(
      seabORD::spacoordinates,
      SITECODE == site_code
    )
  )

}


#' Load an example SPA site designation record
#'
#' Retrieves information for a single Special Protection Area (SPA) from
#' the example `spalist` dataset included with the package.
#'
#' @param site_code Character string giving the site code to extract.
#'   Defaults to `"UK9004171"`.
#'
#' @return A tibble containing the matching row(s) from `spalist`. If the
#'   supplied `site_code` is not found, an empty tibble is returned.
#'
#' @details
#' This function provides convenient access to an example SPA record from
#' the package dataset and is intended for demonstrations, tutorials, and
#' testing workflows.
#'
#' @examples
#' # Load the default example SPA
#' spa <- get_example_spadat2()
#'
#' # Load a specific SPA by site code
#' spa <- get_example_spadat2("UK9004171")
#'
#' @seealso
#' [get_example_spadat1()] for retrieving site coordinate information.
#'
#' @export
get_example_spadat2 <- function(site_code = "UK9004171") {

  tibble::as_tibble(
    dplyr::filter(
      seabORD::spalist,
      SITE_CODE == site_code
    )
  )

}



#' Load species-specific example data
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
#' ki_data <- get_spdat_example("KI")
#'
#' # Common Guillemot
#' gu_data <- get_spdat_example("GU")
#'
#' # Razorbill
#' ra_data <- get_spdat_example("RA")
#'
#' # Atlantic Puffin
#' pu_data <- get_spdat_example("PU")
#'
#' @export
get_spdat_example <- function(species_code = "KI") {

  dplyr::filter(
    seabORD::energeticsandpreydata,
    Code == species_code
  )

}



#' Load example seabird density raster data
#'
#' Creates a raster layer from the example `BrdData_example` dataset
#' included with the package. The raster is reconstructed from the stored
#' metadata and populated with the associated seabird density values.
#'
#' @return
#' A `RasterLayer` containing example seabird density data for the Forth
#' Islands study area. The raster includes the correct extent, resolution,
#' and coordinate reference system derived from the example metadata.
#'
#' @details
#' This function provides a ready-to-use example raster for demonstrations,
#' tutorials, and testing workflows. The raster is created from the
#' built-in `BrdData_example` dataset using
#' [raster_from_metadata()] and populated with the associated bird density
#' values.
#'
#' The returned raster layer is named `"Forth.Islands"`.
#'
#' @examples
#' # Load the example bird density raster
#' brddata <- get_example_brddata()
#'
#' # Inspect the raster
#' brddata
#'
#' # Plot the raster
#' plot(brddata)
#'
#' @seealso
#' [raster_from_metadata()] for creating rasters from metadata,
#' [get_example_seamask()] for loading the example sea mask raster.
#'
#' @export
get_example_brddata <- function() {

  # load the data
  example_brddata <- seabORD::BrdData_example

  # make a raster with the right dimension/proj/extents etc
  brddata_example <- seabORD::raster_from_metadata(
    example_brddata$metadata
  )

  # fill it with the data available
  brddata_example <- raster::setValues(
    brddata_example,
    example_brddata$matrix
  )

  # set the name of the layer
  names(brddata_example) <- "Forth.Islands"

  return(brddata_example)
}


#' Load the example distance-decay bird distribution raster
#'
#' Retrieves and reconstructs the example bird distribution raster used
#' for distance-decay calibration runs at the Whinnyfold colony. The
#' raster is reconstructed from data and metadata supplied with the
#' package.
#'
#' @return A `RasterLayer` containing the example bird distribution map
#'   used in distance-decay calibration examples. The layer is named
#'   `"Whinnyfold"`.
#'
#' @details
#' The raster represents the spatial distribution of birds around the
#' Whinnyfold colony and is used as the `BrdData` input for calibration
#' simulations based on a distance-decay derived utilisation
#' distribution.
#'
#' The raster is reconstructed from values stored in
#' `BrdData_example_dd` together with the associated spatial metadata.
#'
#' @examples
#' \dontrun{
#' # Load the example bird distribution map
#' BrdData_example_dd <- get_example_brddata_dd()
#'
#' # Inspect the raster
#' BrdData_example_dd
#'
#' # Plot the raster
#' raster::plot(BrdData_example_dd)
#' }
#'
#' @seealso
#' [plot_BrdData()] for visualising bird distribution rasters.
#'
#' @export
get_example_brddata_dd <- function() {

  # load the data
  example_brddata <- seabORD::BrdData_example_dd

  # make a raster with the right dimension/proj/extents etc
  brddata_example <- seabORD::raster_from_metadata(
    example_brddata$metadata
  )

  # fill it with the data available
  brddata_example <- raster::setValues(
    brddata_example,
    example_brddata$matrix
  )

  # set the name of the layer
  names(brddata_example) <- "Whinnyfold"

  return(brddata_example)
}


#' Load example competition distribution raster data
#'
#' Creates a raster layer from the example `frgcompdata_example` dataset
#' included with the package. The raster is reconstructed from the stored
#' metadata and populated with the associated competition distribution
#' values.
#'
#' @return
#' A `RasterLayer` containing example competition distribution data for
#' the Forth Islands study area. The raster includes the correct extent,
#' resolution, and coordinate reference system derived from the example
#' metadata.
#'
#' @details
#' This function provides a ready-to-use example raster for
#' demonstrations, tutorials, and testing workflows.
#'
#' The raster represents a competition distribution map, i.e. the
#' distribution of conspecifics from surrounding colonies that may
#' interact with the colony or SPA being simulated. This information is
#' used throughout the seabORD model to estimate how competition from
#' other colonies influences intake rate at different foraging locations.
#'
#' The raster is reconstructed from the built-in
#' `frgcompdata_example` dataset using [raster_from_metadata()] and
#' populated with the associated competition distribution values.
#'
#' The returned raster layer is named `"Forth.Islands"`.
#'
#' @examples
#' # Load the example competition distribution raster
#' frgcompdata <- get_example_frgcompdata()
#'
#' # Inspect the raster
#' frgcompdata
#'
#' # Plot the raster
#' plot(frgcompdata)
#'
#' @seealso
#' [plot_frgcompdata()] for plotting the competition distribution raster,
#' [raster_from_metadata()] for creating rasters from metadata.
#'
#' @export
get_example_frgcompdata <- function() {

  # load the data
  example_frgcompdata <- seabORD::frgcompdata_example

  # make a raster with the right dimension/proj/extents etc
  frgcompdata_example <- seabORD::raster_from_metadata(
    example_frgcompdata$metadata
  )

  # fill it with the data available
  frgcompdata_example <- raster::setValues(
    frgcompdata_example,
    example_frgcompdata$matrix
  )

  # set the name of the layer
  names(frgcompdata_example) <- "Forth.Islands"

  return(frgcompdata_example)

}

#' Load the example competition distribution raster for distance-decay calibration
#'
#' Retrieves and reconstructs the example competition distribution raster
#' used in the distance-decay calibration workflow for the Whinnyfold
#' colony. The raster is reconstructed from data included with the
#' package and returned as a `RasterLayer`.
#'
#' @return A `RasterLayer` containing the example competition
#'   distribution map used in distance-decay calibration examples. The
#'   layer is named `"Whinnyfold"`.
#'
#' @details
#' The competition distribution map represents the distribution of
#' conspecifics originating from surrounding colonies that may interact
#' with birds from the focal colony. This layer is used throughout the
#' model to estimate competition for resources within the foraging
#' landscape.
#'
#' The raster is reconstructed from values stored in
#' `frgcompdata_example_dd` together with the associated spatial
#' metadata.
#'
#' @examples
#' \dontrun{
#' # Load the example competition distribution map
#' FrgCompData_example_dd <- get_example_frgcompdata_dd()
#'
#' # Inspect the raster
#' FrgCompData_example_dd
#'
#' # Plot the raster
#' raster::plot(FrgCompData_example_dd)
#' }
#'
#' @seealso
#' [get_example_brddata_dd()] for the corresponding bird distribution
#' raster used in the calibration example.
#'
#' @export
get_example_frgcompdata_dd <- function() {

  # load the data
  example_frgcompdata <- seabORD::frgcompdata_example_dd

  # make a raster with the right dimension/proj/extents etc
  frgcompdata_example <- seabORD::raster_from_metadata(
    example_frgcompdata$metadata
  )

  # fill it with the data available
  frgcompdata_example <- raster::setValues(
    frgcompdata_example,
    example_frgcompdata$matrix
  )

  # set the name of the layer
  names(frgcompdata_example) <- "Whinnyfold"

  return(frgcompdata_example)

}

#' Load example flight distance raster
#'
#' Creates a raster layer from the example `UK9004171_bysea_3035`
#' dataset included with the package. The raster is reconstructed from
#' the stored metadata and populated with the associated flight distance
#' values.
#'
#' @return
#' A `RasterLayer` containing example flight distances from SPA
#' `"UK9004171"`. The raster includes the correct extent, resolution,
#' and coordinate reference system derived from the example metadata.
#'
#' @details
#' This function provides a ready-to-use example raster for
#' demonstrations, tutorials, and testing workflows.
#'
#' The raster represents the distance at sea from the example SPA
#' `"UK9004171"` to each raster cell. Flight distance is a key input to
#' seabORD and is used throughout the model to estimate travel costs
#' associated with foraging at different locations.
#'
#' The raster is reconstructed from the built-in
#' `UK9004171_bysea_3035` dataset using [raster_from_metadata()] and
#' populated with the associated distance values.
#'
#' The returned raster layer is named `"UK9004171"`.
#'
#' @examples
#' # Load the example flight distance raster
#' fltdist_base <- get_example_fltdist_base()
#'
#' # Inspect the raster
#' fltdist_base
#'
#' # Plot the raster
#' plot(fltdist_base)
#'
#' @seealso
#' [raster_from_metadata()] for creating rasters from metadata.
#'
#' @export
get_example_fltdist_base <- function() {

  # load the data
  example_UK9004171_bysea <- seabORD::UK9004171_bysea_3035

  # make a raster with the right dimension/proj/extents etc
  fltdist_base_example <- seabORD::raster_from_metadata(
    example_UK9004171_bysea$metadata
  )

  # fill it with the data available
  fltdist_base_example <- raster::setValues(
    fltdist_base_example,
    example_UK9004171_bysea$matrix
  )

  # set the name of the layer
  names(fltdist_base_example) <- "UK9004171"

  return(fltdist_base_example)

}

#' Load the example flight-distance raster for distance-decay calibration
#'
#' Retrieves and reconstructs the example flight-distance raster used in
#' the distance-decay calibration workflow for the Whinnyfold colony.
#' The raster is reconstructed from example data included with the
#' package and returned as a `RasterLayer`.
#'
#' @return A `RasterLayer` containing the example flight-distance map
#'   used in distance-decay calibration examples. The layer is named
#'   `"UK9002491"`.
#'
#' @details
#' The flight-distance map represents the distance travelled over sea
#' from the colony to all accessible locations within the study area.
#' This raster is used as the `fltdist_base` input to the SeabORD model
#' and provides the baseline flight distances used in energy budget and
#' foraging calculations.
#'
#' The raster is reconstructed from values stored in
#' `UK9002491_bysea_3035` together with the associated spatial metadata.
#'
#' @examples
#' \dontrun{
#' # Load the example flight-distance map
#' fltdist_base_example <- get_example_fltdist_base_dd()
#'
#' # Inspect the raster
#' fltdist_base_example
#'
#' # Plot the raster
#' raster::plot(fltdist_base_example)
#' }
#'
#' @seealso
#' [plot_fltdist_base()] for visualising flight-distance rasters.
#'
#' @export
get_example_fltdist_base_dd <- function() {

  # load the data
  example_UK9002491_bysea <- seabORD::UK9002491_bysea_3035

  # make a raster with the right dimension/proj/extents etc
  fltdist_base_example <- seabORD::raster_from_metadata(
    example_UK9002491_bysea$metadata
  )

  # fill it with the data available
  fltdist_base_example <- raster::setValues(
    fltdist_base_example,
    example_UK9002491_bysea$matrix
  )

  # set the name of the layer
  names(fltdist_base_example) <- "UK9002491"

  return(fltdist_base_example)

}


#' Load the example Distance By Sea (DBS) map
#'
#' Retrieves and reconstructs the example Distance By Sea (DBS) map
#' included with the package and returns it as a `RasterLayer`.
#' The raster is created from the stored metadata and populated with
#' the example DBS values.
#'
#' @return A `RasterLayer` containing the example Distance By Sea (DBS)
#'   map. The layer is named `"DBS_map"`.
#'
#' @details
#' The DBS map represents the distance travelled over sea between
#' locations. The raster is reconstructed from example data bundled with the package
#' using the associated spatial metadata.
#'
#' @examples
#' \dontrun{
#' DBS_map <- get_example_DBS_map()
#'
#' # Inspect the raster
#' DBS_map
#'
#' # Plot the example DBS map
#' raster::plot(DBS_map)
#' }
#'
#' @export
get_example_DBS_map <- function(){

  example_data_DBS_map <- seabORD::DBS_map_example

  # make a raster with the right dimension/proj/extents etc
  DBS_map_example <- seabORD::raster_from_metadata(
    example_data_DBS_map$metadata
  )

  # fill it with the data available
  DBS_map_example <- raster::setValues(
    DBS_map_example,
    example_data_DBS_map$matrix
  )

  # set the name of the layer
  names(DBS_map_example) <- "DBS_map"

  return(DBS_map_example)

}


#' Load the example Distance By Sea map including ORDs
#'
#' Retrieves and reconstructs the example Distance By Sea (DBS) raster
#' generated in the presence of Offshore Renewable Developments (ORDs).
#' The raster is reconstructed from the stored metadata and populated
#' with the example DBS values.
#'
#' @return A `RasterLayer` containing the example Distance By Sea map
#'   with ORDs included. The layer is named `"DBS_withORDs"`.
#'
#' @details
#' The DBS map represents travel distance over sea rather than
#' straight-line (Euclidean) distance. This example includes the effect
#' of Offshore Renewable Developments (ORDs) on the distance surface and
#' is supplied as part of the package for demonstration and testing
#' purposes.
#'
#' The raster is reconstructed from the example matrix values and the
#' associated spatial metadata stored within the package.
#'
#' @examples
#' \dontrun{
#' # Load the example DBS map including ORDs
#' DBS_withORDs <- get_example_DBS_map_with_ORDs()
#'
#' # Inspect the raster
#' DBS_withORDs
#'
#' # Plot the raster
#' raster::plot(DBS_withORDs)
#' }
#'
#' @seealso
#' [get_example_DBS_map()] for the equivalent Distance By Sea map
#' without ORDs.
#'
#' @export
get_example_DBS_map_with_ORDs <- function (){

  # Load the example
  example_data_DBS_withORDs <- seabORD::DBS_withORDs_example

  # make a raster with the right dimension/proj/extents etc
  DBS_map_example <- seabORD::raster_from_metadata(
    example_data_DBS_withORDs$metadata
  )

  # fill it with the data available
  DBS_map_example <- raster::setValues(
    DBS_map_example,
    example_data_DBS_withORDs$matrix
  )

  # set the name of the layer
  names(DBS_map_example) <- "DBS_withORDs"

  return(DBS_map_example)

}
