#' Create RasterLayer from metadata
#'
#' @param metadata List containing raster metadata.
#'
#' @return A RasterLayer object.
#' @keywords internal
#' @noRd
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

#' Load example parameter lists
#'
#' Retrieves the example parameter lists used throughout the package
#' tutorials and vignettes. The returned object contains the
#' species, and simulation parameters required to run an example
#' seabORD analysis.
#'
#' @return A list containing example parameter objects used by seabORD.
#'
#' @examples
#' \dontrun{
#' par <- get_example_par()
#'
#' # View available parameter objects
#' names(par)
#' }
#'
#' @export
get_example_par <- function(type = NULL) {
  if(is.null(type)){
    seabORD::example_1_lists$Par
  }else if (type == "cal_dd"){
    seabORD::example_lists_calibration_dd$Par
  }else if (type == "cal"){
    seabORD::example_lists_calibration$Par
  }else if (type == "dd"){
    seabORD::example_lists_dd$Par
  }

}

#' Load example model parameters
#'
#' Retrieves the example model parameter list (`modPar`) used throughout
#' the package examples and vignettes. This object contains the model
#' configuration settings required to run a seabORD simulation.
#'
#' @return A list containing the example model parameters.
#'
#' @examples
#' \dontrun{
#' modPar <- get_example_par()
#'
#' # View parameter names
#' names(modPar)
#' }
#'
#' @export
get_example_modpar <- function(type = NULL) {
  if(is.null(type)){
    seabORD::example_1_lists$modPar
  }else if (type == "cal_dd"){
    seabORD::example_lists_calibration_dd$modPar
  }else if (type == "cal"){
    seabORD::example_lists_calibration$modPar
  }else if (type == "dd"){
    seabORD::example_lists_dd$modPar
  }
}

#' Load example ordPar object
#'
#' Retrieves the example offshore renewable development parameter list
#' (`ordPar`) used throughout the package examples and vignettes. This
#' object contains the parameters describing the offshore renewable
#' development scenario.
#'
#' @return A list containing example ORD parameters.
#'
#' @examples
#' ordPar <- get_example_ordPar()
#'
#' @export
get_example_ordPar <- function(type = NULL) {
  if(is.null(type)){
    seabORD::example_1_lists$ordPar
  }else if (type == "cal_dd"){
    seabORD::example_lists_calibration_dd$ordPar
  }else if (type == "cal"){
    seabORD::example_lists_calibration$ordPar
  }else if (type == "dd"){
    seabORD::example_lists_dd$ordPar
  }
}

#' Load example switch settings
#'
#' Retrieves the example switch configuration (`switches`) used throughout
#' the package examples and vignettes. This object contains logical flags
#' and settings that control which model components and outputs are used
#' during a seabORD simulation.
#'
#' @return A list containing example model switch settings.
#'
#' @examples
#' switches <- get_example_switches()
#'
#' @export
get_example_switches <- function(type = NULL) {
  if(is.null(type)){
    seabORD::example_1_lists$switches
  }else if (type == "cal_dd"){
    seabORD::example_lists_calibration_dd$switches
  }else if (type == "cal"){
    seabORD::example_lists_calibration$switches
  }else if (type == "dd"){
    seabORD::example_lists_dd$switches
  }
}

#' Load example flight grid correction raster
#'
#' Retrieves the example flight grid correction TransitionLayer
#' (`FlightGridcorrection_3035`) supplied with the package. This TransitionLayer is
#' used to correct flight distances by accounting for the difference
#' between straight-line movement and movement across the model grid.
#'
#' The TransitionLayer is provided in the ETRS89 / LAEA Europe projection
#' (EPSG:3035).
#'
#' @return A `TransitionLayer` containing example flight grid correction values.
#'
#' @examples
#' fgc <- get_example_fgc_3035()
#' @export
get_example_fgc_3035 <- function(){
  seabORD::FlightGridcorrection_3035
}

#' Load example ORD polygons for distance-decay workflows
#'
#' Retrieves the example offshore renewable development (ORD) polygons
#' (`ORDpoly_example_wfold`) supplied with the package. These polygons
#' represent the wind farm footprints used in the Whinnyfold
#' density-dependence example workflow.
#'
#' @return An `sf` object containing example ORD polygons.
#'
#' @examples
#' ORDpolys <- get_example_ORDpolys_dd()
#'
#' # View the polygons
#' plot(sf::st_geometry(ORDpolys))
#'
#' @export
get_example_ORDpoly <- function(type = NULL) {
  if (is.null(type)){
    seabORD::ORDpoly_example
  }else if (type == "dd"){
    seabORD::ORDpoly_example_wfold
  }
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
  seamask_example <- raster_from_metadata(example_data_seamask$metadata)

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
get_example_spadat1 <- function(type = NULL) {

  if (is.null(type)){
    site_code <- "UK9004171"
  }else if (type == "dd"){
    site_code <- "UK9002491"
  }


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
get_example_spadat2 <- function(type = NULL) {

  if (is.null(type)){
    site_code <- "UK9004171"
  }else if (type == "dd"){
    site_code <- "UK9002491"
  }


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
#' ki_data <- get_example_spdat("KI")
#'
#' # Common Guillemot
#' gu_data <- get_example_spdat("GU")
#'
#' # Razorbill
#' ra_data <- get_example_spdat("RA")
#'
#' # Atlantic Puffin
#' pu_data <- get_example_spdat("PU")
#'
#' @export
get_example_spdat <- function() {
  species_code <- "KI"
  get_spdat(species_code)
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
get_example_brddata <- function(type = NULL) {

  # load the data
  if(is.null(type)){
    example_brddata <- seabORD::BrdData_example
    name_l <- "Forth.Islands"
  }else if (type == "dd"){
    example_brddata <- seabORD::BrdData_example_dd
    name_l <- "Whinnyfold"
  }

  # make a raster with the right dimension/proj/extents etc
  brddata_example <- raster_from_metadata(
    example_brddata$metadata
  )

  # fill it with the data available
  brddata_example <- raster::setValues(
    brddata_example,
    example_brddata$matrix
  )

  # set the name of the layer
  names(brddata_example) <- name_l

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
get_example_frgcompdata <- function(type = NULL) {

  if (is.null(type)){
    # load the data
    example_frgcompdata <- seabORD::frgcompdata_example
    name_l <- "Forth.Islands"

  }else if (type == "dd"){

    # load the data
    example_frgcompdata <- seabORD::frgcompdata_example_dd
    name_l <- "Whinnyfold"
  }

  # make a raster with the right dimension/proj/extents etc
  frgcompdata_example <- raster_from_metadata(
    example_frgcompdata$metadata
  )

  # fill it with the data available
  frgcompdata_example <- raster::setValues(
    frgcompdata_example,
    example_frgcompdata$matrix
  )

  # set the name of the layer
  names(frgcompdata_example) <- name_l

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
get_example_fltdist_base <- function(type = NULL) {

  if (is.null(type)){
    # load the data
    example_site_bysea <- seabORD::UK9004171_bysea_3035
    name_l <- "UK9004171"

  }else if (type == "dd"){

    # load the data
    example_site_bysea <- seabORD::UK9002491_bysea_3035
    name_l <- "UK9002491"
  }

  # load the data

  # make a raster with the right dimension/proj/extents etc
  fltdist_base_example <- raster_from_metadata(
    example_site_bysea$metadata
  )

  # fill it with the data available
  fltdist_base_example <- raster::setValues(
    fltdist_base_example,
    example_site_bysea$matrix
  )

  # set the name of the layer
  names(fltdist_base_example) <- name_l

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
get_example_DBS_map <- function(type = NULL){

  if (is.null(type)){

  example_data_DBS_map <- seabORD::DBS_map_example
  name_l <- "DBS_map"

  }else if (type == "ords"){

  example_data_DBS_map <- seabORD::DBS_withORDs_example
  name_l <- "DBS_withORDs"

  }

  # make a raster with the right dimension/proj/extents etc
  DBS_map_example <- raster_from_metadata(
    example_data_DBS_map$metadata
  )

  # fill it with the data available
  DBS_map_example <- raster::setValues(
    DBS_map_example,
    example_data_DBS_map$matrix
  )

  # set the name of the layer
  names(DBS_map_example) <- name_l

  return(DBS_map_example)

}

#' Load example model outputs
#'
#' Retrieves example seabORD model outputs supplied with the package.
#' Different example outputs can be loaded by specifying the `type`
#' argument.
#'
#' Currently supported output types are:
#' \itemize{
#'   \item \code{"cal_dd"}: Output from the density-dependence
#'   calibration example.
#' }
#'
#' @param type Character string specifying which example output to load.
#' See Details for available options.
#'
#' @return An example seabORD output object.
#'
#' @examples
#' # Load calibration output from the density-dependence example
#' output <- get_example_output("cal_dd")
#'
#' @export
get_example_output <- function(type = NULL) {
  if (is.null(type)) {
    seabORD::example_scenario_output
  } else if (type == "cal_dd") {
    seabORD::example_calibration_output_dd
  } else if (type == "cal"){
    seabORD::example_calibration_output
  }
}



