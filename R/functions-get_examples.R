#' Create a raster layer from stored metadata
#'
#' @description
#' Creates an empty [raster::RasterLayer-class] object from a list containing
#' the raster dimensions, extent, and coordinate reference system.
#'
#' @param metadata A named list containing the raster metadata. It must contain
#'   the following elements:
#'   \itemize{
#'     \item `n_rows`: Number of raster rows.
#'     \item `n_cols`: Number of raster columns.
#'     \item `x_min`: Minimum x-coordinate of the raster extent.
#'     \item `x_max`: Maximum x-coordinate of the raster extent.
#'     \item `y_min`: Minimum y-coordinate of the raster extent.
#'     \item `y_max`: Maximum y-coordinate of the raster extent.
#'     \item `crs`: Coordinate reference system supplied in a format accepted
#'       by [raster::raster()].
#'   }
#'
#' @return
#' An empty [raster::RasterLayer-class] object with the dimensions, extent,
#' and coordinate reference system specified in `metadata`.
#'
#' @details
#' This internal helper is used to reconstruct example rasters stored within
#' the package as separate metadata and cell-value objects.
#'
#' The function checks that all required metadata fields are present and that
#' the raster dimensions and extent coordinates are non-missing numeric
#' scalars.
#'
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
#' @description
#' Creates a rectangular raster extent around a colony location using
#' user-specified distances in each cardinal direction.
#'
#' @param colony An `sf` object representing a colony location. The coordinate
#'   reference system must use metres as its coordinate units.
#' @param km_to_left Numeric scalar giving the distance, in kilometres, to
#'   extend the crop extent west of the colony. Defaults to `30`.
#' @param km_to_right Numeric scalar giving the distance, in kilometres, to
#'   extend the crop extent east of the colony. Defaults to `50`.
#' @param km_up Numeric scalar giving the distance, in kilometres, to extend
#'   the crop extent north of the colony. Defaults to `30`.
#' @param km_down Numeric scalar giving the distance, in kilometres, to extend
#'   the crop extent south of the colony. Defaults to `70`.
#'
#' @return
#' A [raster::Extent-class] object defining the crop area around the colony.
#'
#' @details
#' Distances are converted from kilometres to metres before being added to or
#' subtracted from the colony bounding-box coordinates. The input colony must
#' therefore use a projected coordinate reference system with coordinates
#' measured in metres.
#'
#' @keywords internal
#' @noRd
set_crop_extent_colony <- function(
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

#' Load example biological and simulation parameters
#'
#' @description
#' Retrieves an example `Par` list containing the biological and simulation
#' parameters required to run a seabORD analysis.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard scenario example.
#'     \item `"dd"`: Density-dependence scenario example.
#'     \item `"cal"`: Standard calibration example.
#'     \item `"cal_dd"`: Density-dependence calibration example.
#'   }
#'
#' @return
#' A named list containing example biological and simulation parameters for
#' use with [seabord()].
#'
#' @details
#' The returned `Par` object contains parameters such as colony sizes, species,
#' displacement and barrier probabilities, prey values, and scaling settings.
#'
#' See the package input vignette for a description of the input objects
#' required by [seabord()]:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load parameters for the standard scenario example
#' par <- get_example_par()
#'
#' # Load parameters for the density-dependence example
#' par_dd <- get_example_par("dd")
#'
#' # Inspect the available parameters
#' names(par)
#'
#' @seealso
#' [get_example_modpar()], [get_example_ordPar()],
#' [get_example_switches()], and [seabord()].
#'
#' @family example data
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

#' Load example model-control parameters
#'
#' @description
#' Retrieves an example `modPar` list containing model-control, replication,
#' computing-environment, and output settings for a seabORD analysis.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard scenario example.
#'     \item `"dd"`: Density-dependence scenario example.
#'     \item `"cal"`: Standard calibration example.
#'     \item `"cal_dd"`: Density-dependence calibration example.
#'   }
#'
#' @return
#' A named list containing example model-control parameters for use with
#' [seabord()].
#'
#' @details
#' The returned `modPar` object contains settings such as the number of
#' simulation replicates, random-number seed, run reference, output directory,
#' and parallel-processing configuration.
#'
#' See the package input vignette for a description of the input objects
#' required by [seabord()]:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load model parameters for the standard scenario example
#' modPar <- get_example_modpar()
#'
#' # Load model parameters for the calibration example
#' modPar_cal <- get_example_modpar("cal")
#'
#' # Inspect the available settings
#' names(modPar)
#'
#' @seealso
#' [get_example_par()], [get_example_ordPar()],
#' [get_example_switches()], and [seabord()].
#'
#' @family example data
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

#' Load example offshore renewable development parameters
#'
#' @description
#' Retrieves an example `ordPar` list containing parameters describing the
#' offshore renewable development scenario used in a seabORD analysis.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard scenario example.
#'     \item `"dd"`: Density-dependence scenario example.
#'     \item `"cal"`: Standard calibration example.
#'     \item `"cal_dd"`: Density-dependence calibration example.
#'   }
#'
#' @return
#' A named list containing example offshore renewable development parameters
#' for use with [seabord()].
#'
#' @details
#' The returned `ordPar` object contains settings describing the offshore
#' renewable developments included in the model, including footprint and
#' buffer-zone parameters where applicable.
#'
#' See the package input vignette for a description of the input objects
#' required by [seabord()]:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load ORD parameters for the standard scenario example
#' ordPar <- get_example_ordPar()
#'
#' # Load ORD parameters for the density-dependence example
#' ordPar_dd <- get_example_ordPar("dd")
#'
#' # Inspect the available parameters
#' names(ordPar)
#'
#' @seealso
#' [get_example_par()], [get_example_modpar()],
#' [get_example_switches()], [get_example_ORDpoly()], and [seabord()].
#'
#' @family example data
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

#' Load example model switches
#'
#' @description
#' Retrieves an example `switches` list containing settings that control
#' optional model behaviour, execution modes, and output generation in a
#' seabORD analysis.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard scenario example.
#'     \item `"dd"`: Density-dependence scenario example.
#'     \item `"cal"`: Standard calibration example.
#'     \item `"cal_dd"`: Density-dependence calibration example.
#'   }
#'
#' @return
#' A named list containing example model switches for use with [seabord()].
#'
#' @details
#' The returned object contains switches controlling features such as the
#' model mode, execution environment, output detail, file creation, and
#' debugging behaviour.
#'
#' See the package input vignette for a description of the input objects
#' required by [seabord()]:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load switches for the standard scenario example
#' switches <- get_example_switches()
#'
#' # Load switches for the calibration example
#' switches_cal <- get_example_switches("cal")
#'
#' # Inspect the available switches
#' names(switches)
#'
#' @seealso
#' [get_example_par()], [get_example_modpar()],
#' [get_example_ordPar()], and [seabord()].
#'
#' @family example data
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

#' Load the example flight-grid correction object
#'
#' @description
#' Retrieves the example flight-grid correction transition object supplied
#' with the package.
#'
#' @return
#' A `TransitionLayer` containing the example flight-grid correction values in
#' the ETRS89-extended / LAEA Europe coordinate reference system
#' (EPSG:3035).
#'
#' @details
#' The flight-grid correction object accounts for the additional distance
#' associated with movement across the raster grid compared with direct
#' straight-line movement. It is used when calculating distance-by-sea routes
#' for seabORD simulations.
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the example flight-grid correction object
#' fgc <- get_example_fgc_3035()
#'
#' # Inspect the object
#' fgc
#'
#' @seealso
#' [get_example_fltdist_base()], [get_example_DBS_map()], and [seabord()].
#'
#' @family example data
#' @export
get_example_fgc_3035 <- function(){
  seabORD::FlightGridcorrection_3035
}

#' Load example offshore renewable development polygons
#'
#' @description
#' Retrieves example offshore renewable development polygons supplied with
#' the package.
#'
#' @param type Character string specifying the example polygon dataset to
#'   load. Available options are:
#'   \itemize{
#'     \item `NULL`: Polygons for the standard scenario example.
#'     \item `"dd"`: Polygons for the Whinnyfold density-dependence example.
#'   }
#'
#' @return
#' An `sf` object containing example offshore renewable development
#' footprints.
#'
#' @details
#' The returned polygons represent the offshore renewable development
#' footprints used by the selected example workflow. They can be supplied as
#' the `ORDpoly` argument to [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load polygons for the standard scenario example
#' ORDpoly <- get_example_ORDpoly()
#'
#' # Load polygons for the density-dependence example
#' ORDpoly_dd <- get_example_ORDpoly("dd")
#'
#' # Plot the polygon geometry
#' plot(sf::st_geometry(ORDpoly))
#'
#' @seealso
#' [get_example_ordPar()] and [seabord()].
#'
#' @family example data
#' @export
get_example_ORDpoly <- function(type = NULL) {
  if (is.null(type)){
    seabORD::ORDpoly_example
  }else if (type == "dd"){
    seabORD::ORDpoly_example_wfold
  }
}

#' Load the example sea-mask raster
#'
#' @description
#' Reconstructs and returns the example sea-mask raster supplied with the
#' package.
#'
#' @return
#' A [raster::RasterLayer-class] object containing the example sea mask. The
#' returned raster layer is named `"seamask_3035"`.
#'
#' @details
#' The example sea mask is stored internally as a list containing:
#' \itemize{
#'   \item `metadata`: Raster dimensions, extent, and coordinate reference
#'     system.
#'   \item `matrix`: Raster cell values.
#' }
#'
#' The raster is reconstructed from the stored metadata using the package's
#' internal raster-construction helper and is then populated with the stored
#' cell values.
#'
#' The returned raster can be supplied as the `seamask` argument to
#' [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the example sea mask
#' seamask <- get_example_seamask()
#'
#' # Inspect the raster
#' seamask
#'
#' # Plot the raster
#' raster::plot(seamask)
#'
#' @seealso
#' [get_example_brddata()], [get_example_frgcompdata()],
#' [get_example_fltdist_base()], and [seabord()].
#'
#' @family example data
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

#' Load example SPA coordinate data
#'
#' @description
#' Retrieves the coordinates and associated information for the Special
#' Protection Area used by the selected example workflow.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard example using site code `"UK9004171"`.
#'     \item `"dd"`: Density-dependence example using site code `"UK9002491"`.
#'   }
#'
#' @return
#' A tibble containing the coordinate record for the SPA used by the selected
#' example workflow.
#'
#' @details
#' The function filters the package's built-in `spacoordinates` dataset to
#' return the SPA coordinate record associated with the selected workflow.
#'
#' The returned object can be supplied as the `spadat1` argument to
#' [seabord()].
#'
#' Corresponding SPA designation information can be retrieved with
#' [get_example_spadat2()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load coordinates for the standard example SPA
#' spadat1 <- get_example_spadat1()
#'
#' # Load coordinates for the density-dependence example SPA
#' spadat1_dd <- get_example_spadat1("dd")
#'
#' @seealso
#' [get_example_spadat2()] and [seabord()].
#'
#' @family example data
#' @export
get_example_spadat1 <- function(type = NULL) {

  if (is.null(type)){
    site_code <- "UK9004171"
  }else if (type == "dd"){
    site_code <- "UK9002491"
  }


  df <- tibble::as_tibble(
    dplyr::filter(
      seabORD::spacoordinates,
      SITECODE == site_code
    )
  )

  if (!is.null(type) && type == "dd"){
    # Use the exact colony location of Whinnyfold
    whinnyfold_lon <- -1.85849
    whinnyfold_lat <- 57.388807

    df$Longitude <- whinnyfold_lon
    df$Latitude  <- whinnyfold_lat
    df$flt.LONG  <- whinnyfold_lon
    df$flt.LAT   <- whinnyfold_lat
  }

  return(df)

}

#' Load example SPA designation data
#'
#' @description
#' Retrieves the designation and associated information for the Special
#' Protection Area used by the selected example workflow.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard example using site code `"UK9004171"`.
#'     \item `"dd"`: Density-dependence example using site code `"UK9002491"`.
#'   }
#'
#' @return
#' A tibble containing the SPA designation record for the selected example
#' workflow.
#'
#' @details
#' The function filters the package's built-in `spalist` dataset to return the
#' SPA designation record associated with the selected workflow.
#'
#' The returned object can be supplied as the `spadat2` argument to
#' [seabord()].
#'
#' Corresponding SPA coordinate information can be retrieved with
#' [get_example_spadat1()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load designation data for the standard example SPA
#' spadat2 <- get_example_spadat2()
#'
#' # Load designation data for the density-dependence example SPA
#' spadat2_dd <- get_example_spadat2("dd")
#'
#' @seealso
#' [get_example_spadat1()] and [seabord()].
#'
#' @family example data
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

#' Load example species parameters
#'
#' @description
#' Retrieves the species-specific energetics, demography, and prey parameters
#' used by the package's Kittiwake example.
#'
#' @return
#' A data frame containing the species-specific parameters for
#' Kittiwake, identified by species code `"KI"`.
#'
#' @details
#' This function calls `get_spdat("KI")` to retrieve the
#' Kittiwake parameters from the species-parameter data supplied with the
#' package.
#'
#' The returned object can be supplied as the `spdat` argument to [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the example Kittiwake species parameters
#' spdat <- get_example_spdat()
#'
#' # Inspect the available parameters
#' names(spdat)
#'
#' @seealso
#' [get_spdat()] and [seabord()].
#'
#' @family example data
#' @export

get_example_spdat <- function() {
  species_code <- "KI"
  get_spdat(species_code)
}


#' Load example seabird-distribution raster data
#'
#' @description
#' Reconstructs and returns the seabird-distribution raster used by the selected
#' example workflow.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Forth Islands standard example.
#'     \item `"dd"`: Whinnyfold density-dependence example.
#'   }
#'
#' @return
#' A [raster::RasterLayer-class] object containing example seabird-distribution
#' data with the appropriate dimensions, extent, coordinate reference system,
#' and layer name.
#'
#' @details
#' The raster is reconstructed from data stored internally as separate
#' metadata and cell-value objects.
#'
#' For the standard example, the returned layer is named `"Forth.Islands"`.
#' For the density-dependence example, it is named `"Whinnyfold"`.
#'
#' The returned raster can be supplied as the `BrdData` argument to
#' [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the standard example bird-density raster
#' BrdData <- get_example_brddata()
#'
#' # Load the density-dependence example
#' BrdData_dd <- get_example_brddata("dd")
#'
#' # Plot the raster
#' raster::plot(BrdData)
#'
#' @seealso
#' [get_example_seamask()], [get_example_frgcompdata()],
#' [get_example_fltdist_base()], and [seabord()].
#'
#' @family example
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


#' Load example foraging-competition raster data
#'
#' @description
#' Reconstructs and returns the foraging-competition raster used by the
#' selected example workflow.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Forth Islands standard example.
#'     \item `"dd"`: Whinnyfold density-dependence example.
#'   }
#'
#' @return
#' A [raster::RasterLayer-class] object containing example foraging-competition
#' data with the appropriate dimensions, extent, coordinate reference system,
#' and layer name.
#'
#' @details
#' The raster represents the distribution of birds from colonies that are not
#' simulated directly but may compete with the modelled population at
#' foraging locations.
#'
#' The raster is reconstructed from data stored internally as separate
#' metadata and cell-value objects.
#'
#' For the standard example, the returned layer is named `"Forth.Islands"`.
#' For the density-dependence example, it is named `"Whinnyfold"`.
#'
#' The returned raster can be supplied as the `FrgCompData` argument to
#' [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the standard example competition raster
#' FrgCompData <- get_example_frgcompdata()
#'
#' # Load the density-dependence example
#' FrgCompData_dd <- get_example_frgcompdata("dd")
#'
#' # Plot the raster
#' raster::plot(FrgCompData)
#'
#' @seealso
#' [get_example_brddata()], [get_example_seamask()],
#' [get_example_fltdist_base()], and [seabord()].
#'
#' @family example data
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

#' Load example baseline flight-distance raster data
#'
#' @description
#' Reconstructs and returns the baseline distance-by-sea raster used by the
#' selected example workflow.
#'
#' @param type Character string specifying the example workflow to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Standard example for SPA site `"UK9004171"`.
#'     \item `"dd"`: Density-dependence example for SPA site `"UK9002491"`.
#'   }
#'
#' @return
#' A [raster::RasterLayer-class] object containing baseline flight distances
#' from the selected SPA to the cells in the model domain.
#'
#' @details
#' The raster represents distance by sea from the example colony to each
#' reachable raster cell. These distances are used by seabORD to estimate
#' travel time and energetic costs associated with foraging trips.
#'
#' The raster is reconstructed from data stored internally as separate
#' metadata and cell-value objects.
#'
#' For the standard example, the returned layer is named `"UK9004171"`.
#' For the density-dependence example, it is named `"UK9002491"`.
#'
#' The returned raster can be supplied as the `fltdist_base` argument to
#' [seabord()].
#'
#' See the package input vignette for an example:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' # Load the standard example baseline flight-distance raster
#' fltdist_base <- get_example_fltdist_base()
#'
#' # Load the density-dependence example
#' fltdist_base_dd <- get_example_fltdist_base("dd")
#'
#' # Plot the raster
#' raster::plot(fltdist_base)
#'
#' @seealso
#' [get_example_DBS_map()], [get_example_fgc_3035()],
#' [get_example_seamask()], and [seabord()].
#'
#' @family example data
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


#' Load an example distance-by-sea raster
#'
#' @description
#' Reconstructs and returns an example distance-by-sea raster, either without
#' or with offshore renewable developments included.
#'
#' @param type Character string specifying the distance-by-sea raster to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Baseline distance-by-sea raster without offshore
#'       renewable developments.
#'     \item `"ords"`: Distance-by-sea raster with offshore renewable
#'       developments included.
#'   }
#'
#' @return
#' A [raster::RasterLayer-class] object containing example distance-by-sea
#' values.
#'
#' @details
#' The raster is reconstructed from data stored internally as separate
#' metadata and cell-value objects.
#'
#' When `type = NULL`, the returned layer is named `"DBS_map"`. When
#' `type = "ords"`, the returned layer is named `"DBS_withORDs"`.
#'
#' The baseline and ORD-inclusive rasters can be used to inspect how offshore
#' renewable development footprints alter available movement routes and
#' flight distances.
#'
#' See the package input vignette for related spatial inputs:
#' \code{vignette("C_KI_inputs", package = "seabORD")}.
#'
#' @examples
#' \dontrun{
#' # Load the baseline distance-by-sea raster
#' DBS_map <- get_example_DBS_map()
#'
#' # Load the distance-by-sea raster with ORDs
#' DBS_with_ORDs <- get_example_DBS_map("ords")
#'
#' # Plot the baseline raster
#' raster::plot(DBS_map)
#' }
#'
#' @seealso
#' [get_example_fltdist_base()], [get_example_fgc_3035()],
#' and [get_example_ORDpoly()].
#'
#' @family example data
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

#' Load example seabORD model output
#'
#' @description
#' Retrieves a precomputed seabORD model-output object for use in package
#' examples, vignettes, plotting functions, and testing workflows.
#'
#' @param type Character string specifying the example output to load.
#'   Available options are:
#'   \itemize{
#'     \item `NULL`: Output from the standard scenario example.
#'     \item `"cal"`: Output from the standard calibration example.
#'     \item `"cal_dd"`: Output from the density-dependence calibration
#'       example.
#'   }
#'
#' @return
#' A precomputed seabORD model-output object. Its contents depend on the
#' selected example and may include adult, chick, flight, survival, impact,
#' parameter, and spatial output components.
#'
#' @details
#' This function provides access to precomputed model outputs so that examples
#' and vignettes can demonstrate output processing and plotting without
#' rerunning the complete simulation.
#'
#' The structure of the returned object corresponds to the output produced by
#' [seabord()], although individual components depend on the model mode and
#' output switches used when the example was generated.
#'
#' See the package output vignette for guidance on examining and plotting
#' seabORD results:
#' \code{vignette("I_exampleKI_output", package = "seabORD")}.
#'
#' @examples
#' # Load the standard scenario output
#' output <- get_example_output()
#'
#' # Load the standard calibration output
#' calibration_output <- get_example_output("cal")
#'
#' # Load the density-dependence calibration output
#' calibration_output_dd <- get_example_output("cal_dd")
#'
#' # Inspect the top-level output components
#' names(output)
#'
#' @seealso
#' [seabord()] and [get_example_par()].
#'
#' @family example data
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



