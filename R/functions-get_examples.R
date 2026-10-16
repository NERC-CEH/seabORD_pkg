
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

#' Plot seamask raster
#'
#' @param seamask A `RasterLayer` containing seamask data,
#'   such as the object returned by [get_example_seamask()].
#' @param title Plot title.
#' @param caption Plot caption.
#'
#' @return A ggplot object.
#' @export
plot_seamask <- function(
    seamask,
    title = "coasts (red) and seamask (blue)",
    caption = "Example 1: data seamask"
) {

  ### UK outline (use coastline - available in the package)
  uk_map <- seabORD::cef_coast_4326 %>%
    dplyr::filter(ID_0 %in%  c("GBR","FRO", "GGY", "IMN"))

  uk_map_repr <- sf::st_transform(
    uk_map,
    crs = raster::crs(seamask)@projargs
  )


  seamask_df <- raster::as.data.frame(seamask, xy = TRUE)

  layer_name <- names(seamask)[1]

  seamask_df[[layer_name]] <- factor(
    seamask_df[[layer_name]],
    levels = c(0, 1)
  )

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = seamask_df,
      ggplot2::aes_string(
        x = "x",
        y = "y",
        fill = layer_name
      )
    ) +
    ggplot2::geom_sf(
      data = uk_map_repr,
      fill = NA,
      color = "#D63333",
      linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(
      values = c("0" = "grey80",
                 "1" = "#0483A4")
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title,
      caption = caption
    )
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

#' Plot a BrdData raster
#'
#' Creates a map of seabird density data using a `BrdData` raster and
#' overlays the UK coastline for geographical context.
#'
#' @param brddata A `RasterLayer` containing seabird density data, such as
#'   the object returned by [get_example_brddata()].
#'
#' @param title Character string giving the plot title.
#'   Defaults to `"coasts (red) and BrdData"`.
#'
#' @param caption Character string giving the plot caption.
#'   Defaults to `"Example 1: data BrdData"`.
#'
#' @return
#' A `ggplot` object showing the bird density raster overlaid with the UK
#' coastline. Raster values equal to zero are displayed as missing values.
#'
#' @details
#' The coastline dataset (`cef_coast_4326`) is reprojected to match the
#' coordinate reference system of the input raster before plotting.
#' Non-zero raster values are displayed using a viridis colour scale,
#' while missing values are shown in grey.
#'
#' @examples
#' # Load example bird density data
#' brddata <- get_example_brddata()
#'
#' # Create a map
#' p <- plot_brddata(brddata)
#'
#' # Display the plot
#' p
#'
#' @seealso
#' [get_example_brddata()] for loading the example bird density raster,
#' [theme_seabord()] for the plotting theme used by seabORD.
#'
#' @export
plot_brddata <- function(
    brddata,
    title = "coasts (red) and BrdData",
    caption = "Example 1: data BrdData"
) {

  ### UK outline (use coastline - available in the package)
  uk_map <- seabORD::cef_coast_4326%>%
    dplyr::filter(ID_0 %in%  c("GBR","FRO", "GGY", "IMN"))

  uk_map_repr <- sf::st_transform(
    uk_map,
    crs = raster::crs(brddata)@projargs
  )

  BrdData_df <- raster::as.data.frame(brddata, xy = TRUE)

  layer_name <- names(brddata)[1]

  BrdData_df[[layer_name]] <-
    dplyr::na_if(
      BrdData_df[[layer_name]],
      0
    )

  ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = BrdData_df,
                          ggplot2::aes_string(
                            x = "x",
                            y = "y",
                            fill = layer_name
                          )) +
    # Add the UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth = 1.5) +
    # Customize the plot
    seabORD::theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title =  title,
                  caption = caption)
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


#' Plot a competition distribution raster
#'
#' Creates a map of the competition distribution used by seabORD to
#' represent the influence of conspecifics from surrounding colonies on
#' foraging behaviour and intake rates.
#'
#' @param frgcompdata A `RasterLayer` containing competition distribution
#'   data, such as the object returned by
#'   [get_example_frgcompdata()].
#'
#' @param title Character string giving the plot title.
#'   Defaults to `"coasts (red) and FrgCompData"`.
#'
#' @param caption Character string giving the plot caption.
#'   Defaults to `"Example 1: data FrgCompData"`.
#'
#' @return
#' A `ggplot` object showing the competition distribution raster together
#' with coastlines for geographical context.
#'
#' @details
#' The coastline dataset (`cef_coast_4326`) is reprojected to match the
#' coordinate reference system of the input raster before plotting.
#'
#' Competition distribution values are displayed using a viridis colour
#' scale, while cells with a value of zero are displayed as missing values
#' in grey.
#'
#' @examples
#' # Load example competition distribution data
#' frgcompdata <- get_example_frgcompdata()
#'
#' # Create and display the map
#' plot_frgcompdata(frgcompdata)
#'
#' @seealso
#' [get_example_frgcompdata()] for loading the example competition
#' distribution raster.
#'
#' @export
plot_frgcompdata <- function(
    frgcompdata,
    title = "coasts (red) and FrgCompData",
    caption = "Example 1: data FrgCompData"
) {

  ### UK outline (use coastline - available in the package)
  uk_map <- seabORD::cef_coast_4326%>%
    dplyr::filter(ID_0 %in%  c("GBR","FRO", "GGY", "IMN"))

  uk_map_repr <- sf::st_transform(
    uk_map,
    crs = raster::crs(frgcompdata)@projargs
  )

  FrgCompData_df <- raster::as.data.frame(frgcompdata, xy = TRUE)

  layer_name <- names(frgcompdata)[1]

  FrgCompData_df[[layer_name]] <-
    dplyr::na_if(
      FrgCompData_df[[layer_name]],
      0
    )

  ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = FrgCompData_df,  ggplot2::aes_string(
      x = "x",
      y = "y",
      fill = layer_name
    )) +
    # Add the reprojected UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth  = 1.5) +
    # Customize the plot
    seabORD::theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title = title,
                  caption = caption)
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



#' Plot a flight distance raster
#'
#' Creates a map of flight distances from a SPA to all accessible
#' foraging locations and overlays coastlines for geographical context.
#'
#' @param fltdist_base A `RasterLayer` containing flight distance data,
#'   such as the object returned by [get_example_fltdist_base()].
#'
#' @param title Character string giving the plot title.
#'   Defaults to `"coasts (red) and fltdist_base"`.
#'
#' @param caption Character string giving the plot caption.
#'   Defaults to `"Example 1: data fltdist_base"`.
#'
#' @return
#' A `ggplot` object showing the flight distance raster together with
#' coastlines for geographical context.
#'
#' @details
#' The input raster represents the distance at sea from a breeding colony
#' or SPA to each raster cell.
#'
#' The coastline dataset (`cef_coast_4326`) is reprojected to match the
#' coordinate reference system of the input raster before plotting.
#'
#' Flight distance values are displayed using a viridis colour scale,
#' while cells with a value of zero are displayed as missing values in
#' grey.
#'
#' @examples
#' # Load example flight distance data
#' fltdist_base <- get_example_fltdist_base()
#'
#' # Create and display the map
#' plot_fltdist_base(fltdist_base)
#'
#' @seealso
#' [get_example_fltdist_base()] for loading the example flight distance
#' raster.
#'
#' @export
plot_fltdist_base <- function(
    fltdist_base,
    title = "coasts (red) and fltdist_base",
    caption = "Example 1: data fltdist_base"
) {

  ### UK outline (use coastline - available in the package)
  uk_map <- seabORD::cef_coast_4326%>%
    dplyr::filter(ID_0 %in%  c("GBR","FRO", "GGY", "IMN"))

  uk_map_repr <- sf::st_transform(
    uk_map,
    crs = raster::crs(fltdist_base)@projargs
  )

  fltdist_base_df <- raster::as.data.frame(fltdist_base, xy = TRUE)

  layer_name <- names(fltdist_base)[1]

  fltdist_base_df[[layer_name]] <-
    dplyr::na_if(
      fltdist_base_df[[layer_name]],
      0
    )

  ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = fltdist_base_df,  ggplot2::aes_string(
      x = "x",
      y = "y",
      fill = layer_name
    )) +
    # Add the reprojected UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth  = 1.5) +
    # Customize the plot
    seabORD::theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title = title,
                  caption = caption)
}



#' Plot an ORD polygon
#'
#' Creates a map of an ORD polygon and overlays coastlines for
#' geographical context.
#'
#' @param ORDpoly An `sf` polygon object containing the ORD area to be
#'   plotted.
#'
#' @param title Character string giving the plot title.
#'   Defaults to `"coasts (red) and ORDpoly (blue)"`.
#'
#' @param caption Character string giving the plot caption.
#'   Defaults to `"Example 1: data ORDpoly"`.
#'
#' @return
#' A `ggplot` object showing the ORD polygon together with coastlines.
#'
#' @details
#' The input polygon is reprojected to match the coordinate reference
#' system of the built-in coastline dataset (`cef_coast_4326`) before
#' plotting.
#'
#' The coastline is displayed in red and the ORD polygon is displayed in
#' blue.
#'
#' @examples
#' # Load example ORD polygon
#' ORDpoly <- seabORD::ORDpoly_example
#'
#' # Create and display the map
#' plot_ORDpoly(ORDpoly)
#'
#' @seealso
#' [ORDpoly_example] for the example ORD polygons.
#'
#' @export
plot_ORDpoly <- function(
    ORDpoly,
    title = "coasts (red) and ORDpoly (blue)",
    caption = "Example 1: data ORDpoly"
) {

  # UK outline
  uk_map <- seabORD::cef_coast_4326 %>%
    dplyr::filter(ID_0 %in% c("GBR", "FRO", "GGY", "IMN"))

  #proj and box
  ORDpoly_3035 <- sf::st_transform(ORDpoly, 3035)

  bbox <- sf::st_bbox(
    sf::st_buffer(ORDpoly_3035, dist = 50000)
  )

  bbox_sf <- sf::st_as_sfc(bbox) %>%
    sf::st_transform(sf::st_crs(uk_map))

  bbox <- sf::st_bbox(bbox_sf)

  ORDpoly_repr <- sf::st_transform(
    ORDpoly,
    sf::st_crs(uk_map)
  )

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = uk_map,
      fill = NA,
      color = "#D63333",
      linewidth = 1.5
    ) +
    ggplot2::geom_sf(
      data = ORDpoly_repr,
      fill = "#0483A4",
      color = "#0483A4",
      linewidth = 1
    ) +
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title,
      caption = caption
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
#' colony <- create_colony_point(
#'   lon = -1.85849,
#'   lat = 57.388807
#' )
#'
#' colony
#'
#' @export
create_colony_point <- function(lon, lat) {

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


#' Plot a colony location against the coastline
#'
#' Creates a zoomed-in map showing a colony location relative to the
#' coastline, allowing the user to visually confirm that the colony is
#' located on land and adjacent to the sea.
#'
#' @param colony An `sf` point object containing the colony location in
#'   EPSG:3035.
#' @param expand_km Distance (km) to extend the plotting window around the
#'   colony. Defaults to 1.
#'
#' @return
#' A `ggplot` object.
#'
#' @details
#' The plot is centred on the colony location and displays a square window
#' extending `expand_km` kilometres in all directions. This is useful for
#' checking that colony coordinates have been specified correctly before
#' generating distance-at-sea layers or running seabORD simulations.
#'
#' @examples
#' colony <- create_colony_point(
#'   lon = -1.85849,
#'   lat = 57.388807
#' )
#'
#' plot_colony_location(
#'   colony = colony
#' )
#'
#' @export
plot_colony_location <- function(
    colony,
    expand_km = 1
) {

  coastline <-  seabORD::uk_coast_3035

  coords <- sf::st_coordinates(colony)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = coastline,
      fill = "grey",
      colour = "black"
    ) +
    ggplot2::geom_sf(
      data = colony,
      colour = "blue",
      pch = 4,
      size = 4
    ) +
    ggplot2::coord_sf(
      xlim = c(
        coords[, 1] - expand_km * 1000,
        coords[, 1] + expand_km * 1000
      ),
      ylim = c(
        coords[, 2] - expand_km * 1000,
        coords[, 2] + expand_km * 1000
      ),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(
      location = "tr",
      width_hint = 0.5
    ) +
    ggplot2::labs(
      title = "Colony location and surrounding coastline",
      caption = "Blue cross: colony location"
    ) +
    seabORD::theme_seabord()

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


#' Plot a Distance By Sea (DBS) raster
#'
#' Creates a map of Distance By Sea (DBS) values and overlays coastlines
#' for geographical context.
#'
#' @param DBS_map A `RasterLayer` containing Distance By Sea (DBS) data,
#'   such as the object returned by [get_example_DBS_map()].
#'
#' @param title Character string giving the plot title.
#'   Defaults to `"coasts (red) and DBS_map"`.
#'
#' @param caption Character string giving the plot caption.
#'   Defaults to `"Example 1: Distance By Sea (DBS) map"`.
#'
#' @return
#' A `ggplot` object showing the Distance By Sea raster together with
#' coastlines for geographical context.
#'
#' @details
#' The DBS map represents the distance travelled over sea between
#' locations rather than straight-line (Euclidean) distance.
#'
#' The coastline dataset (`cef_coast_4326`) is reprojected to match the
#' coordinate reference system of the input raster before plotting.
#'
#' DBS values are displayed using a viridis colour scale, while cells
#' with a value of zero are displayed as missing values in grey.
#'
#' @examples
#' # Load example DBS data
#' DBS_map <- get_example_DBS_map()
#'
#' # Create and display the map
#' plot_DBS_map(DBS_map)
#'
#' @seealso
#' [get_example_DBS_map()] for loading the example DBS raster.
#'
#' @export
plot_DBS_map <- function(
    DBS_map,
    title = "coasts (red) and DBS_map",
    caption = "Example 1: Distance By Sea (DBS) map"
) {

  # UK outline (use coastline available in the package)
  uk_map <- seabORD::cef_coast_4326 %>%
    dplyr::filter(ID_0 %in% c("GBR", "FRO", "GGY", "IMN"))

  uk_map_repr <- sf::st_transform(
    uk_map,
    crs = raster::crs(DBS_map)@projargs
  )

  DBS_map_df <- raster::as.data.frame(DBS_map, xy = TRUE)

  layer_name <- names(DBS_map)[1]

  DBS_map_df[[layer_name]] <-
    dplyr::na_if(
      DBS_map_df[[layer_name]],
      0
    )

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = DBS_map_df,
      ggplot2::aes_string(
        x = "x",
        y = "y",
        fill = layer_name
      )
    ) +
    ggplot2::geom_sf(
      data = uk_map_repr,
      fill = NA,
      color = "#D63333",
      linewidth = 1.5
    ) +
    seabORD::theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",
      direction = -1,
      name = "Distance by sea"
    ) +
    ggplot2::labs(
      title = title,
      caption = caption
    )
}


#' Plot a DD map raster
#'
#' Creates a map of DD values and optionally crops the map around a
#' colony location for closer inspection.
#'
#' @param DD_map A `RasterLayer` containing DD map data.
#'
#' @param colony An `sf` point object giving the colony location.
#'   If supplied together with the cropping distances, the raster
#'   is cropped around the colony before plotting.
#'
#' @param km_to_left Distance (km) to extend the cropped map westward.
#'   Defaults to 30.
#'
#' @param km_to_right Distance (km) to extend the cropped map eastward.
#'   Defaults to 50.
#'
#' @param km_up Distance (km) to extend the cropped map northward.
#'   Defaults to 30.
#'
#' @param km_down Distance (km) to extend the cropped map southward.
#'   Defaults to 70.
#'
#' @param crop Logical. Should the raster be cropped around the colony
#'   location? Defaults to `FALSE`.
#'
#' @param title Character string giving the plot title.
#'
#' @param caption Character string giving the plot caption.
#'
#' @return
#' A `ggplot` object showing the DD map raster.
#'
#' @examples
#' DD_map <- get_example_DD_map()
#'
#' plot_DD_map(DD_map)
#'
#' @export
plot_DD_map <- function(
    DD_map,
    colony = NULL,
    km_to_left = 30,
    km_to_right = 50,
    km_up = 30,
    km_down = 70,
    crop = FALSE,
    title = "DD map",
    caption = NULL
) {

  if (crop) {

    if (is.null(colony)) {
      stop("A colony location must be supplied when crop = TRUE.")
    }

    crop_extent <- seabORD::create_crop_extent_colony(
      colony,
      km_to_left,
      km_to_right,
      km_up,
      km_down
    )

    DD_map <- raster::crop(
      DD_map,
      crop_extent
    )
  }

  DD_map_df <- raster::as.data.frame(
    DD_map,
    xy = TRUE
  )

  layer_name <- names(DD_map)[1]

  ggplot2::ggplot(
    DD_map_df,
    ggplot2::aes(
      x = x,
      y = y,
      fill = .data[[layer_name]]
    )
  ) +
    ggplot2::geom_raster() +
    seabORD::theme_seabord() +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_viridis_c(
      name = layer_name
    ) +
    ggplot2::labs(
      title = title,
      caption = caption
    )
}


#' Plot a DD map with ORD footprints and buffer zones
#'
#' Creates a map showing the DD raster together with offshore wind farm
#' footprints (ORD polygons), user-defined buffer zones and coastlines.
#'
#' @param DD_map A `RasterLayer` containing DD values.
#'
#' @param ORDpolys An `sf` polygon object defining ORD footprints.
#'
#' @param colony An `sf` point object giving the colony location used to
#'   define the plotting extent.
#'
#' @param bufferwidth Width of the ORD buffer in kilometres.
#'   Defaults to `2`.
#'
#' @param km_to_left Distance (km) to extend the plotting area westward.
#'   Defaults to `30`.
#'
#' @param km_to_right Distance (km) to extend the plotting area eastward.
#'   Defaults to `50`.
#'
#' @param km_up Distance (km) to extend the plotting area northward.
#'   Defaults to `30`.
#'
#' @param km_down Distance (km) to extend the plotting area southward.
#'   Defaults to `70`.
#'
#' @param title Character string giving the plot title.
#'
#' @return
#' A `ggplot` object showing the DD raster, ORD footprints, buffer zones
#' and coastline.
#'
#' @details
#' The raster is cropped around the colony location before plotting to
#' improve rendering speed and focus the visualisation on the area of
#' interest.
#'
#' A buffer is generated around each ORD polygon and displayed alongside
#' the original footprint. Coastlines are cropped to the same spatial
#' extent to further reduce plotting time.
#'
#' @examples
#' \dontrun{
#' plot_DD_map_ORD(
#'   DD_map = DD_map,
#'   ORDpolys = ORDpolys,
#'   colony = colonyLAEA,
#'   UK_coastline = UK_coastline_LAEA
#' )
#' }
#'
#' @export
plot_DD_map_with_ORDs <- function(
    DD_map,
    ORDpolys,
    colony,
    bufferwidth = 2,
    km_to_left = 30,
    km_to_right = 50,
    km_up = 30,
    km_down = 70,
    title = "DD map and ORD footprints"
) {

  UK_coastline <- seabORD::cef_coast_3035 %>%
    dplyr::filter(
      COUNTRY == "United Kingdom"
    )

  crop_extent <- raster::extent(
    sf::st_bbox(colony)$xmin - (km_to_left * 1000),
    sf::st_bbox(colony)$xmin + (km_to_right * 1000),
    sf::st_bbox(colony)$ymin - (km_down * 1000),
    sf::st_bbox(colony)$ymin + (km_up * 1000)
  )

  DD_map_cropped <- raster::crop(
    DD_map,
    crop_extent
  )

  DD_map_df <- raster::as.data.frame(
    DD_map_cropped,
    xy = TRUE
  )

  colnames(DD_map_df) <- c(
    "x",
    "y",
    "density"
  )

  ORDpolys_buffer <- seabORD::create_ORD_buffer(
    ORDpolys,
    bufferwidth = bufferwidth
  )

  UK_coastline_cropped <- sf::st_crop(
    UK_coastline,
    crop_extent
  )

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = DD_map_df,
      ggplot2::aes(
        x = x,
        y = y,
        fill = density
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "B",
      direction = -1,
      na.value = "transparent",
      name = "DD value"
    ) +
    ggplot2::geom_sf(
      data = ORDpolys,
      colour = "blue",
      fill = NA
    ) +
    ggplot2::geom_sf(
      data = ORDpolys_buffer,
      colour = "lightblue",
      fill = NA
    ) +
    ggplot2::geom_sf(
      data = UK_coastline_cropped,
      fill = "grey",
      colour = "black"
    ) +
    ggplot2::coord_sf(
      xlim = c(crop_extent[1], crop_extent[2]),
      ylim = c(crop_extent[3], crop_extent[4]),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(
      location = "tr",
      width_hint = 0.5
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title
    )
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



#' Create buffer polygons around ORDs
#'
#' @param ORDpolys An `sf` polygon or multipolygon object containing
#'   ORD footprints.
#' @param bufferwidth Buffer width in kilometres.
#'
#' @return An `sf` object containing buffered ORD polygons.
#'
#' @export
create_ORD_buffer <- function(
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
#' ORDpolys_with_buffer <- create_ORD_buffer(
#'   ORDpolys,
#'   bufferwidth = 2
#' )
#'
#' calculate_pc_in_ORD_buffer(
#'   DD_map,
#'   ORDpolys_with_buffer
#' )
#' }
#'
#' @export
calculate_pc_in_ORD_buffer <- function(
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

#' Plot areas affected by ORD obstruction
#'
#' Creates a map highlighting locations where the presence of Offshore
#' Renewable Developments (ORDs) increases Distance By Sea (DBS)
#' relative to an unobstructed scenario.
#'
#' @param DBS_map A `RasterLayer` containing Distance By Sea values
#'   calculated without ORDs.
#'
#' @param DBS_withORDs A `RasterLayer` containing Distance By Sea values
#'   calculated with ORDs present.
#'
#' @param colony An `sf` point object representing the colony location
#'   used to define the plotting extent.
#'
#' @param smalltol Numeric threshold used to identify meaningful
#'   increases in Distance By Sea. Cells where
#'   `DBS_withORDs - DBS_map > smalltol` are classified as obstructed.
#'   Defaults to `0.01`.
#'
#' @param km_to_left Distance (km) to extend the plotting area westward.
#'   Defaults to `30`.
#'
#' @param km_to_right Distance (km) to extend the plotting area eastward.
#'   Defaults to `50`.
#'
#' @param km_up Distance (km) to extend the plotting area northward.
#'   Defaults to `30`.
#'
#' @param km_down Distance (km) to extend the plotting area southward.
#'   Defaults to `70`.
#'
#' @return
#' A `ggplot` object showing the area affected by ORD obstruction.
#' Cells shown in red indicate where Distance By Sea increases by more
#' than `smalltol` due to the presence of ORDs.
#'
#' @details
#' The function compares Distance By Sea surfaces calculated with and
#' without Offshore Renewable Developments. Areas identified as
#' obstructed represent locations where travel routes are altered by the
#' presence of ORDs, resulting in longer sea-travel distances.
#'
#' The plot is cropped around the supplied colony location using
#' [create_crop_extent_colony()] to focus on the area of interest and
#' improve plotting performance.
#'
#' @examples
#' \dontrun{
#' DBS_map <- get_example_DBS_map()
#' DBS_withORDs <- get_example_DBS_map_with_ORDs()
#' colony <- get_example_colony_3035()
#'
#' plot_ORD_obstruction(
#'   DBS_map = DBS_map,
#'   DBS_withORDs = DBS_withORDs,
#'   colony = colony
#' )
#' }
#'
#' @seealso
#' [get_example_DBS_map()],
#' [get_example_DBS_map_with_ORDs()],
#' [create_crop_extent_colony()]
#'
#' @export
plot_ORD_obstruction <- function(
    DBS_map,
    DBS_withORDs,
    colony,
    smalltol = 0.01,
    km_to_left = 30,
    km_to_right = 50,
    km_up = 30,
    km_down = 70
) {

  crop_extent <- seabORD::create_crop_extent_colony(
    colony,
    km_to_left,
    km_to_right,
    km_up,
    km_down
  )

  obstructed <- (DBS_withORDs - DBS_map) > smalltol

  obstructed <- raster::crop(
    obstructed,
    crop_extent
  )

  obstructed_df <- raster::as.data.frame(
    obstructed,
    xy = TRUE
  )

  names(obstructed_df) <- c(
    "x",
    "y",
    "obstructed"
  )

  obstructed_df$obstructed <- dplyr::case_when(
    is.na(obstructed_df$obstructed) ~ "Land",
    obstructed_df$obstructed ~ "Obstructed sea",
    TRUE ~ "Accessible sea"
  )

  ggplot2::ggplot(
    obstructed_df,
    ggplot2::aes(
      x = x,
      y = y,
      fill = obstructed
    )
  ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(
      values = c(
        "Land" = "grey80",
        "Accessible sea" = "white",
        "Obstructed sea" = "red"
      ),
      breaks = c("Accessible sea", "Obstructed sea")
    )+
    seabORD::theme_seabord() +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Areas affected by ORD obstruction"
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
    stop(
      paste0(
        "Input mismatch: modPar$Nreplicates = ",
        n_reps,
        " but length(Par$Pmedian) = ",
        n_prey,
        ". These values should be identical for calibration runs."
      ),
      call. = FALSE
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
