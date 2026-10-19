#' seabORD ggplot theme
#'
#' Registers the seabORD fonts (if necessary) and returns the
#' package ggplot2 theme.
#'
#' @return A ggplot2 theme object.
#' @export
theme_seabord <- function() {

  if (!"montserrat" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("Montserrat", "montserrat")
  }

  if (!"jetbrains_mono" %in% sysfonts::font_families()) {
    sysfonts::font_add_google("JetBrains Mono", "jetbrains_mono")
  }

  showtext::showtext_auto()

  ggplot2::theme(

    # backgrounds
    plot.background = ggplot2::element_rect(
      fill = "#fff",
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "#EAEFEC",
      color = NA
    ),

    # grid
    panel.grid.major = ggplot2::element_line(
      color = "#EAEFEC"
    ),
    panel.grid.minor = ggplot2::element_line(
      color = "#EAEFEC"
    ),

    # axes
    axis.text = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),
    axis.title = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),

    # title
    plot.title = ggplot2::element_text(
      color = "#0483A4",
      family = "montserrat",
      size = 16,
      face = "bold",
      hjust = 0.5
    ),

    # caption
    plot.caption = ggplot2::element_text(
      color = "#292C2F",
      family = "jetbrains_mono"
    ),

    # legend
    legend.background = ggplot2::element_rect(
      fill = "#fff",
      color = NA
    ),
    legend.key = ggplot2::element_rect(
      fill = "#EAEFEC",
      color = NA
    ),
    legend.text = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat"
    ),
    legend.title = ggplot2::element_text(
      color = "#292C2F",
      family = "montserrat",
      face = "bold"
    )
  )
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


#' Plot a BirdFlightMap raster
#'
#' Plots a BirdFlightMap output from a seabORD simulation together with
#' the UK coastline for geographical context.
#'
#' @param BirdFlightMap A BirdFlightMap raster, such as
#'   `sbordout$BirdFlightMap`.
#'
#' @param xlim Optional numeric vector of length 2 defining the longitude
#'   limits. If `NULL`, limits are automatically calculated from cells with
#'   values greater than zero.
#'
#' @param ylim Optional numeric vector of length 2 defining the latitude
#'   limits. If `NULL`, limits are automatically calculated from cells with
#'   values greater than zero.
#'
#' @param title Character string giving the plot title.
#'
#' @return A `ggplot` object.
#'
#' @details
#' The BirdFlightMap is reprojected to the coordinate reference system
#' of the coastline dataset (`cef_coast_4326`) before plotting.
#'
#' @examples
#' \dontrun{
#' sbordout <- seabORD::example_calibration_output_dd
#'
#' plot_BirdFlightMap(
#'   sbordout$BirdFlightMap
#' )
#' }
#'
#' @export
plot_BirdFlightMap <- function(
    BirdFlightMap,
    xlim = NULL,
    ylim = NULL,
    title = "Coastline and BirdFlightMap"
) {

  uk_map <- seabORD::cef_coast_4326 %>%
    dplyr::filter(
      ID_0 %in% c("GBR", "FRO", "GGY", "IMN")
    )

  BirdFlightMap <- terra::rast(BirdFlightMap) %>%
    terra::project(
      terra::crs(uk_map)
    )

  BirdFlightMap_df <- as.data.frame(
    BirdFlightMap,
    xy = TRUE
  )

  layer_name <- names(BirdFlightMap)[1]

  # Derive plotting extent if not supplied
  if (is.null(xlim) || is.null(ylim)) {

    nonzero_cells <- BirdFlightMap_df %>%
      dplyr::filter(.data[[layer_name]] > 0)

    if (nrow(nonzero_cells) > 0) {

      x_buffer <- diff(range(nonzero_cells$x)) * 0.01
      y_buffer <- diff(range(nonzero_cells$y)) * 0.01

      if (is.null(xlim)) {
        xlim <- c(
          min(nonzero_cells$x) - x_buffer,
          max(nonzero_cells$x) + x_buffer
        )
      }

      if (is.null(ylim)) {
        ylim <- c(
          min(nonzero_cells$y) - y_buffer,
          max(nonzero_cells$y) + y_buffer
        )
      }

    } else {

      bb <- sf::st_bbox(uk_map)

      if (is.null(xlim)) {
        xlim <- c(bb["xmin"], bb["xmax"])
      }

      if (is.null(ylim)) {
        ylim <- c(bb["ymin"], bb["ymax"])
      }

    }
  }


  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = BirdFlightMap_df,
      ggplot2::aes(
        x = x,
        y = y,
        fill = .data[[layer_name]]
      )
    ) +
    ggplot2::geom_sf(
      data = uk_map,
      fill = NA,
      color = "#D63333",
      linewidth = 1.5
    ) +
    ggplot2::scale_fill_viridis_c(
      na.value = "grey",
      direction = -1,
      name = "BirdFlightMap"
    ) +
    ggplot2::coord_sf(
      xlim = xlim,
      ylim = ylim
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title
    )
}


#' Plot calibration outputs across prey values
#'
#' Creates calibration plots showing the relationship between prey
#' values and key model outputs used for seabORD calibration. The
#' function displays adult mass loss and chick productivity across the
#' range of prey values tested and highlights the prey range satisfying
#' the specified calibration criteria.
#'
#' @param sbordout A seabORD output object containing calibration
#'   results, including `output_a0$data` and `output_c0$data`.
#'
#' @param calibration A data frame returned by
#'   [find_calibration_range()] containing prey values and logical
#'   indicators of whether adult, chick and combined calibration
#'   criteria are met.
#'
#' @param masslower Lower acceptable bound for proportional adult mass
#'   loss. Defaults to `0.09`.
#'
#' @param massupper Upper acceptable bound for proportional adult mass
#'   loss. Defaults to `0.11`.
#'
#' @param prodlower Lower acceptable bound for chick productivity
#'   (`ChicksPerNest`). Defaults to `0.45`.
#'
#' @param produpper Upper acceptable bound for chick productivity
#'   (`ChicksPerNest`). Defaults to `0.55`.
#'
#' @return
#' A two-panel plot (a `gtable` object produced by
#' [gridExtra::grid.arrange()]) showing:
#' \itemize{
#'   \item Adult mass loss as a function of prey value.
#'   \item Chick productivity (chicks per nest) as a function of prey
#'   value.
#' }
#'
#' The light blue shaded region indicates prey values that satisfy both
#' the adult mass-loss and chick-productivity criteria simultaneously.
#' Grey dashed horizontal lines indicate the calibration bounds used to
#' define acceptable ("moderate") conditions.
#'
#' @details
#' Adult mass loss is calculated as:
#' \deqn{
#' (BM\_adult\_t0.mn - BM\_adult.mn) / BM\_adult\_t0.mn
#' }
#'
#' The calibration range is determined using
#' [find_calibration_range()]. Prey values within the shaded region
#' satisfy both the adult and chick calibration criteria and may be used
#' as candidate prey values for subsequent scenario simulations.
#'
#' @examples
#' \dontrun{
#' sbordout <- seabORD::example_calibration_output_dd
#'
#' calibration <- seabORD::find_calibration_range(
#'   sbordout = sbordout,
#'   masslower = 0.09,
#'   massupper = 0.11,
#'   prodlower = 0.45,
#'   produpper = 0.55
#' )
#'
#' seabORD::plot_calibration_outputs(
#'   sbordout = sbordout,
#'   calibration = calibration,
#'   masslower = 0.09,
#'   massupper = 0.11,
#'   prodlower = 0.45,
#'   produpper = 0.55
#' )
#' }
#'
#' @seealso
#' [find_calibration_range()] for identifying prey values that satisfy
#' the calibration criteria.
#'
#' @export
plot_calibration_outputs <- function(
    sbordout,
    calibration,
    masslower = 0.09,
    massupper = 0.11,
    prodlower = 0.45,
    produpper = 0.55
) {

  valid_prey <- calibration$Prey[calibration$overlap]

  xmin <- min(valid_prey)
  xmax <- max(valid_prey)

  adult_data <- sbordout$output_a0$data %>%
    dplyr::mutate(
      percentage_loss =
        (BM_adult_t0.mn - BM_adult.mn) /
        BM_adult_t0.mn
    )

  pAd <- ggplot2::ggplot(
    adult_data,
    ggplot2::aes(
      x = Prey,
      y = percentage_loss
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = "lightblue",
      alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = c(masslower, massupper),
      linetype = 2,
      colour = "darkgrey"
    ) +
    ggplot2::geom_point(
      colour = "#D63333"
    ) +
    ggplot2::geom_line(
      colour = "#D63333"
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = "Adult mass loss",
      x = "Prey",
      y = "Proportional mass loss"
    )

  pCh <- ggplot2::ggplot(
    sbordout$output_c0$data,
    ggplot2::aes(
      x = Prey,
      y = ChicksPerNest
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = "lightblue",
      alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = c(prodlower, produpper),
      linetype = 2,
      colour = "darkgrey"
    ) +
    ggplot2::geom_point(
      colour = "#D63333"
    ) +
    ggplot2::geom_line(
      colour = "#D63333"
    ) +
    ggplot2::geom_vline(
      xintercept = c(xmin, xmax),
      colour = "black"
    ) +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = "Chicks per nest",
      x = "Prey",
      y = "Chicks per nest"
    )

  gridExtra::grid.arrange(
    pAd,
    pCh,
    ncol = 1
  )
}



#' Plot adult body mass by season
#'
#' Creates a boxplot of adult body mass grouped by season.
#'
#' @param sbordout_output_adults
#'
#' @param title Character string giving the plot title.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' plot_adult_body_mass(
#'   adults = adults
#' )
#'
#' @export
plot_adult_body_mass <- function(
    sbordout_output_adults,
    title = "Adult body mass by season"
) {

  adults <- sbordout_output_adults$data

  ggplot2::ggplot(
    adults,
    ggplot2::aes(
      x = Season,
      y = BM_adult.mn
    )
  ) +
    ggplot2::geom_boxplot() +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title,
      x = "Season",
      y = "Adult body mass"
    )

}


#' Plot chick survival by season
#'
#' Creates a boxplot of the number of chicks alive grouped by season.
#'
#' @param sbordout_output_chicks
#'
#' @param title Character string giving the plot title.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' plot_chick_survival(
#'   chicks = chicks
#' )
#'
#' @export
plot_chick_survival <- function(
    sbordout_output_chicks,
    title = "Chicks alive by season"
) {

  chicks <- sbordout_output_chicks$data

  ggplot2::ggplot(
    chicks,
    ggplot2::aes(
      x = Season,
      y = N_alive_ch
    )
  ) +
    ggplot2::geom_boxplot() +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = title,
      x = "Season",
      y = "Number of chicks alive"
    )

}


#' Plot flight distance by season
#'
#' @param sbordout A seabORD model output object.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_flight_distance <- function(
    sbordout_output_flight
) {

  ggplot2::ggplot(
    sbordout_output_flight$data,
    ggplot2::aes(
      x = Season,
      y = Tot_basickm.mn
    )
  ) +
    ggplot2::geom_boxplot() +
    seabORD::theme_seabord() +
    ggplot2::labs(
      title = "Flight distance by season",
      x = "Season",
      y = "Flight distance (km)"
    )

}
