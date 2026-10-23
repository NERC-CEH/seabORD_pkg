#' Apply the seabORD plotting theme
#'
#' @description
#' Registers the fonts used by seabORD, enables text rendering with
#' `showtext`, and returns the common `ggplot2` theme used by package plotting
#' functions.
#'
#' @return
#' A [ggplot2::theme()] object.
#'
#' @details
#' The theme uses Montserrat for plot titles, axis labels, axis text, and
#' legends, and JetBrains Mono for plot captions. If these fonts are not
#' already registered, they are retrieved using
#' [sysfonts::font_add_google()].
#'
#' This is an internal helper used to provide consistent styling across
#' seabORD plots.
#'
#' @keywords internal
#' @noRd
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




#' Plot a sea-mask raster
#'
#' @description
#' Creates a map of a sea-mask raster and overlays coastlines for geographical
#' context.
#'
#' @param seamask A [raster::RasterLayer-class] object containing sea-mask
#'   data, such as the object returned by [get_example_seamask()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and sea mask"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing land, sea, and the coastline.
#'
#' @details
#' Cells containing missing values are classified as land, while cells
#' containing non-missing values are classified as sea.
#'
#' The coastline data supplied with the package are transformed to the
#' coordinate reference system of `seamask` before plotting.
#'
#' @examples
#' # Load and plot the example sea mask
#' seamask <- get_example_seamask()
#' plot_seamask(seamask)
#'
#' @seealso
#' [get_example_seamask()] for loading the example sea-mask raster.
#'
#' @family plotting functions
#' @export
plot_seamask <- function(
    seamask,
    title = "Coastline and bird distribution"
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

  seamask_df$mask_type <- ifelse(
    is.na(seamask_df[[layer_name]]),
    "Land",
    "Sea"
  )

  seamask_df$mask_type <- factor(
    seamask_df$mask_type,
    levels = c("Land", "Sea")
  )

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = seamask_df,
      ggplot2::aes_string(
        x = "x",
        y = "y",
        fill = "mask_type"
      )
    ) +
    ggplot2::geom_sf(
      data = uk_map_repr,
      fill = NA,
      color = "#D63333",
      linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Land" = "grey80",
        "Sea" = "#0483A4"
      ),
      name = NULL
    ) +
    theme_seabord() +
    ggplot2::labs(
      title = title
    )
}


#' Plot a bird-distribution raster
#'
#' @description
#' Creates a map of a seabORD bird-distribution raster and overlays coastlines
#' for geographical context.
#'
#' @param brddata A [raster::RasterLayer-class] object containing the relative
#'   spatial distribution of birds, such as the object returned by
#'   [get_example_brddata()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and bird distribution"`.
#' @param zoom Logical. If `TRUE`, zooms the map to the extent of
#'   `brddata` plus a 10 km buffer. Defaults to `FALSE`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the bird-distribution raster and
#' coastline.
#'
#' @details
#' `BrdData` represents the relative spatial distribution of birds across the
#' model domain. This distribution may be derived from GPS tracking data or,
#' where suitable tracking data are unavailable, estimated using a
#' distance-decay model.
#'
#' Raster cells with a value of zero are converted to missing values before
#' plotting. Non-missing values are displayed using a continuous viridis
#' colour scale.
#'
#' The coastline data supplied with the package are transformed to the
#' coordinate reference system of `brddata` before plotting.
#'
#' @examples
#' # Load and plot the standard example bird distribution
#' brddata <- get_example_brddata()
#' plot_brddata(brddata)
#'
#' # Load and plot the distance-decay example
#' brddata_dd <- get_example_brddata("dd")
#' plot_brddata(brddata_dd)
#'
#' @seealso
#' [get_example_brddata()] for loading example bird-distribution rasters and
#' [plot_frgcompdata()] for plotting the corresponding foraging-competition
#' distribution.
#'
#' @family plotting functions
#' @export
plot_brddata <- function(
    brddata,
    title = "Coastline and bird distribution",
    zoom = FALSE
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

  p <- ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = BrdData_df,
                          ggplot2::aes_string(
                            x = "x",
                            y = "y",
                            fill = layer_name
                          )) +
    # Add the UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth = 0.5) +
    # Customize the plot
    theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title =  title)


  if (zoom) {

    BrdData_zoom <- BrdData_df |>
      dplyr::filter(!is.na(.data[[layer_name]]))

    buffer <- 10000 # 10 km

    p <- p +
      ggplot2::coord_sf(
        xlim = c(
          min(BrdData_zoom$x) - buffer,
          max(BrdData_zoom$x) + buffer
        ),
        ylim = c(
          min(BrdData_zoom$y) - buffer,
          max(BrdData_zoom$y) + buffer
        ),
        expand = FALSE
      )

  }

  p

}

#' Plot a foraging-competition raster
#'
#' @description
#' Creates a map of the spatial distribution of birds from colonies that are
#' not simulated directly but may compete with the modelled population at
#' foraging locations.
#'
#' @param frgcompdata A [raster::RasterLayer-class] object containing the
#'   foraging-competition distribution, such as the object returned by
#'   [get_example_frgcompdata()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and foraging-competition distribution"`.
#' @param zoom Logical. If `TRUE`, zooms the map to the extent of
#'   `frgcompdata` plus a 10 km buffer. Defaults to `FALSE`.
#' @return
#' A [ggplot2::ggplot()] object showing the foraging-competition raster and
#' coastline.
#'
#' @details
#' The input raster represents the spatial distribution of birds from
#' surrounding colonies that are not simulated directly. These birds may
#' compete with the modelled population and influence intake rates at
#' foraging locations.
#'
#' Raster cells with a value of zero are converted to missing values before
#' plotting. Non-missing values are displayed using a continuous viridis
#' colour scale.
#'
#' The coastline data supplied with the package are transformed to the
#' coordinate reference system of `frgcompdata` before plotting.
#'
#' @examples
#' # Load and plot the standard example competition distribution
#' frgcompdata <- get_example_frgcompdata()
#' plot_frgcompdata(frgcompdata)
#'
#' # Load and plot the density-dependence example
#' frgcompdata_dd <- get_example_frgcompdata("dd")
#' plot_frgcompdata(frgcompdata_dd)
#'
#' @seealso
#' [get_example_frgcompdata()] for loading example foraging-competition
#' rasters and [plot_brddata()] for plotting bird-distribution data.
#'
#' @family plotting functions
#' @export
plot_frgcompdata <- function(
    frgcompdata,
    title = "Coastline and foraging-competition distribution",
    zoom = FALSE
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

  p <- ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = FrgCompData_df,  ggplot2::aes_string(
      x = "x",
      y = "y",
      fill = layer_name
    )) +
    # Add the reprojected UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth  = 0.5) +
    # Customize the plot
    theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title = title)


  if (zoom) {

    FrgCompData_zoom <- FrgCompData_df |>
      dplyr::filter(!is.na(.data[[layer_name]]))

    buffer <- 10000 # 10 km

    p <- p +
      ggplot2::coord_sf(
        xlim = c(
          min(FrgCompData_zoom$x) - buffer,
          max(FrgCompData_zoom$x) + buffer
        ),
        ylim = c(
          min(FrgCompData_zoom$y) - buffer,
          max(FrgCompData_zoom$y) + buffer
        ),
        expand = FALSE
      )

  }

  p

}

#' Plot a baseline flight-distance raster
#'
#' @description
#' Creates a map of baseline flight distances from a breeding colony to
#' accessible foraging locations and overlays coastlines for geographical
#' context.
#'
#' @param fltdist_base A [raster::RasterLayer-class] object containing
#'   baseline flight distances, such as the object returned by
#'   [get_example_fltdist_base()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and baseline flight distance"`.
#' @param zoom Logical. If `TRUE`, zooms the map to the extent of
#'   `fltdist_base` plus a 10 km buffer. Defaults to `FALSE`.
#' @return
#' A [ggplot2::ggplot()] object showing the baseline flight-distance raster
#' and coastline.
#'
#' @details
#' The input raster represents distance by sea from a breeding colony or SPA
#' to each accessible raster cell. These distances are used by seabORD to
#' calculate travel time and the energetic costs associated with foraging
#' trips.
#'
#' Raster cells with a value of zero are converted to missing values before
#' plotting. Non-missing values are displayed using a continuous viridis
#' colour scale.
#'
#' The coastline data supplied with the package are transformed to the
#' coordinate reference system of `fltdist_base` before plotting.
#'
#' @examples
#' # Load and plot the standard example flight-distance raster
#' fltdist_base <- get_example_fltdist_base()
#' plot_fltdist_base(fltdist_base)
#'
#' # Load and plot the density-dependence example
#' fltdist_base_dd <- get_example_fltdist_base("dd")
#' plot_fltdist_base(fltdist_base_dd)
#'
#' @seealso
#' [get_example_fltdist_base()] for loading example flight-distance rasters
#' and [plot_DBS_map()] for plotting a distance-by-sea raster.
#'
#' @family plotting functions
#' @export
plot_fltdist_base <- function(
    fltdist_base,
    title = "Coastline and baseline flight distance",
    zoom = FALSE
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

  p <- ggplot2::ggplot() +
    # Plot the raster
    ggplot2:: geom_raster(data = fltdist_base_df,  ggplot2::aes_string(
      x = "x",
      y = "y",
      fill = layer_name
    )) +
    # Add the reprojected UK boundary
    ggplot2::geom_sf(data = uk_map_repr, fill = NA, color = "#D63333", linewidth  = 0.5) +
    # Customize the plot
    theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",  # Set the color for NA (i.e., for 0 values)
      name = layer_name, direction = -1) +
    ggplot2::labs(title = title)


  if (zoom) {

    fltdist_base_zoom <- fltdist_base_df |>
      dplyr::filter(!is.na(.data[[layer_name]]))

    buffer <- 10000 # 10 km

    p <- p +
      ggplot2::coord_sf(
        xlim = c(
          min(fltdist_base_zoom$x) - buffer,
          max(fltdist_base_zoom$x) + buffer
        ),
        ylim = c(
          min(fltdist_base_zoom$y) - buffer,
          max(fltdist_base_zoom$y) + buffer
        ),
        expand = FALSE
      )

  }

  p

}

#' Plot offshore renewable development polygons
#'
#' @description
#' Creates a map of offshore renewable development polygons and overlays
#' coastlines for geographical context.
#'
#' @param ORDpoly An `sf` polygon object containing one or more offshore
#'   renewable development footprints, such as the object returned by
#'   [get_example_ORDpoly()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and ORD footprints"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the offshore renewable development
#' footprints and coastline.
#'
#' @details
#' The plotting extent is calculated by transforming the ORD polygons to
#' EPSG:3035, applying a 50-km buffer around their combined bounding box, and
#' transforming the resulting extent to the coordinate reference system of
#' the coastline data.
#'
#' Coastlines are displayed in red and ORD footprints are displayed in blue.
#'
#' @examples
#' # Load and plot the standard example ORD polygons
#' ORDpoly <- get_example_ORDpoly()
#' plot_ORDpoly(ORDpoly)
#'
#' # Load and plot the density-dependence example
#' ORDpoly_dd <- get_example_ORDpoly("dd")
#' plot_ORDpoly(ORDpoly_dd)
#'
#' @seealso
#' [get_example_ORDpoly()] for loading example ORD polygons and
#' [plot_DD_map_with_ORDs()] for plotting ORD footprints over a
#' distance-decay surface.
#'
#' @family plotting functions
#' @export
plot_ORDpoly <- function(
    ORDpoly,
    title = "Coastline and ORD footprints"
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
      linewidth = 0.5
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
    theme_seabord() +
    ggplot2::labs(
      title = title
    )

}

#' Plot a colony location and surrounding coastline
#'
#' @description
#' Creates a detailed map of a colony location and the surrounding coastline.
#'
#' @param colony An `sf` point object containing the colony location. The
#'   object must use the ETRS89-extended / LAEA Europe coordinate reference
#'   system (EPSG:3035).
#' @param expand_km Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting window from the colony in each
#'   direction. Defaults to `1`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the colony location and surrounding
#' coastline.
#'
#' @details
#' The plot is centred on the supplied colony point and displays a square
#' plotting window extending `expand_km` kilometres to the north, south, east,
#' and west.
#'
#' This plot can be used to check that colony coordinates are correctly
#' positioned on the coastline before generating distance-by-sea rasters or
#' running a seabORD simulation.
#'
#' The coastline is displayed in grey and the colony is shown using a blue
#' cross.
#'
#' @examples
#' \dontrun{
#' colony <- create_colony_point(
#'   lon = -1.85849,
#'   lat = 57.388807
#' )
#'
#' plot_colony_location(
#'   colony = colony,
#'   expand_km = 1
#' )
#' }
#'
#' @family plotting functions
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
      title = "Colony location and surrounding coastline"
    ) +
    theme_seabord()

}


#' Plot a distance-by-sea raster
#'
#' @description
#' Creates a map of distance-by-sea values and overlays coastlines for
#' geographical context.
#'
#' @param DBS_map A [raster::RasterLayer-class] object containing
#'   distance-by-sea values, such as the object returned by
#'   [get_example_DBS_map()].
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and distance by sea"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distance-by-sea raster and
#' coastline.
#'
#' @details
#' A distance-by-sea raster represents the distance travelled over accessible
#' sea cells rather than the straight-line Euclidean distance between
#' locations.
#'
#' Raster cells with a value of zero are converted to missing values before
#' plotting. Non-missing values are displayed using a continuous viridis
#' colour scale.
#'
#' The coastline data supplied with the package are transformed to the
#' coordinate reference system of `DBS_map` before plotting.
#'
#' @examples
#' # Load and plot the baseline distance-by-sea raster
#' DBS_map <- get_example_DBS_map()
#' plot_DBS_map(DBS_map)
#'
#' # Load and plot the distance-by-sea raster containing ORDs
#' DBS_with_ORDs <- get_example_DBS_map("ords")
#' plot_DBS_map(DBS_with_ORDs)
#'
#' @seealso
#' [get_example_DBS_map()] for loading example distance-by-sea rasters and
#' [plot_ORD_obstruction()] for identifying areas affected by ORD
#' obstruction.
#'
#' @family plotting functions
#' @export
plot_DBS_map <- function(
    DBS_map,
    title = "Coastline and distance by sea"
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
      linewidth = 0.5
    ) +
    theme_seabord() +
    ggplot2::scale_fill_viridis_c(
      na.value = "gray",
      direction = -1,
      name = "Distance by sea"
    ) +
    ggplot2::labs(
      title = title
    )
}


#' Plot a distance-decay map
#'
#' @description
#' Creates a map of a distance-decay raster and optionally crops the map
#' around a colony location.
#'
#' @param DD_map A [raster::RasterLayer-class] object containing
#'   distance-decay values.
#' @param colony An `sf` point object containing the colony location. This is
#'   required when `crop = TRUE` and must use a projected coordinate reference
#'   system with coordinates measured in metres.
#' @param km_to_left Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the cropped area west of the colony. Defaults to
#'   `30`.
#' @param km_to_right Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the cropped area east of the colony. Defaults to
#'   `50`.
#' @param km_up Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the cropped area north of the colony. Defaults to
#'   `30`.
#' @param km_down Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the cropped area south of the colony. Defaults to
#'   `70`.
#' @param crop Logical scalar indicating whether `DD_map` should be cropped
#'   around `colony`. Defaults to `FALSE`.
#' @param title Character string giving the plot title. Defaults to
#'   `"Distance-decay map"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distance-decay raster.
#'
#' @details
#' When `crop = FALSE`, the complete raster is plotted.
#'
#' When `crop = TRUE`, `colony` must be supplied. The plotting extent is
#' calculated using the four directional distances. These distances are
#' converted from kilometres to metres, so the colony and raster must use a
#' projected coordinate reference system with coordinates measured in metres.
#'
#' @examples
#' \dontrun{
#' # Plot the complete distance-decay map
#' plot_DD_map(DD_map)
#'
#' # Plot the map cropped around a colony
#' plot_DD_map(
#'   DD_map = DD_map,
#'   colony = colony,
#'   crop = TRUE
#' )
#' }
#'
#' @seealso
#' [plot_DD_map_with_ORDs()] for adding ORD footprints and buffer zones.
#'
#' @family plotting functions
#' @export
plot_DD_map <- function(
    DD_map,
    colony = NULL,
    km_to_left = 30,
    km_to_right = 50,
    km_up = 30,
    km_down = 70,
    crop = FALSE,
    title = "Distance-decay map"
) {

  if (crop) {

    if (is.null(colony)) {
      stop("A colony location must be supplied when crop = TRUE.")
    }

    crop_extent <- set_crop_extent_colony(
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
    theme_seabord() +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_viridis_c(
      name = layer_name
    ) +
    ggplot2::labs(
      title = title
    )
}

#' Plot a distance-decay map with ORD footprints
#'
#' @description
#' Creates a map showing a distance-decay raster together with offshore
#' renewable development footprints, associated buffer zones, and the
#' coastline.
#'
#' @param DD_map A [raster::RasterLayer-class] object containing
#'   distance-decay values.
#' @param ORDpolys An `sf` polygon object containing one or more offshore
#'   renewable development footprints.
#' @param colony An `sf` point object containing the colony location used to
#'   define the plotting extent. The object must use a projected coordinate
#'   reference system with coordinates measured in metres.
#' @param bufferwidth Non-negative numeric scalar giving the width, in
#'   kilometres, of the buffer created around each ORD footprint. Defaults to
#'   `2`.
#' @param km_to_left Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area west of the colony. Defaults to
#'   `30`.
#' @param km_to_right Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area east of the colony. Defaults to
#'   `50`.
#' @param km_up Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area north of the colony. Defaults to
#'   `30`.
#' @param km_down Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area south of the colony. Defaults to
#'   `70`.
#' @param title Character string giving the plot title. Defaults to
#'   `"Distance-decay map and ORD footprints"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distance-decay raster, ORD
#' footprints, ORD buffer zones, and coastline.
#'
#' @details
#' The distance-decay raster and coastline are cropped around the colony
#' before plotting. This focuses the map on the area of interest and reduces
#' the amount of spatial data that must be rendered.
#'
#' ORD footprints are displayed in blue and their associated buffer zones are
#' displayed in light blue. Distance-decay values are displayed using a
#' continuous viridis colour scale.
#'
#' All spatial inputs should use compatible projected coordinate reference
#' systems.
#'
#' @examples
#' \dontrun{
#' plot_DD_map_with_ORDs(
#'   DD_map = DD_map,
#'   ORDpolys = ORDpolys,
#'   colony = colony,
#'   bufferwidth = 2
#' )
#' }
#'
#' @seealso
#' [plot_DD_map()] for plotting a distance-decay raster without ORD
#' footprints, [get_example_ORDpoly()] for loading example ORD polygons, and
#' [plot_ORD_obstruction()] for identifying obstructed areas.
#'
#' @family plotting functions
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

  ORDpolys_buffer <- set_ORD_buffer(
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
    theme_seabord() +
    ggplot2::labs(
      title = title
    )
}


#' Plot areas affected by ORD obstruction
#'
#' @description
#' Identifies and plots raster cells where offshore renewable developments
#' increase distance by sea relative to the unobstructed baseline.
#'
#' @param DBS_map A [raster::RasterLayer-class] object containing baseline
#'   distance-by-sea values calculated without ORD obstruction.
#' @param DBS_withORDs A [raster::RasterLayer-class] object containing
#'   distance-by-sea values calculated with ORD obstruction.
#' @param colony An `sf` point object containing the colony location used to
#'   define the plotting extent. The object must use a projected coordinate
#'   reference system with coordinates measured in metres.
#' @param smalltol Non-negative numeric scalar defining the minimum increase
#'   in distance by sea required for a cell to be classified as obstructed.
#'   Defaults to `0.01`.
#' @param km_to_left Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area west of the colony. Defaults to
#'   `30`.
#' @param km_to_right Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area east of the colony. Defaults to
#'   `50`.
#' @param km_up Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area north of the colony. Defaults to
#'   `30`.
#' @param km_down Non-negative numeric scalar giving the distance, in
#'   kilometres, to extend the plotting area south of the colony. Defaults to
#'   `70`.
#'
#' @return
#' A [ggplot2::ggplot()] object classifying raster cells as accessible sea,
#' obstructed sea, or land.
#'
#' @details
#' A cell is classified as obstructed when:
#'
#' \deqn{DBS_{\mathrm{with\ ORDs}} - DBS_{\mathrm{baseline}} >
#' \mathrm{smalltol}}
#'
#' Obstructed cells therefore represent locations for which the presence of
#' ORD footprints alters the available routes and increases the distance
#' travelled over sea.
#'
#' The comparison raster is cropped around `colony` using the specified
#' directional distances. Accessible sea is displayed in white, obstructed
#' sea in red, and land in grey.
#'
#' @examples
#' \dontrun{
#' DBS_map <- get_example_DBS_map()
#' DBS_withORDs <- get_example_DBS_map("ords")
#'
#' plot_ORD_obstruction(
#'   DBS_map = DBS_map,
#'   DBS_withORDs = DBS_withORDs,
#'   colony = colony
#' )
#' }
#'
#' @seealso
#' [get_example_DBS_map()] for loading the baseline and ORD-inclusive
#' distance-by-sea rasters, and [plot_DBS_map()] for plotting either raster
#' separately.
#'
#' @family plotting functions
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

  crop_extent <- set_crop_extent_colony(
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
    theme_seabord() +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Areas affected by ORD obstruction"
    )
}

#' Plot a bird-flight map
#'
#' @description
#' Creates a map of simulated bird foraging locations from a seabORD
#' `BirdFlightMap` output and overlays coastlines for geographical context.
#'
#' @param BirdFlightMap A `RasterLayer`, `SpatRaster`, or compatible raster
#'   object containing the number of simulated visits to each foraging
#'   location, such as `sbordout$BirdFlightMap`.
#' @param xlim Optional numeric vector of length two giving the lower and
#'   upper x-axis limits. When `NULL`, the limits are calculated from raster
#'   cells with values greater than zero.
#' @param ylim Optional numeric vector of length two giving the lower and
#'   upper y-axis limits. When `NULL`, the limits are calculated from raster
#'   cells with values greater than zero.
#' @param title Character string giving the plot title. Defaults to
#'   `"Coastline and bird-flight map"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the simulated foraging locations and
#' coastline.
#'
#' @details
#' The input raster is converted to a `terra` `SpatRaster` and transformed to
#' the coordinate reference system of the coastline data before plotting.
#'
#' If `xlim` or `ylim` is not supplied, the relevant limits are calculated
#' from raster cells with values greater than zero, with a one-percent buffer
#' added around the observed range.
#'
#' If the raster contains no positive values, the coastline bounding box is
#' used as the plotting extent.
#'
#' @examples
#' \dontrun{
#' sbordout <- get_example_output()
#'
#' plot_BirdFlightMap(
#'   BirdFlightMap = sbordout$BirdFlightMap
#' )
#' }
#'
#' @seealso
#' [get_example_output()] for loading precomputed seabORD output.
#'
#' @family plotting functions
#' @export
plot_BirdFlightMap <- function(
    BirdFlightMap,
    xlim = NULL,
    ylim = NULL,
    title = "Coastline and bird-flight map"
) {

  uk_map <- sf::st_as_sf(seabORD::cef_coast_4326)

  uk_map <- uk_map[
    uk_map$ID_0 %in% c("GBR", "FRO", "GGY", "IMN"),
  ]

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
      linewidth = 0.5
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
    theme_seabord() +
    ggplot2::labs(
      title = title
    )
}

#' Plot calibration outputs across prey values
#'
#' @description
#' Creates paired calibration plots showing adult proportional mass loss and
#' chick productivity across the range of prey values evaluated during a
#' seabORD calibration run.
#'
#' @param sbordout A seabORD calibration-output object containing
#'   `output_a0$data` and `output_c0$data`.
#' @param calibration A data frame returned by [find_calibration_range()].
#'   It must contain `Prey` and `overlap` columns identifying prey values that
#'   satisfy both the adult and chick calibration criteria.
#' @param masslower Numeric scalar giving the lower acceptable bound for
#'   proportional adult mass loss. Defaults to `0.09`.
#' @param massupper Numeric scalar giving the upper acceptable bound for
#'   proportional adult mass loss. Defaults to `0.11`.
#' @param prodlower Numeric scalar giving the lower acceptable bound for chick
#'   productivity, measured as chicks per nest. Defaults to `0.45`.
#' @param produpper Numeric scalar giving the upper acceptable bound for chick
#'   productivity, measured as chicks per nest. Defaults to `0.55`.
#'
#' @return
#' A `gtable` object produced by [gridExtra::grid.arrange()] containing two
#' vertically arranged calibration plots:
#' \itemize{
#'   \item proportional adult mass loss across prey values; and
#'   \item chick productivity across prey values.
#' }
#'
#' @details
#' Proportional adult mass loss is calculated as:
#'
#' \deqn{
#' \frac{BM_{\mathrm{adult},t0} - BM_{\mathrm{adult}}}
#' {BM_{\mathrm{adult},t0}}
#' }
#'
#' The light-blue region identifies the range of prey values for which
#' `calibration$overlap` is `TRUE`. These prey values satisfy the adult
#' mass-loss and chick-productivity criteria simultaneously.
#'
#' Grey dashed horizontal lines show the lower and upper calibration bounds.
#' Solid vertical lines show the minimum and maximum prey values in the
#' overlapping calibration range.
#'
#' @examples
#' \dontrun{
#' sbordout <- get_example_output("cal_dd")
#'
#' calibration <- find_calibration_range(
#'   sbordout = sbordout,
#'   masslower = 0.09,
#'   massupper = 0.11,
#'   prodlower = 0.45,
#'   produpper = 0.55
#' )
#'
#' plot_calibration_outputs(
#'   sbordout = sbordout,
#'   calibration = calibration
#' )
#' }
#'
#' @seealso
#' [find_calibration_range()] for identifying prey values that satisfy the
#' calibration criteria and [get_example_output()] for loading example
#' calibration output.
#'
#' @family plotting functions
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

  if (length(valid_prey) == 0L) {
    stop(
      "No prey values satisfy both calibration criteria.",
      call. = FALSE
    )
  }

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
    ggplot2::geom_vline(
      xintercept = c(xmin, xmax),
      colour = "black"
    ) +
    theme_seabord() +
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
    theme_seabord() +
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
#' @description
#' Creates a boxplot comparing mean adult body mass between modelled seasons.
#'
#' @param sbordout_output_adults A seabORD adult-output object containing a
#'   `data` component. The data must include the columns `Season` and
#'   `BM_adult.mn`, such as `sbordout$output_a0`.
#' @param title Character string giving the plot title. Defaults to
#'   `"Adult body mass by season"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distribution of mean adult body
#' mass within each season.
#'
#' @details
#' Each boxplot summarises `BM_adult.mn` across simulation replicates for one
#' modelled season. This allows the baseline and scenario distributions of
#' adult body mass to be compared.
#'
#' @examples
#' \dontrun{
#' sbordout <- get_example_output()
#'
#' plot_adult_body_mass(
#'   sbordout_output_adults = sbordout$output_a0
#' )
#' }
#'
#' @seealso
#' [plot_chick_survival()] for plotting chick survival and
#' [plot_flight_distance()] for plotting mean flight distance.
#'
#' @family plotting functions
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
    theme_seabord() +
    ggplot2::labs(
      title = title,
      x = "Season",
      y = "Adult body mass"
    )

}

#' Plot chick survival by season
#'
#' @description
#' Creates a boxplot comparing the number of chicks alive between modelled
#' seasons.
#'
#' @param sbordout_output_chicks A seabORD chick-output object containing a
#'   `data` component. The data must include the columns `Season` and
#'   `N_alive_ch`, such as `sbordout$output_c0`.
#' @param title Character string giving the plot title. Defaults to
#'   `"Chicks alive by season"`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distribution of the number of
#' chicks alive within each season.
#'
#' @details
#' Each boxplot summarises `N_alive_ch` across simulation replicates for one
#' modelled season. This allows chick survival in the baseline and scenario
#' seasons to be compared.
#'
#' @examples
#' \dontrun{
#' sbordout <- get_example_output()
#'
#' plot_chick_survival(
#'   sbordout_output_chicks = sbordout$output_c0
#' )
#' }
#'
#' @seealso
#' [plot_adult_body_mass()] for plotting adult body mass and
#' [plot_flight_distance()] for plotting mean flight distance.
#'
#' @family plotting functions
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
    theme_seabord() +
    ggplot2::labs(
      title = title,
      x = "Season",
      y = "Number of chicks alive"
    )

}


#' Plot mean flight distance by season
#'
#' @description
#' Creates a boxplot comparing mean total baseline flight distance between
#' modelled seasons.
#'
#' @param sbordout_output_flight A seabORD flight-output object containing a
#'   `data` component. The data must include the columns `Season` and
#'   `Tot_basickm.mn`, such as `sbordout$output_f0`.
#'
#' @return
#' A [ggplot2::ggplot()] object showing the distribution of mean total flight
#' distance within each season.
#'
#' @details
#' `Tot_basickm.mn` is the mean total baseline flight distance, in kilometres,
#' across simulated birds within a replicate.
#'
#' Each boxplot summarises `Tot_basickm.mn` across simulation replicates for
#' one modelled season. This allows flight-distance distributions in the
#' baseline and scenario seasons to be compared.
#'
#' @examples
#' \dontrun{
#' sbordout <- get_example_output()
#'
#' plot_flight_distance(
#'   sbordout_output_flight = sbordout$output_f0
#' )
#' }
#'
#' @seealso
#' [plot_adult_body_mass()] for plotting adult body mass and
#' [plot_chick_survival()] for plotting chick survival.
#'
#' @family plotting functions
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
    theme_seabord() +
    ggplot2::labs(
      title = "Flight distance by season",
      x = "Season",
      y = "Flight distance (km)"
    )

}
