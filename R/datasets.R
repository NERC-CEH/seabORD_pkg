#' Species List Dataset
#'
#' This dataset provides information about the bird species included in seabORD, including their English and Latin names, taxa codes, and 2 letter codes.
#'
#' @format A data frame with the following 13 columns:
#' \describe{
#'   \item{ENGLISH_NAME}{Character: The English name of the species.}
#'   \item{LATIN_NAME}{Character: The scientific (Latin) name of the species.}
#'   \item{TAXA_CODE}{Character: A short taxa code associated with the species.}
#'   \item{SHORT_CODE}{Character: A short code used for quick reference.}
#'   \item{FIVE_LETTER}{Character: A five-letter shorthand for the species.}
#'   \item{BOU_NAME}{Character: The name of the species as recognized by the British Ornithologists' Union (BOU).}
#'   \item{sCRM_NAME}{Character:  NEED EXPL}
#'   \item{CEF}{character: Indicates if the species is included in the CEF project.}
#'   \item{iPCoD}{character: Indicates if the species is included in the iPCoD project.}
#'   \item{SeabORD}{character: Indicates if the species is included in the SeabORD project.}
#'   \item{sCRM}{character: Indicates if the species is included in the sCRM project.}
#'   \item{NEPVA}{character: Indicates if the species is included in the NEPVA project.}
#'   \item{DispMat}{character: Indicates if dispersal matrices are available for the species.}
#' }
#' @source NEED EXPL
"specieslist"

#' SPA Site List Dataset
#'
#' This dataset contains information about Special Protection Areas (SPAs), including site codes, site names.
#'
#' @format A data frame with the following 10 columns:
#' \describe{
#'   \item{SITE_CODE}{Character: A unique code identifying each SPA.}
#'   \item{SITE_NAME}{Character: The name of the SPA.}
#'   \item{CEF.include}{character: Indicates if the site is included in the CEF project (Y) or not (NA).}
#'   \item{Marine.only}{character: Indicates if the site is classified as a marine-only SPA.}
#'   \item{Altnames.sufficient}{character:NEED EXPL}
#'   \item{Altnames.SPApolys}{character:NEED EXPL}
#'   \item{Altnames.SMP}{character:NEED EXPL}
#'   \item{Altnames.Productivity}{character:NEED EXPL}
#'   \item{Altnames.BDMPS}{character:NEED EXPL}
#'   \item{Altnames.FR}{character:NEED EXPL.}
#' }
#' @source NEED EXPL
"spalist"


#' CEF Size Table
#'
#' Needs a description
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{X}{Numeric: An identifier or index for the row.}
#'   \item{species}{Character: The species code.}
#'   \item{site_codes}{Character: The unique code identifying each site.}
#'   \item{site_names}{Character: The name of the site.}
#'   \item{spacolsize}{Numeric: NEED EXPL}
#' }
#' @source NEED EXPL
"CEF_colsize_table"


#' Energetics and Prey Data
#'
#' A dataset containing information on various bird species' energetics and prey data, including metabolic rates, energy requirements, and survival rates under different conditions.
#'
#' @format A data frame with the following 27 columns:
#' \describe{
#'   \item{Code}{Character: A species code.}
#'   \item{Species}{Character: The common name of the species.}
#'   \item{BM_adult_mn}{Numeric: The mean body mass of adults.}
#'   \item{BM_adult_sd}{Numeric: The standard deviation of adult body mass.}
#'   \item{BM_adult_mortf}{Numeric: The mortality factor for adults.}
#'   \item{BM_adult_abdn}{Numeric: The adult abandonment rate.}
#'   \item{BM_chick_mn}{Numeric: The mean body mass of chicks.}
#'   \item{BM_chick_sd}{Numeric: The standard deviation of chick body mass.}
#'   \item{BM_Chick_mortf}{Numeric: The mortality factor for chicks.}
#'   \item{daylength}{Numeric: The day length in hours.}
#'   \item{seasonlength}{Numeric: The length of the season in days.}
#'   \item{unattend_max_hrs}{Numeric: The maximum hours of unattended activity.}
#'   \item{adult_DEE_mn}{Numeric: The mean daily energy expenditure (DEE) for adults.}
#'   \item{adult_DEE_sd}{Numeric: The standard deviation of adult daily energy expenditure.}
#'   \item{chick_DER}{Numeric: The chick daily energy requirement (DER).}
#'   \item{IR_max}{Numeric: The maximum infrared radiation.}
#'   \item{IR_half_a}{Numeric: Parameter related to infrared radiation.}
#'   \item{IR_half_b}{Numeric: Another parameter related to infrared radiation.}
#'   \item{flight_msec}{Numeric: The flight metabolic rate in milliseconds.}
#'   \item{assim_eff}{Numeric: The assimilation efficiency.}
#'   \item{energy_prey}{Numeric: The energy available from prey.}
#'   \item{energy_nest}{Numeric: The energy associated with the nest.}
#'   \item{energy_flight}{Numeric: The energy used in flight.}
#'   \item{energy_searest}{Numeric: The energy associated with sea rest.}
#'   \item{energy_forage}{Numeric: The energy used for foraging.}
#'   \item{energy_warming}{Numeric: The energy used for warming.}
#'   \item{chick_mass_a}{Numeric: The mass of chicks at age 'a'.}
#'   \item{adult_mass_KG}{Numeric: The adult mass in kilograms.}
#'   \item{beta}{Numeric: A parameter used in energetic calculations.}
#'   \item{basesurv_poor}{Numeric: The survival rate under poor conditions.}
#'   \item{basesurv_modr}{Numeric: The survival rate under moderate conditions.}
#'   \item{basesurv_good}{Numeric: The survival rate under good conditions.}
#'   \item{massloss_poor}{Numeric: The mass loss under poor conditions.}
#'   \item{massloss_modr}{Numeric: The mass loss under moderate conditions.}
#'   \item{massloss_good}{Numeric: The mass loss under good conditions.}
#'   \item{chicksurv_poor}{Numeric: The chick survival rate under poor conditions.}
#'   \item{chicksurv_modr}{Numeric: The chick survival rate under moderate conditions.}
#'   \item{chicksurv_good}{Numeric: The chick survival rate under good conditions.}
#' }
#' @source  NEED EXPL
"energeticsandpreydata"

#' cef_coast_4326
#'
#' NEED EXPL
#'
#' @format NEED EXPL.
#' \describe{
#'   \item{COUNTRY }{Character: NEED EXPL.}
#'   \item{ID_0}{Character: NEED EXPL}
#'   \item{geometry}{Numeric: NEED EXPL}
#' }
#' @source  NEED EXPL
"cef_coast_4326"

#' seamask_3035_example
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source Generated from "seamask_3035.tif".
"seamask_3035_example"

#' Spatial Coordinates of Sites
#'
#' A dataset containing spatial coordinates and related attributes for various sites, including information on whether the site is marine, whether data is available for different columns, and the spatial proximity of flight paths.
#'
#' @format A data frame with 23 columns:
#' \describe{
#'   \item{SITECODE}{Character: The unique code identifying each site.}
#'   \item{CEF.include}{Logical: Whether the site is included in the CEF dataset.}
#'   \item{Marine}{Logical: Whether the site is marine.}
#'   \item{Longitude}{Numeric: The longitude of the site.}
#'   \item{Latitude}{Numeric: The latitude of the site.}
#'   \item{dat.CELLNO}{Numeric: The cell number for the site in the data.}
#'   \item{dat.ATSEA}{Logical: Whether the site is at sea.}
#'   \item{flt.LONG}{Numeric: The longitude of the flight path.}
#'   \item{flt.LAT}{Numeric: The latitude of the flight path.}
#'   \item{flt.CELLNO}{Numeric: The cell number for the flight path.}
#'   \item{flt.ATSEA}{Logical: Whether the flight path is at sea.}
#'   \item{flt.DIST}{Numeric: The distance of the flight path from the site.}
#'   \item{flt.near}{Logical: Whether the flight path is near the site.}
#'   \item{datxy.E}{Numeric: The x-coordinate in the East direction for the site.}
#'   \item{datxy.N}{Numeric: The y-coordinate in the North direction for the site.}
#'   \item{datxy.CELLNO}{Numeric: The cell number for the site in the XY grid.}
#'   \item{datxy.ATSEA}{Logical: Whether the XY coordinates are at sea.}
#'   \item{fltxy.E}{Numeric: The x-coordinate in the East direction for the flight path.}
#'   \item{fltxy.N}{Numeric: The y-coordinate in the North direction for the flight path.}
#'   \item{fltxy.CELLNO}{Numeric: The cell number for the flight path in the XY grid.}
#'   \item{fltxy.ATSEA}{Logical: Whether the XY coordinates for the flight path are at sea.}
#'   \item{fltxy.DIST}{Numeric: The distance of the flight path in XY space.}
#'   \item{fltxy.near}{Logical: Whether the flight path is near the site in XY space.}
#' }
#' @source NEED EXPL
"spacoordinates"


#' UK9004171_bysea_3035
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source Generated from "UK9004171_bysea_3035.tif".
"UK9004171_bysea_3035"


#' FlightGridcorrection_3035: TransitionLayer Object
#' @format An object of class `TransitionLayer` with the following components:
#' \describe{
#'   \item{class}{The object is of class `TransitionLayer`, NEED EXPL}
#'   \item{dimensions}{Dimensions of the data grid: 2200 rows and 1577 columns, with a total of 3,469,400 cells.}
#'   \item{resolution}{The resolution of the raster grid: 1000 meters in both x and y directions.}
#'   \item{extent}{Spatial extent of the grid defined by the minimum and maximum x and y coordinates.  The extent is: xmin = 2661966, xmax = 4238966, ymin = 2685159, ymax = 4885159.}
#'   \item{crs}{Coordinate Reference System (CRS) used for the grid: `+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs`}
#'   \item{values}{NEED EXPL}
#'   \item{matrix class}{The data is stored in a sparse matrix format (`dsTMatrix`), which is efficient for handling large datasets with a significant number of zero values.}
#' }
#' @source  NEED EXPL
"FlightGridcorrection_3035"

#' offshorerenewabledevelopmentnames
#'
#' NEED EXPL
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{code}{Character: NEED EXPL}
#'   \item{name}{Character: NEED EXPL}
#'   \item{Emodnet_Site_Name}{Character: NEED EXPL }
#'   \item{Emodnet_PolygonID}{Numeric: NEED EXPL}
#'   \item{Emodnet_Status}{Character: NEED EXPL}
#' }
#' @source NEED EXPL
"offshorerenewabledevelopmentnames"

#' example_1_lists
#'
#' NEED EXPL
#'
#' @format A list NEED EXPL
#' \describe{
#'   \item{\code{switches}}{A list NEED EXPL}
#'   \item{\code{Par}}{A list NEED EXPL}
#'   \item{\code{modPar}}{A list NEED EXPL}
#'   \item{\code{ordPar}}{A list NEED EXPL}
#' }
#' @source NEED EXPL
"example_1_lists"

#' BrdData_example
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source NEED EXPL
"BrdData_example"

#' frgcompdata_example
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source NEED EXPL
"frgcompdata_example"


#' ORDpoly_example
#'
#' NEED EXPL
#'same projection as seamask
#' @format tibble
#' \describe{
#'   \item{COUNTRY}{NEED EXPL}
#'   \item{NAME}{NEED EXPL}
#'   \item{N_TURBINES}{NEED EXPL}
#'   \item{POWER_MW}{NEED EXPL}
#'   \item{STATUS}{NEED EXPL}
#'   \item{YEAR}{NEED EXPL}
#'   \item{COAST_DIST_M}{NEED EXPL}
#'   \item{AREA_SQKM}{NEED EXPL}
#'   \item{NOTES}{NEED EXPL}
#'   \item{Shape_Length}{NEED EXPL}
#'   \item{Shape_Area}{NEED EXPL}
#' }
#' @source NEED EXPL
"ORDpoly_example"

#' example_calibration_output
#'
#' This dataset is stored in an `.rds` file NEED EXPL
#'
#' @format An object of class \code{data.frame} (or any class the `.rds` file contains).
#' \describe{
#'   \item{switches}{list NEED EXPL}
#'   \item{Parameters}{list NEED EXPL}
#'   \item{ordPar}{list NEED EXPL}
#'   \item{modPar}{list NEED EXPL}
#'   \item{thisRun}{list NEED EXPL}
#'   \item{output_f0}{list of tibbles}
#'   \item{output_c0}{list of tibbles}
#'   \item{output_a0}{list of tibbles}
#'   \item{output_y0}{list of tibbles}
#'   \item{birdflightamp}{raster}
#' }
#' @source \href{https://example.com}{Example Source}
"example_calibration_output"


#' example_lists_calibration
#'
#' NEED EXPL
#'
#' @format A list NEED EXPL
#' \describe{
#'   \item{\code{switches}}{A list NEED EXPL}
#'   \item{\code{Par}}{A list NEED EXPL}
#'   \item{\code{modPar}}{A list NEED EXPL}
#'   \item{\code{ordPar}}{A list NEED EXPL}
#' }
#' @source NEED EXPL
"example_lists_calibration"


#' example_scenario_output
#'
#' This dataset is stored in an `.rds` file NEED EXPL
#'
#' @format An object of class \code{data.frame} (or any class the `.rds` file contains).
#' \describe{
#'   \item{switches}{list NEED EXPL}
#'   \item{Parameters}{list NEED EXPL}
#'   \item{ordPar}{list NEED EXPL}
#'   \item{modPar}{list NEED EXPL}
#'   \item{thisRun}{list NEED EXPL}
#'   \item{output_f0}{list of tibbles}
#'   \item{output_c0}{list of tibbles}
#'   \item{output_a0}{list of tibbles}
#'   \item{output_y0}{list of tibbles}
#'   \item{birdflightamp}{raster}
#' }
#' @source \href{https://example.com}{Example Source}
"example_scenario_output"


#' cef_coast_3035
#'
#' NEED EXPL
#'
#' @format NEED EXPL.
#' \describe{
#'   \item{COUNTRY }{Character: NEED EXPL.}
#'   \item{ID_0}{Character: NEED EXPL}
#'   \item{geometry}{Numeric: NEED EXPL}
#' }
#' @source  NEED EXPL
"cef_coast_3035"


#' ORDpoly_example_wfold
#'
#' NEED EXPL
#'same projection as seamask
#' @format tibble
#' \describe{
#'   \item{COUNTRY}{NEED EXPL}
#'   \item{NAME}{NEED EXPL}
#'   \item{N_TURBINES}{NEED EXPL}
#'   \item{POWER_MW}{NEED EXPL}
#'   \item{STATUS}{NEED EXPL}
#'   \item{YEAR}{NEED EXPL}
#'   \item{COAST_DIST_M}{NEED EXPL}
#'   \item{AREA_SQKM}{NEED EXPL}
#'   \item{NOTES}{NEED EXPL}
#'   \item{Shape_Length}{NEED EXPL}
#'   \item{Shape_Area}{NEED EXPL}
#' }
#' @source NEED EXPL
"ORDpoly_example_wfold"

#' DBS_map_example.R
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source Generated from "DBS_map.tif".
"DBS_map_example"

#' DBS_withORDs_example.R
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source Generated from "DBS_withORDs.tif".
"DBS_withORDs_example"


#' example_lists_calibration_dd
#'
#' NEED EXPL
#'
#' @format A list NEED EXPL
#' \describe{
#'   \item{\code{switches}}{A list NEED EXPL}
#'   \item{\code{Par}}{A list NEED EXPL}
#'   \item{\code{modPar}}{A list NEED EXPL}
#'   \item{\code{ordPar}}{A list NEED EXPL}
#' }
#' @source NEED EXPL
"example_lists_calibration_dd"



#' UK9002491_bysea_3035
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source Generated from "UK9002491_bysea_3035.tif".
"UK9002491_bysea_3035"


#' BrdData_example_dd
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source NEED EXPL
"BrdData_example_dd"

#' frgcompdata_example_dd
#'
#' NEED EXPL
#'
#' @format list of "raster" with the values of a raster in a matrix; and "metadata" with the extent and resolution to build back the raster in memory.
#' \describe{
#'   \item{matrix}{matrix of pixel values}
#'   \item{metadata}{List of metadata: extent, resolution, and CRS.}
#' }
#' @source NEED EXPL
"frgcompdata_example_dd"

#' example_lists_dd
#'
#' NEED EXPL
#'
#' @format A list NEED EXPL
#' \describe{
#'   \item{\code{switches}}{A list NEED EXPL}
#'   \item{\code{Par}}{A list NEED EXPL}
#'   \item{\code{modPar}}{A list NEED EXPL}
#'   \item{\code{ordPar}}{A list NEED EXPL}
#' }
#' @source NEED EXPL
"example_lists_dd"

#' UK coastline in ETRS89 / LAEA Europe (EPSG:3035)
#'
#' A simple-feature (`sf`) object containing the coastline of the United
#' Kingdom extracted from the CEF coastline dataset and reprojected to
#' ETRS89 / LAEA Europe (EPSG:3035).
#'
#' The dataset is primarily intended for plotting and spatial operations
#' involving seabORD example rasters and model inputs, which are supplied
#' in the same projected coordinate reference system.
#'
#' @format An `sf` object containing coastline geometries for the United
#'   Kingdom.
#'
#' @details
#' The dataset was derived from the built-in `cef_coast_3035` coastline
#' dataset by selecting features where `COUNTRY == "United Kingdom"` and
#' retaining the geometry in the ETRS89 / LAEA Europe projection
#' (EPSG:3035).
#'
#' The dataset can be used to provide geographical context when plotting
#' seabORD spatial inputs and outputs, including:
#' \itemize{
#'   \item seamasks
#'   \item bird distribution maps (`BrdData`)
#'   \item competition distribution maps (`FrgCompData`)
#'   \item flight-distance rasters
#'   \item offshore renewable development (ORD) footprints
#' }
#'
#' @examples
#' # Load the coastline
#' data("uk_coast_3035", package = "seabORD")
#'
#' # Inspect the object
#' uk_coast_3035
#'
#' # Plot the coastline
#' plot(sf::st_geometry(uk_coast_3035))
#'
#' @source Derived from the built-in `cef_coast_3035` coastline dataset.
#'
"uk_coast_3035"
