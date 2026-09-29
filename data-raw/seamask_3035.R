## code to prepare `seamask_3035` dataset goes here


#to handle rasters save both the raster data as a matrix and its metadata to then compose the raster back again
seamask_3035_r <- terra::rast("C:/Users/madtig/Documents/repositories/SeabORD/seabord-r-dev/Data/seamask_3035.tif")

seamask_3035_m <- terra::as.matrix(seamask_3035_r)

#save the metadata
nrows <- nrow(seamask_3035_r)
ncols <- ncol(seamask_3035_r)
xmin <- as.vector(ext(seamask_3035_r)[1])
xmax <- as.vector(ext(seamask_3035_r)[2])
ymin <- as.vector(ext(seamask_3035_r)[3])
ymax <- as.vector(ext(seamask_3035_r)[4])
crs <- terra::crs(seamask_3035_r)

seamask_3035_metadata <- list(
  n_rows = nrows,
  n_cols = ncols,
  x_min = xmin,
  x_max = xmax,
  y_min = ymin,
  y_max = ymax,
  crs = crs
)


seamask_3035_example <- list(matrix = seamask_3035_m,
                     metadata = seamask_3035_metadata)

usethis::use_data(seamask_3035_example, overwrite = TRUE)


# re import
# Load in the sea mask of the UK (shows which cells are on land and sea)
example_data_seamask <- seabORD::seamask_3035_example

#make a raster with the right dimension/proj/extents etc.
seamask_example <-
  raster::raster(
    nrows = example_data_seamask$metadata[["n_rows"]],
    ncols = example_data_seamask$metadata[["n_cols"]],
    xmn = example_data_seamask$metadata[["x_min"]],
    xmx = example_data_seamask$metadata[["x_max"]],
    ymn = example_data_seamask$metadata[["y_min"]],
    ymx = example_data_seamask$metadata[["y_max"]],
    crs = example_data_seamask$metadata[["crs"]]
  )

#fill it with the data available
searast <- raster::setValues(seamask_example, #the raster
                             example_data_seamask$matrix) #the values
#Set the name of the layer
names(searast) <- "seamask_3035"
