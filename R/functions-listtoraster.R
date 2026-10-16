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
