#' Add location coordinates
#' 
#' Adds longitude/latitude values to location data within an epiflows object.
#' Coordinates are added to object's location linelist as `lon` and `lat`
#' columns.
#' 
#' @param x An \code{epiflows} object.
#' @param loc_column Name of the column where location names are stored
#' (default: "country").
#' @param lon_lat_columns Names of the appended columns with longitudes
#' and latitudes, respectively (default: "lon" and "lat").
#' @param overwrite If TRUE, retrieves all geocodes, even those already
#' retrieved. If FALSE (default), overwrites only NAs.
#' 
#' @return An updated \code{epiflows} object.
#' 
#' @author Pawel Piatkowski
#' 
#' @examples
#' flows <- make_epiflows(Mex_travel_2009[[2]], Mex_travel_2009[[1]])
#' flows <- add_coordinates(flows)
#' flows$linelist
#' 
#' @export
add_coordinates <- function(x,
                            loc_column = "country",
                            lon_lat_columns = c("lon", "lat"),
                            overwrite = FALSE) {
  if (!"epiflows" %in% class(x)) {
    stop("`x` must be an object of class epiflows")
  }
  if (!loc_column %in% names(x$linelist)) {
    stop(sprintf("`%s` is not a valid column name", loc_column))
  }
  if (!is.character(lon_lat_columns) || length(lon_lat_columns) != 2) {
    stop("`lon_lat_columns` should contain exactly two character strings")
  }
  if (!overwrite && all(lon_lat_columns %in% names(x$linelist))) {
    # If overwrite == FALSE and lon/lat columns already exist,
    # overwrite only rows with NA lon and lat
    which_rows <- apply(is.na(x$linelist[, lon_lat_columns]), 1, all)
    print(x$linelist[which_rows, loc_column])
    x$linelist[which_rows, lon_lat_columns] <- ggmap::geocode(
      as.character(x$linelist[which_rows, loc_column])
    )
  } else {
    # Otherwise, get all geocodes and write them to lon/lat columns
    x$linelist[, lon_lat_columns] <- ggmap::geocode(
      as.character(x$linelist[, loc_column])
    )
  }
  x
}
