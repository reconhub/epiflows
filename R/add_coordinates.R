#' Add/Retrieve location coordinates
#' 
#' Adds/Retrieves longitude/latitude values to location data within an epiflows object.
#' Coordinates are added to object's locations slot as `lon` and `lat`
#' columns.
#' 
#' @param x An `epiflows` object.
#' @param loc_column Name of the column where location names are stored
#' (default: "country").
#' @param coordinates Either names of the appended columns with longitudes
#' and latitudes, respectively (default: "lon" and "lat") or a data frame with longitude and latitude columns.
#' @param overwrite If TRUE, retrieves all geocodes, even those already
#' retrieved. If FALSE (default), overwrites only NAs.
#' @param backend Geocoding backend. See \link{get_geocoding_function}.
#' @param ... Optional parameters passed to the geocoding function.
#' 
#' @return An updated `epiflows` object.
#' @md
#' @author Pawel Piatkowski, Salla Toikkanen, Zhian Kamvar
#' 
#' @examples
#' 
#' # Setting up the data
#' data("Brazil_epiflows")
#' data("YF_coordinates")
#' get_coordinates(Brazil_epiflows) # no coordinates yet
#' ef <- add_coordinates(Brazil_epiflows, YF_coordinates[-1])
#' get_coordinates(ef)
#' get_coordinates(ef, location = "Espirito Santo") # coordinates for Espirito Santo
#' \donttest{
#'   # You can use google maps' geocode functionality if you have a decent 
#'   # internet connection
#'   ef2 <- add_coordinates(Brazil_epiflows, loc_column = "id")
#'   ef2
#' }
#' 
#' @export
#' @seealso [map_epiflows()]; [plot.epiflows()]; [get_locations()]; [get_vars()];
#'   [global_vars()]
#' @importFrom stats complete.cases
#' @importFrom stats setNames
add_coordinates <- function(x,
                            coordinates = c("lon", "lat"),
                            loc_column = "id",
                            overwrite = FALSE,
                            backend = "nominatim",
                            ...) {
    
  if (!inherits(x, "epiflows")) {
    efprint <- as.character(deparse(substitute(x)))
    stop(sprintf("%s must be an object of class epiflows", efprint))
  }
  geocoding_fun <- get_geocoding_function(backend)
  if (!is.function(geocoding_fun)) {
    stop("Invalid geocoding function. Please specify a supported backend.")
  }
  if (!is.null(x$vars$coordinates) && !overwrite) {
    stop("coordinates are present in the object. Use `overwrite = TRUE` to replace them.")
  }
  coordinates <- if (is.matrix(coordinates)) as.data.frame(coordinates) else coordinates
  if (is.data.frame(coordinates)) {
    if (ncol(coordinates) != 2) {
      stop("The data frame `coordinates` should contain exactly two columns specifying the longitude and latitude coordinates")
    }
    if (all(c("lon", "lat") %in% tolower(names(coordinates)))) {
      # Ensuring they are in the correct order if they are named lon/lat
      # otherwise, we just assume they are.
      names(coordinates) <- tolower(names(coordinates))
      coordinates <- coordinates[c("lon", "lat")]
    }
    if (overwrite && !is.null(x$vars$coordinates)) {
      names(coordinates)               <- x$vars$coordinates
      x$linelist[x$vars$coordinates] <- coordinates
    } else {
      x$linelist    <- cbind(get_locations(x), coordinates) 
      x$vars$coordinates <- setNames(names(coordinates), c("lon", "lat"))
    }
  } else {
    if (!is.character(coordinates) || length(coordinates) != 2) {
      stop("`coordinates` should contain exactly two character strings")
    }
    x$vars$coordinates <- setNames(coordinates, c("lon", "lat"))
    if (!loc_column %in% names(get_locations(x))) {
      stop(sprintf("`%s` is not a valid column name", loc_column))
    }
    the_locations <- as.character(get_vars(x, loc_column)[[1]])
    if (overwrite && all(coordinates %in% names(get_locations(x)))) {
      # If overwrite == FALSE and lon/lat columns already exist,
      # overwrite only rows with NA lon and lat
      which_rows <- !complete.cases(get_coordinates(x))
      if (any(which_rows)) {
        x$linelist[which_rows, coordinates] <- geocoding_fun(the_locations[which_rows], ...)
      }
    } else {
      # Otherwise, get all geocodes and write them to lon/lat columns
      x$linelist[, coordinates] <- geocoding_fun(as.character(the_locations), ...)
    }
  }
  x
}

#' Get geocoding function
#' 
#' Retrieves a function used by \link{add_coordinates} to geocode coordinates
#' for locations from an `epiflows` object.
#' 
#' @param backend Either a function, or a character string pointing to one of
#' the supported geocoding backends. If `backend` is a function, it should
#' accept a vector of character strings as input (optional parameters can also
#' be passed from \code{add_coordinates()}) and return a data frame or a tibble.
#' 
#' @return A function object.
get_geocoding_function <- function(backend) {
  if (is.function(backend)) {
    backend
  } else {
    switch(
      backend,
      "nominatim" = nominatim_backend_function,
      "ggmap" = ggmap::geocode,
      stop("Please specify a geocoding backend. Supported backends: nominatim, ggmap")
    )
  }
}

#' Nominatim geocoding backend
#' 
#' Wrapper for Nominatim geocoding function provided by `nominatim` package.
#' See the documentation at https://github.com/hrbrmstr/nominatim for details.
#' 
#' @param query Geocoding query.
#' @param ... Optional parameters.
#' 
#' @return A data frame with two columns: `lat` and `lon`.
nominatim_backend_function <- function(query, ...) {
  output <- nominatim::osm_geocode(query = query, ...)
  output[c("lat", "lon")]
}
