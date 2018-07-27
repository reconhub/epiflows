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
#' 
#' @return An updated `epiflows` object.
#' 
#' @author Pawel Piatkowski, Zhian Kamvar
#' 
#' @examples
#' 
#' # Setting up the data
#' data(Mex_travel_2009)
#' flows   <- Mex_travel_2009[[1]]
#' outflow <- setNames(flows[["MEX"]], rownames(flows))
#' inflow  <- unlist(flows["MEX", , drop = TRUE])
#' ef <- make_epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]])
#' 
#' # generating random coordinates for demonstration
#' dummy_coordinates <- data.frame(lon = runif(41, min = -95, max = 95),
#'                            lat = runif(41, min = -95, max = 95))
#' print(ef)
#' get_coordinates(ef) # no coordinates yet
#' print(flows <- add_coordinates(ef, coordinates = dummy_coordinates))
#' get_coordinates(flows)
#' get_coordinates(flows, location = "MEX") # coordinates for MEX
#' 
#' \dontrun{
#'   # You can use google maps' geocode functionality if you have a decent 
#'   # internet connection
#'   flows <- add_coordinates(ef, loc_column = "country")
#' }
#' 
#' @export
#' @importFrom stats complete.cases
#' @importFrom stats setNames
add_coordinates <- function(x, coordinates = c("lon", "lat"), loc_column = "id",
                            overwrite = FALSE) {
  if (!inherits(x, "epiflows")) {
    efprint <- as.character(deparse(substitute(x)))
    stop(sprintf("%s must be an object of class epiflows", efprint))
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
      x$linelist[which_rows, coordinates] <- ggmap::geocode(the_locations[which_rows])
    } else {
      # Otherwise, get all geocodes and write them to lon/lat columns
      x$linelist[, coordinates] <- ggmap::geocode(as.character(the_locations))
    }
  }
  x
}
