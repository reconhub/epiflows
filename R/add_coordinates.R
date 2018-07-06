#' Add location coordinates
#' 
#' Adds longitude/latitude values to location data within an epiflows object.
#' Coordinates are added to object's locations slot as `lon` and `lat`
#' columns.
#' 
#' @param ef An \code{epiflows} object.
#' @param loc_column Name of the column where location names are stored
#' (default: "country").
#' @param coords Either names of the appended columns with longitudes
#' and latitudes, respectively (default: "lon" and "lat") or a data frame with longitude and latitude columns.
#' @param overwrite If TRUE, retrieves all geocodes, even those already
#' retrieved. If FALSE (default), overwrites only NAs.
#' 
#' @return An updated \code{epiflows} object.
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
#' ef <- epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]])
#' 
#' # generating random coordinates for demonstration
#' dummy_coords <- data.frame(lon = runif(41, min = -95, max = 95),
#'                            lat = runif(41, min = -95, max = 95))
#' print(ef)
#' print(flows <- add_coordinates(ef, coords = dummy_coords))
#' get_coords(flows)
#' 
#' \dontrun{
#'   # You can use google maps' geocode functionality if you have a decent 
#'   # internet connection
#'   flows <- add_coordinates(ef, loc_column = "country")
#' }
#' 
#' @export
add_coordinates <- function(ef, coords = c("lon", "lat"), loc_column = "id",
                            overwrite = FALSE) {
  if (!inherits(ef, "epiflows")) {
    efprint <- as.character(deparse(substitute(ef)))
    stop(sprintf("%s must be an object of class epiflows", efprint))
  }
  if (!is.null(ef$vars$coords) && !overwrite) {
    stop("coordinates are present in the object. Use `overwrite = TRUE` to replace them.")
  }
  if (is.data.frame(coords)) {
    if (ncol(coords) != 2) {
      stop("The data frame `coords` should contain exactly two columns specifying the longitude and latitude coordinates")
    }
    if (all(c("lon", "lat") %in% tolower(names(coords)))) {
      # Ensuring they are in the correct order if they are named lon/lat
      # otherwise, we just assume they are.
      names(coords) <- tolower(names(coords))
      coords <- coords[c("lon", "lat")]
    }
    if (overwrite && !is.null(ef$vars$coords)) {
      names(coords)               <- ef$vars$coords
      ef$linelist[ef$vars$coords] <- coords
    } else {
      ef$linelist    <- cbind(get_locations(ef), coords) 
      ef$vars$coords <- setNames(names(coords), c("lon", "lat"))
    }
  } else {
    if (!is.character(coords) || length(coords) != 2) {
      stop("`coords` should contain exactly two character strings")
    }
    ef$vars$coords <- setNames(coords, c("lon", "lat"))
    if (!loc_column %in% names(get_locations(ef))) {
      stop(sprintf("`%s` is not a valid column name", loc_column))
    }
    the_locations <- as.character(get_vars(ef, loc_column)[[1]])
    if (!overwrite && all(coords %in% names(get_locations(ef)))) {
      # If overwrite == FALSE and lon/lat columns already exist,
      # overwrite only rows with NA lon and lat
      which_rows <- complete.cases(get_coords(ef))
      ef$linelist[which_rows, coords] <- ggmap::geocode(the_locations[which_rows])
    } else {
      # Otherwise, get all geocodes and write them to lon/lat columns
      ef$linelist[, coords] <- ggmap::geocode(as.character(the_locations))
    }
  }
  ef
}
