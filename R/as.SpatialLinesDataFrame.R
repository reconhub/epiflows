#' Convert to SpatialLinesDataFrame class
#'
#' @param x an epiflows object 
#' @rdname as.SpatialLinesDataFrame
#'
#' @return an object of class [sp::SpatialLinesDataFrame]
#' @export
#'
#' @examples
#' data("Brazil_epiflows")
#' data("YF_coordinates")
#' ef <- add_coordinates(Brazil_epiflows, YF_coordinates[-1])
#' ef2 <- epicontacts::thin(ef[j = c("Espirito Santo", "Italy"), contacts = "both"])
#' as.SpatialLinesDataFrame(ef2)
as.SpatialLinesDataFrame <- function(x) {
  UseMethod("as.SpatialLinesDataFrame", x)
}


#' @rdname as.SpatialLinesDataFrame
#' @export
as.SpatialLinesDataFrame.epiflows <- function(x) {
  the_flows  <- get_flows(x)
  the_coordinates <- get_coordinates(x)
  if (is.null(the_coordinates)) {
    stop(sprintf("%s does not have coordinates set", deparse(substitute(x))))
  }
  make_SpatialLinesDataFrame(the_flows, the_coordinates)
}

# Internal Functions ------------------------------------------------
#' Look-up coordinates from a flows data frame
#'
#' @param df a data frame of flows with "from" and "to" columns. use get_flows()
#' @param coordinates a data frame containing coordinates and an ID column. use get_coordinates()
#' @param what a character string specifying what to lookup.
#'
#' @return a data frame with coordinates and ID
#' @noRd
#' @keywords internal
coord_lookup <- function(the_flows, the_coordinates, what = "from") {
  lookup <- the_flows[[what]]
  the_coordinates[match(lookup, the_coordinates$id), ]
}

#' use flows and coordinates to create a SpatialLinesDataFrame class object from
#' the sp package.
#'
#' @param the_flows a data frame of flows. use get_flows() 
#' @param the_coordinates a data frame of coordinates. use get_coordinates()
#'
#' @return a SpatialLinesDataFrame class object.
#' @noRd
#' @keywords internal
make_SpatialLinesDataFrame <- function(the_flows, the_coordinates) {
  # First step: create two complementary data frames using the lookup table
  from_coordinates <- coord_lookup(the_flows, the_coordinates, what = "from")
  to_coordinates   <- coord_lookup(the_flows, the_coordinates, what = "to")
  # Second step: calculate intermediate points along the earth.
  SL <- geosphere::gcIntermediate(from_coordinates[-1L],
                                  to_coordinates[-1L],
                                  n = 100L,
                                  breakAtDateLine = TRUE,
                                  addStartEnd = TRUE,
                                  sp = TRUE
  )
  # Third step: rename the lines because they get named generic numbers
  row.names(SL)        <- paste(from_coordinates$id, to_coordinates$id, sep = ":")
  row.names(the_flows) <- row.names(SL)
  # Fourth step: create the data frame object containing the counts.
  sp::SpatialLinesDataFrame(SL, the_flows)
}
