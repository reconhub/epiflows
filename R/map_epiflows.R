#' Plot a flow map
#'
#' Returns a graph (XX / YY or an interactive map) of population flows from the specified
#' location of origin.
#'
#' @param x An \code{epiflows} object.
#' @param origin Code of the location of origin.
#' @param title Plot title.
#' @param type The type of plot wanted to be drawn: map/visNetwork/grid.
#' (default: ??)
#' @param loc_column Name of the column where location names are stored
#' (default: "country").
#' @param lon_lat_columns Names of the columns with longitudes and latitudes,
#' respectively (default: "lon" and "lat").
#' @param ... Additional parameters (not used).
#'
#' @return A \code{leaflet} object
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @author Paula Moraga, Pawel Piatkowski, Salla Toikkanen
#'

#' flows <- Mex_travel_2009[[1]]
#' to <- structure(flows[["MEX"]], names = rownames(flows))
#' from <- unlist(flows["MEX", ])
#' ef <- make_epiflows( to = to, from = from, code = "MEX", locationsdata = Mex_travel_2009[[2]])
#' plot(ef, type = "map", origin = "MEX")
#'
#'
#' from2 <-  structure(round(pmax(0, rnorm(5, 2500, 600))), names = LETTERS[1:5])
#' to2 <- structure(round(pmax(0,rnorm(5, 5000, 400 ))), names = LETTERS[1:5])
#' my_locationsdata <-  data.frame(code = LETTERS[1:5], country = letters[1:5], population = pmax(100, rnorm(5, 5000000, 64000)))
#' testepi <-  make_epiflows(from = from2, to = to2, code = "A", locationsdata = locationsdata2)
#' testef <- add_coordinates(testepi, lon_lat_columns = data.frame(lon = rnorm(5), lat = rnorm(5)))
#' plot(testef, type = "foo", origin = "A") ## TBD



#' @export
map_epiflows <- function(x,
                          type = "foo",
                          origin,
                          title = sprintf("Flows from %s", origin_name),
                          loc_column = "country",
                          lon_lat_columns = c("lon", "lat"),
                          ...) {
  # Add coordinates if needed
  location_cols <- names(x$locationsdata)
  if (!loc_column %in% location_cols) {
    stop("`%s` not found in x$locationsdata")
  }

  if (!all(lon_lat_columns %in% location_cols)) {
    ## Tries to find the coordinates with add_coordinates(), should only do this if type == "map"
    if(type=="map"){
     x %<>%
      add_coordinates(
        loc_column = loc_column,
        lon_lat_columns = lon_lat_columns
      )
    }
    else(stop("lon_lat_columns not specified in epiflows object"))
  }

  if (any(is.na(x$locationsdata[, lon_lat_columns]))) {
    stop(
      "NA values present in location coordinates. ",
      "Please rerun `add_coordinates()` before plotting"
    )
  }

  ## Data
  flow_data <- get_flow_data(x, direction = "from")
  loc_codes <- names(flow_data)
  flow_df <- data.frame(code = loc_codes, count = flow_data)
  location_df <- get_location_data(x, loc_codes)
  origin_data <- location_df[location_df$code == origin, ]
  origin_name <- origin_data[[loc_column]]
  print(locs_with_flows <- merge(
    location_df,
    flow_df,
    by = "code"
  ))
if(type != "map"){
  ## add here the other types of plots
  require(ggplot2)
  print(lon_lat_columns)
  graph <-  ggplot() + geom_point(aes( x = locs_with_flows[, lon_lat_columns[1]], y =locs_with_flows[, lon_lat_columns[2]] ))
  cat("Work in progress")
}
  if(type == "map") {
    listlines <- apply(
      locs_with_flows,
    1,
    function(loc, origin_data) {
      connection <- geosphere::gcIntermediate(
        as.numeric(origin_data[lon_lat_columns]),
        as.numeric(loc[lon_lat_columns]),
        n = 100,
        addStartEnd = TRUE,
        sp = TRUE,
        breakAtDateLine = TRUE
      )
      sp::Lines(connection@lines[[1]]@Lines, ID = loc["code"])
    },
    origin_data = origin_data
  )

    sl <- sp::SpatialLines(listlines)
    sldf <- sp::SpatialLinesDataFrame(sl, data.frame(count = flow_data))

  ## Plot
  pal <- leaflet::colorQuantile(palette = "YlGnBu", domain = sldf$count, n = 5)
  labels <- lapply(
    sprintf(
      "%s to %s: %s",
      origin_name,
      locs_with_flows[, loc_column],
      locs_with_flows[, "count"]
    ),
    htmltools::HTML
  )

  graph <- leaflet::leaflet(data = sldf) %>%
    leaflet::setView(
      lng = origin_data[,lon_lat_columns[1]],
      lat = origin_data[,lon_lat_columns[2]],
      zoom = 2
    ) %>%
    leaflet::addTiles(
      urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    ) %>%
    leaflet::addPolylines(
      data = sldf,
      color =  ~pal(count),
      highlightOptions = leaflet::highlightOptions(color = "black", weight = 2),
      label = labels
    ) %>%
    leaflet::addLegend(
      "bottomright",
      pal = pal,
      values = ~count,
      title = title,
      labFormat = function(type, cuts, p) {
        n = length(cuts)
        paste0(cuts[-n], " &ndash; ", cuts[-1])
      }
    )
}

  graph

}
