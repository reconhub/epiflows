#' Map flows of people between locations
#'
#' The function `map_epiflows` uses `leaflet` to generate an interactive map
#' displaying flows of people travelling between locations stored in a
#' `epiflows` object. Note that the object needs to possess geographic
#' coordinates.
#'
#' @param x An \code{epiflows} object.
#'
#' @param title Plot title.
#'
#' @param ... Additional parameters (not used).
#'
#' @return A \code{leaflet} object
#'
#' @author Paula Moraga, Pawel Piatkowski, Salla Toikkanen, Zhian Kamvar
#'
#' @export
#'
#' @md
#'
#' @examples
#' data(Mex_travel_2009)
#' flows <- Mex_travel_2009[[1]]
#' to <- structure(flows[["MEX"]], names = rownames(flows))
#' from <- unlist(flows["MEX", ])
#' ef <- epiflows(from, to, "MEX", Mex_travel_2009[[2]])
#' plot(ef, type = "map", origin = "MEX")
#'
#'
#' from2 <-  structure(round(pmax(0, rnorm(5, 2500, 600))), names = LETTERS[1:5])
#' to2 <- structure(round(pmax(0,rnorm(5, 5000, 400 ))), names = LETTERS[1:5])
#' my_locationsdata <-  data.frame(code = LETTERS[1:5], country = letters[1:5], population = pmax(100, rnorm(5, 5000000, 64000)))
#' testepi <-  epiflows(to2, from2, "A", my_locationsdata)
#' testef <- add_coordinates(testepi, lon_lat_columns = data.frame(lon = rnorm(5), lat = rnorm(5)))
#' plot(testef, type = "foo", origin = "A") ## TBD


map_epiflows <- function(x, ...) {

  xy <- get_coords(x)

  if (is.null(xy)) {
    msg <- paste("epiflows object 'x' does not possess spatial coordinates",
                 "consider adding these using 'add_coordinates()'",
                 sep = "\n")
    stop(msg)
  }


  ## Data
  flow_data <- get_flows(x)$from
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

  graph <- leaflet::leaflet(data = sldf)
  graph <- leaflet::setView(graph,
      lng = origin_data[,lon_lat_columns[1]],
      lat = origin_data[,lon_lat_columns[2]],
      zoom = 2
    )
  graph <- leaflet::addTiles(graph, 
      urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    )
  graph <- leaflet::addPolylines(graph,
      data = sldf,
      color =  ~pal(count),
      highlightOptions = leaflet::highlightOptions(color = "black", weight = 2),
      label = labels
    )
  graph <- leaflet::addLegend(graph,
      "bottomright",
      pal = pal,
      values = ~count,
      title = title,
      labFormat = function(type, cuts, p) {
        n = length(cuts)
        paste0(cuts[-n], " &ndash; ", cuts[-1])
      }
    )


  graph

}
