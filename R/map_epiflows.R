#' Map flows of people between locations
#'
#' The function `map_epiflows` uses `leaflet` to generate an interactive map
#' displaying flows of people travelling between locations stored in a
#' `epiflows` object. Note that the object needs to possess geographic
#' coordinates.
#'
#' @param x An `epiflows` object.
#'
#' @param title Plot title.
#' 
#' @param center An optional set of coordinates or character string specifying ID to use as the center of the map
#' 
#' @param sort a logical. When `TRUE` (default), the flows will be sorted
#'   in order of number of cases on the map so that the largest flows appear on
#'   top.
#' 
#' @param pal a color palette to pass on to [leaflet::colorQuantile()]. This
#'   can be the name of a viridis or RColorBrewer palette, a vector of hex colors,
#'   or a color-generating functon.
#' 
#' @param adjust_width a logical specifying if the width of the flows should
#'   be adjusted to reflect the number of flows between locations. Defaults to
#'   `TRUE`.
#'
#' @param ... Additional parameters (not used).
#'
#' @return A `leaflet` object
#'
#' @author Paula Moraga, Pawel Piatkowski, Salla Toikkanen, Zhian Kamvar
#'
#' @export
#'
#' @md
#'
#' @examples
#' data(Mex_travel_2009)
#' loc <- data.frame(stringsAsFactors=FALSE,
#'   id = c("ARG", "BEL", "BHS", "BLZ", "BOL", "BRA", "BRB", "CAN", "CHE",
#'          "CHL", "CHN", "COL", "CRI", "CUB", "DEU", "DOM", "ECU", "ESP",
#'          "FIN", "FRA", "GBR", "GTM", "HKG", "HND", "ITA", "JAM", "JPN",
#'          "LUX", "MEX", "MMR", "NIC", "NLD", "PAN", "PER", "PRT", "PRY",
#'          "SLV", "SWE", "TTO", "USA", "VEN"),
#'   lon = c(-63.616672, 4.469936, -77.39628, -88.49765, -63.588653,
#'           -51.92528, -59.543198, -106.346771, 8.227512, -71.542969,
#'           104.195397, -74.297333, -83.753428, -77.781167, 10.451526,
#'           -70.162651, -78.183406, -3.74922, 25.7481511, 2.213749, -3.435973,
#'           -90.230759, 114.109497, -86.241905, 12.56738, -77.297508, 138.252924,
#'           6.129583, -102.552784, 95.955974, -85.207229, 5.291266,
#'           -80.782127, -75.015152, -8.224454, -58.443832, -88.89653, 18.643501,
#'           -61.222503, -95.712891, -66.58973),
#'   lat = c(-38.416097, 50.503887, 25.03428, 17.189877, -16.290154,
#'           -14.235004, 13.193887, 56.130366, 46.818188, -35.675147,
#'           35.86166, 4.570868, 9.748917, 21.521757, 51.165691, 18.735693,
#'           -1.831239, 40.463667, 61.92411, 46.227638, 55.378051, 15.783471,
#'           22.396428, 15.199999, 41.87194, 18.109581, 36.204824, 49.815273,
#'           23.634501, 21.916221, 12.865416, 52.132633, 8.537981, -9.189967,
#'           39.399872, -23.442503, 13.794185, 60.128161, 10.691803, 37.09024,
#'           6.42375)
#'   )
#' flows   <- Mex_travel_2009[[1]]
#' outflow <- setNames(flows[["MEX"]], rownames(flows))
#' inflow  <- unlist(flows["MEX", , drop = TRUE])
#' ef      <- make_epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]])
#' ef      <- add_coordinates(ef, loc[-1])
#' map_epiflows(ef, center = "MEX", title = "Flows to and from Mexico")
map_epiflows <- function(x, title = "", center = NULL, sort = TRUE, 
                         pal = "YlOrBr", adjust_width = TRUE, ...) {

  # First thing to do is to calculate the great circle arc for the flows with
  # the make_lines internal function.
  the_flows <- get_flows(x)
  if (sort) {
    the_flows <- the_flows[order(the_flows$n), ]
  }
  the_coordinates  <- get_coordinates(x)
  
  # SpatialLinesDataFrame class construction ------------------------
  sldf <- make_SpatialLinesDataFrame(the_flows, the_coordinates)

  # Leaflet plot construction ---------------------------------------
  pal    <- leaflet::colorQuantile(palette = pal,
                                   domain = sldf$n,
                                   n = 5
                                  )
  labels <- sprintf("%s to %s: %s",
                    the_flows$from,
                    the_flows$to,
                    format(the_flows$n,
                           big.mark = ","
                         )
                   )
  labels <- lapply(labels, htmltools::HTML)
  graph  <- leaflet::leaflet(data = sldf)
  if (!is.null(center)) {
    if (is.character(center) && length(center) == 1) {
      center <- get_coordinates(x, center)
    } else if (is.numeric(center) && length(center) == 2) {
      center <- center
    } else {
      stop("center must be a single character string to use for ID lookup or a set of coordinates")
    }
    graph <- leaflet::setView(graph,
                              lng = center[[1]],
                              lat = center[[2]],
                              zoom = 2
                             )
  }
  urltemplate <- "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  graph <- leaflet::addTiles(graph, urlTemplate = urltemplate)
  the_weight <- if (adjust_width) ~log(n + 1) else 5
  lhighlight <- leaflet::highlightOptions(color = "black", weight = 2)
  graph <- leaflet::addPolylines(graph,
                                 color            = ~pal(n),
                                 data             = sldf,
                                 highlightOptions = lhighlight,
                                 label            = labels,
                                 weight           = the_weight
                                )
  graph <- leaflet::addLegend(graph,
                              "bottomright",
                              pal    = pal,
                              values = ~n,
                              title  = title,
                              labFormat = function(type, cuts, p) {
                                n = length(cuts)
                                cuts = format(cuts, big.mark = ",")
                                sprintf("%s &ndash; %s", cuts[-n], cuts[-1])
                              }
                              )

  graph


}