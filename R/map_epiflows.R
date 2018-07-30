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
#' data("Brazil_epiflows")
#' data("YF_coordinates")
#' ef <- add_coordinates(Brazil_epiflows, YF_coordinates[-1])
#' plot(ef)
#' map_epiflows(ef, center = "Espirito Santo", title = "Flows to and from Brazil")
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
  
  if (is.character(center) && length(center) == 1) {
    center <- get_coordinates(x, center)
  }
  if (!is.null(center) && (!is.numeric(center) || length(center) != 2)) {
    stop("center must be a single character string to use for ID lookup or a set of coordinates")
  }
  if (!is.null(center) && all(is.finite(center))) {
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