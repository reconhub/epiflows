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
#' @param center An optional set of coordinates to use as the center of the map
#' 
#' @param sort a logical. When \code{TRUE} (default), the flows will be sorted
#'   in order of number of cases on the map so that the largest flows appear on
#'   top.
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
#' ef      <- epiflows(inflow, outflow, focus = "MEX", locations = Mex_travel_2009[[2]])
#' ef      <- add_coordinates(ef, loc[-1])
#' map_epiflows(ef)
map_epiflows <- function(x, title = "", center = NULL, sort = TRUE, ...) {

  # First thing to do is to calculate the great circle arc for the flows with
  # the make_lines internal function.
  the_flows <- get_flows(ef)
  if (sort) {
    the_flows <- the_flows[order(the_flows$n), ]
  }
  res <- apply(X      = the_flows,
               MARGIN = 1L,
               FUN    = make_lines,
               coords = get_coords(ef)
              )
  # res here may be named due to the apply function. This causes leaflet to bork
  # when attempting to display the lines on the map. To avoid this, we are
  # simply unnaming the elemements before passing them to SpatialLines
  SL <- sp::SpatialLines(unname(res))

  # Now we can create the SpatialLinesDataFrame class that leaflet will use
  # to perform the plotting.
  rownames(the_flows) <- names(SL)
  sldf                <- sp::SpatialLinesDataFrame(SL, the_flows)

  pal    <- leaflet::colorQuantile(palette = "Greys",
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
  # MX_coords <- get_coords(ef, "MEX")
  # graph <- leaflet::setView(graph,
  #                           lng = MX_coords[[1]],
  #                           lat = MX_coords[[2]],
  #                           zoom = 2
  #                          )
  urltemplate <- "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
  graph <- leaflet::addTiles(graph, urlTemplate = urltemplate)

  lhighlight <- leaflet::highlightOptions(color = "black", weight = 2)
  graph <- leaflet::addPolylines(graph,
                                 color            = ~pal(n),
                                 data             = sldf,
                                 highlightOptions = lhighlight,
                                 label            = labels
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

make_lines <- function(df, coords) {
  from <- df[["from"]]
  to   <- df[["to"]]
  cds  <- coords[match(c(from, to), coords$id), 2:3]
  connection <- geosphere::gcIntermediate(
    as.numeric(cds[1L, ]),
    as.numeric(cds[2L, ]),
    n = 100,
    sp = TRUE,
    addStartEnd = TRUE,
    breakAtDateLine = TRUE
  )
  ID <- paste(from, to, sep = ":")
  sp::Lines(connection@lines[[1]]@Lines, ID = ID)
}
