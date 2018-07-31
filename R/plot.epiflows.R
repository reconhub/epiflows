#' Various plots for epiflows objects
#'
#' The `plot` method for `epiflows` objects offers types of
#' graphics (argument `type`):
#'
#'   - `map`: flows are displayed on an interactive map; see [map_epiflows] for
#'   more details
#'
#'   - `network`: flows are displayed as a network using the htmlwidget
#'   `visNetwork` and the plotting method for `epicontacts` objects; see
#'   [vis_epiflows] for more details
#'
#'   - `grid`: flows are displayed as a grid between origins and destinations;
#'   see [grid_epiflows] for more details
#'
#'
#' @param x an epiflows object.
#' @param type The type of plot to produce (defaults to map).
#' @param ... arguments passed on to a given type
#' @author Salla Toikkanen, Thibaut Jombart, Zhian Kamvar
#'
#' @export
#' @md
#' 
#' @examples 
#' data("Brazil_epiflows")
#' 
#' # no coordinates, defaults to network
#' plot(Brazil_epiflows) 
#' 
#' # grid bubbleplot
#' plot(Brazil_epiflows, "grid")
#' 
#' # adding coordinates defaults to map
#' data("YF_coordinates")
#' ef <- add_coordinates(Brazil_epiflows, YF_coordinates[-1])
#' plot(ef)
plot.epiflows <- function(x, type = c("map", "network", "grid"), ...) {
  type <- match.arg(type)

  if (type == "map" && is.null(get_coordinates(x))) {
    type <- "network"
    msg <- paste("type 'map' requested but 'x' has no spatial coordinates",
                 "using 'network' instead", sep = "\n")
    message(msg)
  }
  switch(EXPR = type,
         map     =  map_epiflows(x, ...),
         network =  vis_epiflows(x, ...),
         grid    = grid_epiflows(x, ...)
        )
}
