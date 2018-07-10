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
#'
#' @export
#' @md

plot.epiflows <- function(x, type = c("map", "network", "grid"), ...) {
  type <- match.arg(type)

  if (type == "map" && is.null(get_coordinates(x))) {
    type <- "network"
    msg <- paste("type 'map' requested but 'x' has no spatial coordinates",
                 "using 'network' instead", sep = "\n")
    message(msg)
  }
  if (type == "map") {
    map_epiflows(x, ...)
  }
  if (type == "network") {
    vis_epiflows(x, ...)
  }
  if (type == "grid") {
    grid_epiflows(x, ...)
  }
}
