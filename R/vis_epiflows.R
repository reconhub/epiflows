#' Visualise epidemic flows using visNetwork
#'
#' This function shows flows between locations using a dynamic network
#' visualisation implemented in the package \pkg{visNetwork}, calling the
#' function [epicontacts::vis_epicontacts()] of the \pkg{epicontacts}
#' package. The thickness of arrows/edges is proportional to the corresponding
#' flows.
#'
#' @export
#' @md
#'
#' @author Thibaut Jombart
#'
#' @param x An `epiflows` object.
#'
#' @param arrows A logical indicating if arrows should be used to show
#'   directionality of the flows.
#'
#' @param max_width A single number indicating the maximum width of an edge
#'   (corresponding to the largest flow).
#'
#' @param ... Further arguments passed to [epicontacts::vis_epicontacts()]
#'
#' @importFrom epicontacts vis_epicontacts

vis_epiflows <- function(x, arrows = TRUE, max_width = 10, ...) {

  edge_width <- get_flows(x)$n
  edge_width  <- max_width * edge_width / max(edge_width)
  if (!arrows) {
    x$directed <- FALSE
  }
  out <- epicontacts::vis_epicontacts(x, edge_width = edge_width, ...)
  out
}

