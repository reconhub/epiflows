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
#' @importFrom visNetwork visIgraphLayout

vis_epiflows <- function(x, arrows = TRUE, max_width = 10, ...) {

  edge_width <- get_flows(x)$n
  edge_width  <- max_width * edge_width / max(edge_width)
  
  # The user can change whether or not the graph is directed
  x$directed <- arrows
  
  out <- epicontacts::vis_epicontacts(x, edge_width = edge_width, ...)
  # Label the edges
  out$x$edges$title <- sprintf("<p>%s to %s: <b>%s</b></p>",
                               out$x$edges$from,
                               out$x$edges$to,
                               format(out$x$edges$n, big.mark = ","))
  out$x$edges$smooth <- TRUE
  visNetwork::visIgraphLayout(out, layout = "layout_in_circle")
}

