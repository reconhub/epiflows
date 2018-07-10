#' Visualise epidemic flows using a grid
#'
#' This grid plot shows flows between locations by positioning origins and
#' destination in y and x axes, respectively. This function is called when
#' plotting \code{epiflows} with \code{type = "grid"}.
#'
#' @export
#' @md
#'
#' @author Thibaut Jombart
#'
#' @param x An `epiflows` object.
#'
#' @param color_by A character string indicating if flows should be colored by
#'   origin (`from`) or destination (`to`).
#' 
#' @param ... arguments passed on to [ggplot2::geom_point()]
#'

grid_epiflows <- function(x, color_by = c("from", "to", "none"), ...) {
  color_by <- match.arg(color_by)
  if (color_by == "none") {
    color_by <- NULL
  }

  ## get_flows returns a 3 column data.frame: from, to, flows (
  input <- get_flows(x)

  ggplot2::ggplot(
    input, ggplot2::aes_string(y = "from", x = "to")) +
    ggplot2::geom_point(
      ggplot2::aes_string(size = "n", color = color_by), ...) +
    ggplot2::scale_color_discrete(guide = FALSE) +
    ggplot2::scale_size("Flow") +
    ggplot2::labs(y = "Origin", x = "Destination") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

