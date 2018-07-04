#' Visualise epidemic flows using a grid
#'
#' This grid plot shows flows between locations by positioning origins and
#' destination in y and x axes, respectively. This function is called when
#' plotting \code{epiflows} with \code{type = "grid"}.
#'
#' @export
#'
#' @author Thibaut Jombart
#'
#' @param x An \code{epiflows} object.
#'
## #' @param all_locations A logical indicating if all locations, even though
## #'   without inflows or outflows, should be used; defaults to \code{TRUE}.
## #'

grid_epiflows <- function(x, ...) {

  ## get_flows returns a 3 column data.frame: from, to, flows (
  input <- get_flows(x)

  ggplot2::ggplot(input, ggplot2::aes_string(y = "from", x = "to")) +
    ggplot2::geom_point(ggplot2::aes_string(size = "n"), ...)
}

