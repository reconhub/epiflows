#' Visualise epidemic flows using visNetwork
#'
#' This function shows flows between locations using a dynamic network
#' visualisation implemented in the package \pkg{visNetwork}, calling the
#' function [epicontacts::vis_epicontacts()] of the \pkg{epicontacts}
#' package.
#'
#' @export
#' @md
#'
#' @author Thibaut Jombart
#'
#' @param x An `epiflows` object.
#'
#' @param ... Further arguments to be passed on to
#'   [epicontacts::vis_epicontacts()]
#'

vis_epiflows <- function(x, ...) {

  epicontacts::vis_epicontacts(x, ...)
  
}

