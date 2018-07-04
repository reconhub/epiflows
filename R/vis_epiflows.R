#' Visualise epidemic flows using visNetwork
#'
#' This function shows flows between locations using a dynamic network
#' visualisation implemented in the package \code{visNetwork}, calling the
#' function \code[epicontacts]{vis_epicontacts} of the \code{epicontacts}
#' package.
#'
#' @export
#'
#' @author Thibaut Jombart
#'
#' @param x An \code{epiflows} object.
#'
#' @param ... Further arguments to be passed on to
#'   \code[epicontacts]{vis_epicontacts}
#'

vis_epiflows <- function(x, ...) {

  epicontacts::vis_epicontacts(x, ...)
  
}

