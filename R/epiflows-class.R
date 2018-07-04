#' Read linelist and contact data
#'
#' This function reads data stored as data.frame containing linelist (case
#' information, where each row corresponds to a unique patient), and contacts
#' between patients. Common identifiers should be used in the two data sources
#' for matching to be achieved.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param linelist a \link{data.frame} with at least one column providing unique
#'     patient identifiers
#'
#' @param contacts a \link{data.frame} that needs at least two columns
#'     indicating patients between which cases take place; these need not be
#'     referenced in the linelist
#'
#' @param id an index or name indicating which column in \code{linelist}
#'     contains unique identifiers; default is first column in \code{linelist}
#'     data frame
#'
#' @param from an index or name indicating which column in \code{contacts}
#'     contains the first region
#'
#' @param to an index or name indicating which column in \code{contacts}
#'     contains the second region
#'
#' @param n an index or name indicating which column in \code{contacts}
#'     contains the number of cases flowing between the "from" region and the
#'     "to" region
#'
#' @return An \code{epicontacts} object in list format with three elements:
#'
#' \itemize{
#' \item \code{linelist}: data.frame of cases with first column 'id'
#' containing character vector of unique identifiers
#'
#' \item \code{contacts}: data.frame of contacts with first two columns named
#' 'from' and 'to' indicating unique pairs of contact between individuals
#'
#' \item \code{directed}: indicator as to whether or not the contacts are to be
#' considered directed or not
#' }
#'
#' @details
#'
#' An \code{epicontacts} object can be created from two components:
#' \itemize{
#' \item a linelist provided as a \code{data.frame} where columns are
#' different variables describing cases, and where each row is a different case.
#' and a contact list.
#'
#' \item a contact list provided as a \code{data.frame} where each row contains
#' unique pairs of contacts with unique features of contact in columns. The line
#' list and contact list should share an identification scheme for individuals.
#' }
#'
#' @references
#'     \url{http://foodborne.unl.edu/public/role/epidemiologist/lineLists.html}
#'
#' @examples
#' data(YF_Brazil)
#' from <- as.data.frame.table(YF_Brazil$T_D)
#' to   <- as.data.frame.table(YF_Brazil$T_O)[c(2,1,3)]
#' contacts <- rbind(setNames(from, c("from", "to", "n")), 
#'                   setNames(to, c("from", "to", "n")))
#' linelist <- YF_Brazil$states
#' others <- setdiff(contacts$to, YF_Brazil$states$location_code)
#' linelist <- merge(linelist, data.frame(location_code = others),
#'                   by = "location_code", all = TRUE)
#' epiflows(linelist, contacts, context = "Minas Gerais")
#' @importClassesFrom epicontacts
#' @importFrom epicontacts make_epicontacts
epiflows <- function(linelist, contacts, id = 1L, xy = NULL, from = 1L, to = 2L, n = 3L, context = NULL){
  out <- epicontacts::make_epicontacts(linelist = linelist, 
                                       contacts = contacts, 
                                       id       = id,
                                       from     = from, 
                                       to       = to,
                                       directed = TRUE)
  names(out$contacts[n])  <- "n"
  if (!is.null(xy)) {
    names(out$linelist[xy]) <- c("x", "y")
  }
  if (!is.null(context)) {
    if (!is.character(context)) {
      stop("context must be a character")
    }
    if (length(context) != 1L) {
      stop("context must be a single character string")
    }
    out$context <- context
  } else {
    out$context <- NA_character_
  }
  class(out)  <- c("epiflw", class(out))
  out
}

print.epiflw <- function(x, ...) {
  cat("\n/// Epidemiological Flows //\n")
  cat("\n  // class:", paste(class(x), collapse = ", "))
  cat("\n  //", format(nrow(x$linelist), big.mark = ","), "regions;", 
      format(nrow(x$contacts), big.mark = ","), "contacts; directed")
  if (!is.na(x$context)){
    cat("\n  // context:", x$context ,"\n\n")
  }
  cat("\n  // regions\n\n")
  print(dplyr::tbl_df(x$linelist))
  cat("\n  // contacts\n\n")
  print(dplyr::tbl_df(x$contacts))
  cat("\n")
}