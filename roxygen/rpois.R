#' Method rpois
#' 
#' Generates random numbers following a Poisson distribution. \emph{lambda},
#' the (non-negative) mean can be specified as an object of class
#' \code{\linkS4class{FLQuant}}.
#' 
#' 
#' @name rpois
#' @aliases rpois,numeric,FLQuant-method rpois,numeric,FLQuant-method
#' @docType methods
#' @section Generic function: rpois(n, lambda)
#' @author The FLR Team
#' @seealso \link{rpois}, \link{FLQuant}
#' @keywords methods
#' @examples
#' 
#'    data(ple4)
#'    rpois(50,lambda=harvest(ple4))
#' 