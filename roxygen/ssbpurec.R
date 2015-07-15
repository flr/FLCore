#' Method ssbpurec
#' 
#' Calculates the spawning stock biomass per unit recruit at zero fishing
#' mortality for an \code{FLStock} object.
#' 
#' @name ssbpurec
#' @aliases ssbpurec ssbpurec-methods ssbpurec,FLStock-method
#' @docType methods
#' @section Generic function: ssbpurec(object)
#' @author The FLR Team
#' @seealso \linkS4class{FLStock}
#' @keywords methods
#' @examples
#'
#' data(ple4)
#' ssbpurec(ple4)
#' ssbpurec(ple4, start=1980, end=2000)
#' 