#' Method ssbpurec
#' 
#' Calculates the Spawning Stock Biomass per unit recruit for an \code{FLStock}
#' object.
#' 
#' The method calculates SSB per recruit at zero fishing mortality.
#' 
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
#' 
#' data(ple4)
#' ssbpurec(ple4)
#' 
#' ssbpurec(ple4, start=1980, end=2000)
#' 