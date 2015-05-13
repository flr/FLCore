#' Method ssb
#' 
#' Returns the Spawning Stock Biomass of \code{FLStock} and \code{FLBiol}
#' objects.
#' 
#' For \code{\linkS4class{FLStock}} objects the nature of the calculation
#' depends on the units in the harvest slot. See details below.
#' 
#' 
#' @name ssb
#' @aliases ssb ssb-methods ssb,FLStock-method ssb,FLBiol-method
#' @docType methods
#' @section Generic function: ssb(object)
#' @author The FLR Team
#' @seealso \linkS4class{FLBiol} \linkS4class{FLStock}
#' @keywords methods
#' @examples
#' 
#'  data(ple4)
#'  units(harvest(ple4)) # check the units of the harvest slot
#'  ssb(ple4)
#' 