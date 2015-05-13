#' Harvest calculations for FLBiol
#' 
#' Calculates the fishing mortality (F), based on abundance changes by year and
#' age, and the difference between total mortality (Z) and natural mortality
#' (M), for an object of class \code{FLBiol}.
#' 
#' 
#' @name harvest
#' @aliases harvest,FLBiol-method harvest,FLBiol,missing-method
#' @docType methods
#' @section Generic function: harvest(object)
#' @author The FLR Team
#' @seealso \linkS4class{FLComp}, \linkS4class{FLBiol}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' flb <- as(ple4, 'FLBiol')
#' harvest(flb)
#' 