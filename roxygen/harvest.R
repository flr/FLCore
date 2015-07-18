#' Method harvest
#'
#' Harvest calculations for FLBiol
#' 
#' Calculates the fishing mortality (F), based on abundance changes by year and
#' age, and the difference between total mortality (Z) and natural mortality
#' (M), for an object of class \code{FLBiol}. The \code{harvest} calculation
#' assumes a dynamic plus group and uses the exponential decay equation,
#' setting the F at the plusgroup equal to the F at the oldest true age;
#' this means that the Fs at the plusgroup and oldest true age will not match
#' those from a method (e.g. XSA) that assumes a hanging plusgroup, and could
#' sometimes even be negative (as in the example).
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