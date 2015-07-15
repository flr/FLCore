#' Method ssb
#' 
#' Returns the spawning stock biomass of \code{FLStock} and \code{FLBiol}
#' objects.
#' 
#' If spawning occurs at the beginning of the year, the calculated SSB is the
#' same regardless of the units of the harvest slot. If spawning occurs at any
#' other time during the year such that the stock is subject to fishing
#' mortality prior to spawning, then the calculated SSB will depend on the units
#' of the harvest slot (either 'f' or 'hr').
#'
#' For an \code{FLStock} with harvest units 'f', SSB is calculated as
#'
#'    \eqn(SSB = sum(N*exp(-F*propF-M*propM) * wt * mat)}
#'
#' For an \code{FLStock} with harvest units 'hr', SSB is calculated as
#'
#'		\eqn{SSB = sum(N*(1-harvest*propF)*exp(-M*propM) * wt * mat)}
#'
#' The units of the harvest slot in the \code{FLStock} object must be specified
#' as either 'f' for an instantaneous fishing mortality or else as 'hr' for a
#' harvest rate.
#'
#' For an \code{FLBiol} the spawning biomass at the beginning of the year is
#' calculated as
#'
#'  	\eqn{SSB = sum(N * wt * mat)}
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
#' data(ple4)
#'
#' # check the units of the harvest slot
#'   units(harvest(ple4))
#'
#' ssb(ple4)
#'
