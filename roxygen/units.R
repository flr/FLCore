#' units attribute for FLQuant objects
#' 
#' Objects of class \code{\link{FLQuant}} contain an \code{units} attribute of
#' class \code{character}. This should be used to store the corresponding units
#' of measurement.  This attribute can be directly accessed and modified using
#' the \code{units} and \code{units<-} methods.
#' 
#' For complex objects, \code{units} will return a named list containing the
#' attributes of all \code{FLQuant} slots. \code{units} of a complex object can
#' be modified for all slots or a subset of them, by passing a named list with
#' the new values. See examples below.
#' 
#' 
#' @name units
#' @aliases units,FLArray-method units,FLComp-method units,FLPar-method
#' units,FLCohort-method units<-,FLArray,character-method
#' units<-,FLComp,list-method units<-,FLPar,character-method
#' units<-,FLCohort,character-method
#' @docType methods
#' @section Generic function: units(x)
#' 
#' units<-(x,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLPar}, \linkS4class{FLCohort}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(100), dim=c(5,20), units='kg')
#' units(flq)
#' units(flq) <- 't'
#' summary(flq)
#' 
#' # units for a complex object
#' data(ple4)
#' units(ple4)
#' units(ple4) <- list(harvest='hr')
#' 