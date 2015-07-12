#' Method coerce
#' 
#' Coercion methods for various sets of classes are generated using the
#' \code{\link[methods]{coerce}} function. Users should call the corresponding
#' generated \code{as()} method, with arguments equal to the object to coerce
#' and the name of the class to convert to.
#' 
#' Coercion combinations work by transferring or transforming relevant slots
#' from the original object and placing them in a new object of the target
#' class. The descriptions below document how slots for each pair of classes
#' are transferred or transformed. Where relevant, the \code{name} and
#' \code{desc} slots are simply copied across.
#'
#' @name coerce
#' @aliases coerce,FLlst,list-method coerce,NULL,FLStock-method
#' coerce,NULL,FLIndex-method coerce,NULL,FLBiol-method
#' coerce,NULL,FLQuant-method coerce,FLPar,numeric-method
#' coerce,FLPar,list-method coerce,FLPar,FLQuant-method
#' coerce,FLQuant,FLPar-method coerce,FLCohort,FLQuant-method
#' coerce,FLQuant,FLCohort-method coerce,FLBiol,FLStock-method
#' coerce,FLBiol,FLIndex-method coerce,FLStock,FLBiol-method
#' coerce,FLStock,FLIndex-method coerce,FLComp,FLQuants-method
#' coerce,data.frame,FLComp-method coerce,data.frame,FLQuant-method
#' coerce,data.frame,FLStock-method coerce,data.frame,FLIndex-method
#' @docType methods
#' @section Generic function: coerce(from, to, strict)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' flb <- as(ple4, 'FLBiol')
#' flc <- as(catch.n(ple4),'FLCohort')
#'