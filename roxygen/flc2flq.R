#' Method flc2flq
#'
#' Coerce FLCohort into FLQuant.
#' 
#' Coerces \code{FLCohort} objects into \code{FLQuant} objects. Using
#' \code{as(flcobject,"FLQuant")} achieves the same results, and it will be
#' deprecated in the near future.
#'
#' @name flc2flq
#' @aliases flc2flq flc2flq-methods flc2flq,FLCohort-method
#' @docType methods
#' @section Generic function: flc2flq(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' flc <- FLCohort(catch.n(ple4))
#' flq <- flc2flq(flc)
#' all.equal(flq, catch.n(ple4))
#' 