#' Method expand
#'
#' Expand FLR objects using named dimensions
#' 
#' Expansion of FLR objects can be carried out with dimension names by using
#' \code{expand}. A number of dimension names and selected dimensions are
#' passed to the method, and those are used to expand the input object. The
#' method inserts \code{NA} into the newly-expanded areas.
#'
#' @name expand
#' @aliases expand expand-methods expand,FLArray-method expand,FLQuant-method
#' expand,FLComp-method expand,FLStock-method
#' @docType methods
#' @section Generic function: expand(x)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' expand(ple4, year=1957:2013)
#' 