#' Method expand
#'
#' Expand FLR objects using named dimensions
#' 
#' Expansion of FLR objects can be carried out with dimension names by using
#' \code{expand}. A number of dimension names and selected dimensions are
#' passed to the method, and those are used to expand the input object. The
#' method inserts \code{NA} into the newly-expanded areas. For the method to
#' work, the new dimensions need to include existing ones; although intuitive
#' for the \code{quant}, \code{year}, and \code{iter} dimensions, the
#' \code{unit}, \code{season} and \code{area} dimensions need more careful
#' treatment (see examples).
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
#'
#' # Expanding FLStock objects
#'   expand(ple4, year=1957:2013)
#'   expand(ple4, iter=1:10)
#'
#' # Expanding along the season dimension for an FLQuant object
#'   flq<-expand(stock.n(ple4), season=c("all",2))
#'   dimnames(flq)$season[1]<-1
#'   dimnames(flq)
#'