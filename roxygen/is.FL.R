#' Methods to determine the class of a given object
#' 
#' These methods return \code{TRUE} if the given object is of the corresponding
#' class, and \code{FALSE} otherwise.
#' 
#' These methods should be substituted by calls to \code{\link[methods]{is}}
#' and will very likely be deprecated in future releases.
#' 
#' 
#' @name is.FL
#' @aliases is.FLBiol is.FLQuant is.FLStock is.FLQuants is.FLQuants-methods
#' is.FLQuants,ANY-method is.FLBiols is.FLBiols-methods is.FLBiols,ANY-method
#' is.FLIndices is.FLIndices-methods is.FLIndices,ANY-method is.FLStocks
#' is.FLStocks-methods is.FLStocks,ANY-method
#' @docType methods
#' @section Generic function: is.FLQuants(object)
#' 
#' is.FLBiols(object)
#' 
#' is.FLIndices(object)
#' 
#' is.FLStocks(object)
#' @author The FLR Team
#' @seealso \link[methods]{is}
#' @keywords methods
#' @examples
#' 
#' # This call ...
#' is.FLQuant(FLQuant())
#' # ... should be substituted by
#' is(FLQuant(), 'FLQuant')
#' 