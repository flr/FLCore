#' Method lapply
#' 
#' \code{lapply} returns a list of the same length as X, each element of which
#' is the result of applying FUN to the corresponding element of X.
#'
#' @name lapply
#' @aliases lapply,FLlst,missing-method lapply,FLlst-method
#' @docType methods
#' @section Generic function: lapply(X,FUN)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # On an FLQuants object
#'   flqs <- FLQuants(a=FLQuant(1:10), b=FLQuant(1:20))
#'
#' # lapply could return another FLQuants object...
#'   lapply(flqs, yearSums)
#' # ...or a simple list, depending on the function  being called
#'   lapply(flqs, dim)
#' 