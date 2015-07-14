#' Method print
#' 
#' \code{print} prints its argument and returns it invisibly (via
#' \link[base]{invisible}(x)).
#'
#' @name print
#' @aliases print,FLQuant-method
#' @docType methods
#' @section Generic function: print(x)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' a <- FLQuant(1:6, dim = c(2,3))
#' for(i in 1:3) print(a[,1:i])
#' for(i in 1:3) a[,1:i]
#'
#' a <- print(FLPar())
#' a
#'