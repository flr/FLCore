#' Method mean
#' 
#' Calculates the arithmetic mean. Can be used directly on an object or with
#' apply etc.
#' 
#' 
#' @name mean
#' @aliases mean,FLPar-method mean,FLQuant-method
#' @docType methods
#' @section Generic function: mean(x)
#' @author The FLR Team
#' @seealso \link{median} \link{apply}
#' @keywords methods
#' @examples
#' 
#' flp <- FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#' mean(flp)
#' 