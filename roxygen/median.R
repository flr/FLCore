#' Method median
#' 
#' Calculates the median.
#' 
#' 
#' @name median
#' @aliases median,FLPar,missing-method median,FLPar-method
#' @docType methods
#' @section Generic function: median(x,na.rm)
#' @author The FLR Team
#' @seealso \link[stats]{median}, \link[base]{apply}
#' @keywords methods
#' @examples
#' 
#' flp <- FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#' median(flp)
#' 