#' Method mean
#' 
#' Calculates the arithmetic mean. Can be used directly on an object or with
#' apply, etc. When used on an \code{FLQuant} object that may include multiple
#' iterations along the \code{iter} dimension, an object of class
#' \code{numeric} with length=1 will be returned. However, when used on an
#' \code{FLPar} object that may include multiple iterations along the
#' \code{iter} dimension, an object of the same class with length 1 along the
#' \code{iter} dimension is returned.
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
#' # On an FLQuant object...
#'   data(ple4)
#'   mean(catch.n(ple4))
#'
#' # ...or on an FLPar object with iters
#'   flp <- FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#'   mean(flp)
#' 