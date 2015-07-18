#' Method lowess
#' 
#' LOWESS smoother based on locally-weighted polynomial regression for objects
#' of class \code{\linkS4class{FLSR}}. The model fitted is of the form
#' \code{rec(x)~ssb(x)}, and returns an object of class \code{FLQuants} with
#' elements named \code{ssb} and \code{rec}.
#'
#' @name lowess
#' @aliases lowess,FLSR,missing-method
#' @docType methods
#' @section Generic function: lowess(x, y, f=2/3, iter=3, delta=0.01 *
#' diff(range(xy\$x[o])))
#' @author The FLR Team
#' @seealso \link[stats]{lowess}, \code{\linkS4class{FLSR}}
#' @keywords methods
#' @examples
#' 
#' # use the North Sea herring SR dataset
#'   data(nsher)
#' 
#' # fitting a rec ~ ssb lowess
#'   nshlos <- lowess(nsher)
#'   plot(rec(nsher)~ssb(nsher))
#'   points(nshlos$rec~nshlos$ssb,pch=19)
#'