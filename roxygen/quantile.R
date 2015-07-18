#' Method quantile
#' 
#' Quantiles for \code{\linkS4class{FLQuant}} objects can be obtained with this
#' method.  Default quantiles returned are \code{seq(0, 1, 0.25)}, but they can
#' be specified using the \code{probs} argument. The returned
#' \code{\linkS4class{FLQuant}} object uses the sixth dimension (\emph{iter})
#' to store the requested quantiles, with appropriate dimnames.
#' 
#' For objects of class \code{\linkS4class{FLQuantPoint}}, quantile is merely
#' an accessor for two elements of the sixth dimension, \code{lowq} and
#' \code{uppq}. You could use the \code{\link{lowq}} and \code{\link{uppq}}
#' methods instead.
#'
#' @name quantile
#' @aliases quantile,FLQuant-method quantile,FLQuantPoint-method
#' @docType methods
#' @section Generic function: quantile(x, ...)
#' @author The FLR Team
#' @seealso \link[stats]{quantile}, \linkS4class{FLQuant},
#' \linkS4class{FLQuantPoint}
#' @keywords methods
#' @examples
#' 
#' # Normally distributed FLQuant with lognormal random mean and fixed sd of 20
#'   flq <- rnorm(100, FLQuant(rlnorm(20), dim=c(2,10)), 20)
#' 
#' # Obtain all standard quantiles (0, 0.25, 0.5, 0.75 and 1)...
#'   quantile(flq)
#'   dimnames(quantile(flq))$iter
#' # ...and select one of them
#'   quantile(flq)[,,,,,1]
#'
#' # Calculate the 0.05 quantile only
#'   quantile(flq, 0.05)
#' 
#' # Create an FLQuantPoint from a previous FLQuant...
#'   flp <- FLQuantPoint(flq)
#' # ...and return each of the two quantiles (025 and 0.75)...
#'   quantile(flp, 0.25)
#'   quantile(flp, 0.75)
#' # ...or alternatively use lowq and uppq
#'   lowq(flp)
#'   uppq(flp)
#' 