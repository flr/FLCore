#' Jacknife resampling
#' 
#' The \code{jacknife} method sets up objects ready for jacknifing, i.e. to
#' systematically recompute a given statistic leaving out one observation at a
#' time. From this new set of "observations" for the statistic an estimate for
#' the bias can be calculated as well as an estimate for the variance of the
#' statistic.
#' 
#' Input objects cannot have length > 1 along the \code{iter} dimension, and
#' the resulting object will have as many \code{iterations} as elements in the
#' original object.
#' 
#' 
#' @name jacknife
#' @aliases jacknife jacknife-methods jacknife,FLQuant-method
#' @docType methods
#' @section Generic function: jacknife(object, ...)
#' @author The FLR Team
#' @seealso \code{\link{FLQuant}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(1:8)
#' iters(jacknife(flq))