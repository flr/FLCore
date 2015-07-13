#' Method jacknife
#'
#' Jacknife resampling
#' 
#' The \code{jacknife} method sets up objects ready for jacknifing, i.e. to
#' systematically recompute a given statistic leaving out one observation at a
#' time. From this new set of "observations" for the statistic, estimates for
#' the bias and variance of the statstic can be calculated.
#' 
#' Input objects cannot have length > 1 along the \code{iter} dimension, and
#' the resulting object will have one more \code{iter} than the number of
#' elements in the original object.
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
#' flj <- jacknife(flq)
#' iters(flj)
#'
#' # Calculate the bias of the mean and variance estimators
#' (mean(iter(yearMeans(flj),2:9))-c(iter(yearMeans(flj),1)))*7
#' (mean(iter(yearVars(flj),2:9))-c(iter(yearVars(flj),1)))*7
#'