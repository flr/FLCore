#' Methods cv
#'
#' Coefficient of Variation of FLR objects that may include multiple iterations
#' 
#' The Coefficient of Variation of an \code{FLQuant} object that may include
#' multiple iterations along the sixth (\code{iter}) dimension can be calculated using
#' \code{cv()}. An object of class \code{numeric} with length=1 will be
#' returned.
#'
#' \code{cv(x)} is calculated as \eqn{\frac{sd(x)}{\hat{x}}}{sd(x)/mean(x)}.
#' 
#' Alternatively, to preserve all but the sixth dimension, \code{itercv} can be
#' used instead on objects of classes \code{FLQuant} or code{FLPar}, which
#' returns an object of the same class but with length=1 in the \code{iter}
#' dimension.
#'
#' \code{itercv(x)} is calculated as \eqn{\frac{iterVars(x)}{\hat{x}}}
#' {iterVars(x)/iterMeans(x)}.
#'
#' @name cv
#' @aliases cv cv-methods cv,FLQuant-method cv,FLModel-method
#' @docType methods
#' @section Generic function: cv(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#'
#' # On an FLQuant with iters...
#'   flq <- FLQuant(rnorm(5000, 5, 10), dim=c(5,10), iter=100)
#'   cv(flq)
#' # ...on an existing FLQuant with no iters
#'   data(ple4)
#'   cv(catch.n(ple4))
#'