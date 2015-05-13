#' Coefficient of Variation of FLR objects with multiple iterations
#' 
#' The Coefficient of Variation of an object with mutiple iterations along the
#' sixth (\code{iter}) dimension can be calculated using \code{cv()}. An object
#' of the same class, with length=1 on the sixth dimension, will be returned.
#' 
#' CV of \code{x} is calculated as \eqn{\frac{sd(x)}{\hat{x}}}{sd(x)/mean(x)}.
#' 
#' For objects of class \code{\link{FLModel}}, \code{cv} returns the result of
#' 
#' \deqn{\frac{\sqrt{diag(\Sigma)}}{\Theta}}{sqrt(diag(sigma))/theta}
#' 
#' where \eqn{\Sigma}{sigma} is the variance-covariance matrix of the
#' \eqn{\Theta}{theta} parameter set.
#' 
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
#' flq <- FLQuant(rnorm(200, 5, 10), dim=c(5,10), iter=100)
#' cv(flq)
#' 