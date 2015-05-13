#' Standard deviation of an FLModel object
#' 
#' \code{sd} computes the standard deviation of the parameter estimates in an
#' FLModel object, either by calculating the diagonal of the square root of the
#' variance-covariance matrix or, if multiple parameter estimates, as the
#' standard deviation of each parameter.
#' 
#' 
#' @name sd
#' @aliases sd,FLModel,missing-method
#' @docType methods
#' @section Generic function: sd(x, na.rm)
#' @author The FLR Team
#' @seealso \code{\link[stats]{sd}}, \code{\link{FLModel}}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' sd(nsher)
#' 