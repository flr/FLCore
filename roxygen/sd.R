#' Method sd
#'
#' Standard deviation of an \code{FLModel} object
#' 
#' \code{sd} computes the standard deviation of the parameter estimates in an
#' \code{FLModel} object by calculating either the square root of the diagonal
#' of the variance-covariance matrix[[, or, if there are multiple parameter
#' estimates, the standard deviation of each parameter]].
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