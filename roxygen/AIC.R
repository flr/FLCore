#' Method AIC
#' 
#' A method to calculate the Akaike's information criterion (AIC) of an
#' \link{FLModel} object from the value of the obtained log-likelihood stored
#' in its \code{logLik} slot.
#'
#' @name AIC
#' @aliases AIC,FLModel,numeric-method AIC,FLModel,missing-method
#' @docType methods
#' @section Generic function: AIC(object, k)
#' @author The FLR Team
#' @seealso \link[stats]{AIC}, \link[stats]{logLik}, \link{FLModel}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' AIC(nsher)
#' 