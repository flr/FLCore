#' Bayesian information criterion (BIC) method
#' 
#' A method to calculate the Bayesian information criterion (BIC), also known
#' as Schwarz's Bayesian criterion of an \link{FLModel} object from the value
#' of the obtained log-likelihood stored in its \code{logLik} slot.
#' 
#' 
#' @name BIC
#' @aliases BIC,FLModel-method
#' @docType methods
#' @section Generic function: BIC(object)
#' @author The FLR Team
#' @seealso \link[stats]{BIC}, \link[stats]{AIC}, \link{FLModel},
#' \link[stats]{logLik}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' BIC(nsher)
#' 