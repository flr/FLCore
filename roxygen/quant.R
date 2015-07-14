#' Method quant
#' 
#' Function to get or set the name of the first dimension (quant) in an object
#' of class \code{FLQuant} or \code{FLCohort}.
#'
#' @name quant
#' @aliases quant quant-methods quant,FLArray-method quant<- quant<--methods
#' quant<-,FLArray,missing-method quant<-,FLArray-method
#' quant<-,FLArray,character-method
#' @docType methods
#' @section Generic function: quant(object) quant<-(object,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLCohort}
#' @keywords methods
#' @examples
#' 
#' # quant is 'quant' by default
#'   quant(FLQuant())
#'
#' flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')
#' quant(flq)
#' quant(flq) <- 'length'
#' summary(flq)
#' 