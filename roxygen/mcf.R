#' Method mcf
#' 
#' This method makes FLQuants compatible with respect to their dimensionality.
#' Hence, the FLQuants in the returned object all heve the same dimensions,
#' padded with NAs if necessary
#' 
#' 
#' @name mcf
#' @aliases mcf mcf-methods mcf,FLComp-method mcf,list-method
#' @docType methods
#' @section Generic function: mcf(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' fla <- FLQuant(rnorm(20), dim=c(2,10))
#' flb <- FLQuant(rnorm(45), dim=c(3,15))
#' fls <- FLQuants(a=fla, b=flb)
#' flc <- mcf(fls)
#' lapply(flc, dim)
#' 