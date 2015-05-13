#' Method apply
#' 
#' Functions can be applied to margins of an FLQuant array using this method.
#' In contrast with the standard method, dimensions are not collapsed in the
#' output object.
#' 
#' \code{FUN} in the case of an \code{FLQuant} must collapse at least one
#' dimension when applied over an array.
#' 
#' For further details see \link[base]{apply}.
#' 
#' 
#' @name apply
#' @aliases apply,ANY,missing,missing-method
#' apply,FLQuant,numeric,function-method
#' @docType methods
#' @section Generic function: apply(X,MARGIN,FUN)
#' @author The FLR Team
#' @seealso \link[base]{apply}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(100), dim=c(10,20,1,1,1,5))
#' apply(flq, 1, sum)
#' apply(flq, 2:6, sum)
#' 