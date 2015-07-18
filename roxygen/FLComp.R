#' Class FLComp
#' 
#' A virtual class that forms the basis for most FLR classes composed of slots
#' of class \code{\linkS4class{FLQuant}}. No objects of this class can be
#' constructed.
#'
#' @name FLComp
#' @aliases FLComp FLComp-class
#' @docType class
#' @section Validity: \describe{ \item{Dimensions}{All FLQuant slots must have
#' iters equal to 1 or 'n'.} \item{Iters}{The dimname for iter[1] should be
#' '1'.} \item{Dimnames}{The name of the quant dimension must be the same for
#' all FLQuant slots.} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame},
#' \link{iter}, \link{propagate}, \link{qapply}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link{units,FLComp-method},
#' \link{units<-,FLComp,list-method}, \link[stats]{window}
#' @keywords classes
#'