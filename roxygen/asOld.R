#' Old S3 coercion methods
#' 
#' These methods convert or coerce an object of a given class into another
#' class. They follow the S3 syntax for coercion methods and are being slowly
#' substituted by \code{as()} (see \code{\link[methods]{coerce}}.
#' 
#' 
#' @name asOld
#' @aliases as.FLBiol as.FLBiol-methods as.FLBiol,FLBiol-method
#' as.FLBiol,FLStock-method as.FLIndex as.FLIndex-methods as.FLStock as.FLSR
#' as.FLSR-methods as.FLSR,FLStock-method as.FLSR,FLBiol-method as.FLQuant
#' as.FLQuant-methods as.FLQuant,array-method as.FLQuant,matrix-method
#' as.FLQuant,FLQuant-method as.FLQuant,vector-method
#' as.FLQuant,data.frame-method
#' @docType methods
#' @section Generic function: \describe{ \item{as.FLBiol(object)}{Convert to an
#' \code{\linkS4class{FLBiol}}} \item{as.FLIndex(object)}{Convert to an
#' \code{\linkS4class{FLIndex}}} \item{as.FLSR(object)}{Convert to an
#' \code{\linkS4class{FLSR}}} \item{as.FLQuant(x)}{Convert to an
#' \code{\linkS4class{FLQuant}}} }
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' is(ple4)
#' is(as.FLBiol(ple4))
#' 