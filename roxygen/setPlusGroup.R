#' Method setPlusGroup
#' 
#' Calculates the appropriate values for the plusgroup of an object and returns
#' a new object with the plusgroup set to the given age.
#' 
#' \emph{quant} of the given object must be 'age', and the selected age must
#' not be greater than the oldest age present in the object.
#'
#' @name setPlusGroup
#' @aliases setPlusGroup setPlusGroup-methods
#' setPlusGroup,FLQuant,numeric-method setPlusGroup,FLStock,numeric-method
#' setPlusGroup,FLBiol,numeric-method
#' @docType methods
#' @section Generic function: sePlusGroup(x, plusgroup)
#' @author The FLR Team
#' @seealso \linkS4class{FLStock}, \linkS4class{FLQuant}, \linkS4class{FLBiol},
#' \linkS4class{FLCatch}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' ple4.pg <- setPlusGroup(ple4, 6)
#' 
