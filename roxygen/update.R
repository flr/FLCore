#' Method update
#' 
#' \code{update} is a generic function for updating a model fit using the
#' same call that generated it. Input arguments can be provided that will alter
#' the FLModel object accordingly.
#'
#' @name update
#' @aliases update,FLModel-method
#' @docType methods
#' @section Generic function: update(object, ...)
#' @author The FLR Team
#' @seealso \linkS4class{FLModel}, \link[stats]{update}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' nsher <- update(nsher, ssb=ssb(nsher) * 1.4)
#'