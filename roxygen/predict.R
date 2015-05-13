#' Method predict
#' 
#' \emph{predict} returns predicted values according to the parameter values
#' and model formula in an \code{\linkS4class{FLModel}} object. If no extra
#' input is given, \emph{predict} will use the input values contained in the
#' relevant slots. If any extra named argument is provided, this is used
#' instead and the correspodning predicted values are returned.
#' 
#' 
#' @name predict
#' @aliases predict,FLModel-method
#' @docType methods
#' @section Generic function: predict(object, ...)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # nsher FLSR dataset
#' data(nsher)
#' 
#' # predict with no extra arguments returns the values
#' # predicted during model fitting
#' predict(nsher)
#' 
#' # which can also be extracted from the 'fitted' slot
#' fitted(nsher)
#' 
#' # a different ssb vector can be provided
#' predict(nsher, ssb=FLQuant(seq(10, 150, by=5)))
#' 