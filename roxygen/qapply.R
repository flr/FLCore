#' Method qapply
#' 
#' Returns a \code{\link[base]{list}} or \code{\link{FLlst}} containing values
#' obtained by applying a function to margins for each FLQuant in a composite
#' FLR object.
#' 
#' 
#' @name qapply
#' @aliases qapply qapply-methods qapply,FLComp,function-method
#' @docType methods
#' @section Generic function: qapply(X,FUN)
#' @author The FLR Team
#' @seealso \link{FLComp} \link{apply}
#' @keywords methods
#' @examples
#' 
#' 
#' data(ple4)
#' 
#' # returns a list containing the max value for each quant
#' qapply(ple4, max)
#' 
#' # returns a FLStock of means across all dimensions except year
#' qapply(ple4, apply, 2, mean, na.rm=TRUE) 
#' 
#' # returns an FLStock of max values across all dimensions except year and age
#' qapply(ple4, apply, c(1,2), max)
#' 
#' 