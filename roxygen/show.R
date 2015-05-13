#' Method show
#' 
#' Standard display of an object contents in an interactive session. Objects of
#' class \code{\linkS4class{FLQuant}} with length > 1 along the sixth dimension
#' (\emph{iter}) are output in a summarised form, as \code{median (mad)}, where
#' mad is the median absolute deviation. See \code{\link[stats]{mad}}.
#' 
#' The same format is used for objects of class \code{\linkS4class{FLPar}} with
#' length > 1 on the last dimension ('iter').
#' 
#' 
#' @name show
#' @aliases show,FLArray-method show,FLQuant-method show,FLQuantPoint-method
#' show,FLQuants-method show,FLPar-method
#' @docType methods
#' @section Generic function: show(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # no 'iter'
#' flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age', units='kg')
#' flq
#' 
#' # with 'iter'
#' flq <- FLQuant(rnorm(800), dim=c(4,20,1,1,1,10), quant='age', units='kg')
#' flq
#' 