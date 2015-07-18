#' Method names
#' 
#' The \code{names} method returns the names of the dimnames of an object. For
#' some classes, the names attribute can be modified directly using names<-.
#'
#' @name names
#' @aliases names,FLArray-method names,FLlst-method names,FLPar-method
#' names<-,FLPar,character-method
#' @docType methods
#' @section Generic function: names(x) names<-(x, value)
#' @author The FLR Team
#' @seealso \link[base]{names}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' names(catch.n(ple4))
#'
#' # Contrast this with
#'   dimnames(catch.n(ple4))
#'