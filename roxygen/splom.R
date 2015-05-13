#' Method splom
#' 
#' Draws a conditional scatter plot matrix.
#' 
#' See the help page in \code{\link[lattice]{lattice}} for a full description
#' of each plot and all possible arguments.
#' 
#' 
#' @name splom
#' @aliases splom,FLPar,missing-method
#' @docType methods
#' @section Generic function: splom(x,data)
#' @author The FLR Team
#' @seealso \link[lattice]{splom}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' splom(params(nsher))
#' 