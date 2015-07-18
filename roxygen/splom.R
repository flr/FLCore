#' Method splom
#' 
#' Draws a conditional scatter plot matrix.
#' 
#' See the help page in \code{\link[lattice]{lattice}} for a full description
#' of each plot and all possible arguments.
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
#' flp <- FLPar(t(mvrnorm(500, mu=c(0, 120, 0.01, 20),
#'   Sigma=matrix(.7, nrow=4, ncol=4) + diag(4) * 0.3)),
#'   dimnames=list(params=c('a','b','c','d'), iter=1:500))
#'
#' splom(flp)
#'