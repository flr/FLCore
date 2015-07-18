#' Method propagate
#'
#' Extend an FLQuant along the iter dimension
#' 
#' FLR objects with a single iteration (length of 1 in the sixth dimension) can
#' be extended using the \code{propagate} method. The \code{type} argument
#' selects whether the new iterations are filled with the content of the first
#' iteration (\code{fill.iter=TRUE}, the default) or filled with NAs
#' (\code{fill.iter=FALSE}).
#' 
#' For objects of class \code{\linkS4class{FLPar}}, propagate will extend the
#' object along the last dimension, \code{iter}. The fill.iter argument
#' defaults to TRUE. Objects do not need to have \code{iter=1} to be extended,
#' but this only works if fill.iter=FALSE.
#'
#' @name propagate
#' @aliases propagate propagate-methods propagate,FLQuant-method
#' propagate,FLComp-method propagate,FLCohort-method propagate,FLPar-method
#' @docType methods
#' @section Generic function: propagate(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#'
#' # For an FLQuant object
#'   flq <- FLQuant(rnorm(50), dim=c(5,10))
#'   propagate(flq, 10)

#'   # Look at the % NA in summary...
#'     summary(propagate(flq, 10, fill.iter=FALSE))
#'   # ...and compare with
#'     summary(propagate(flq, 10))
#'
#' # For an FLPar object
#'   flp <- FLPar(1:10, params=letters[1:10])
#'   propagate(flp, 10)
#'   propagate(flp, 10, fill.iter=FALSE)
#'
#'   # With iters
#'     flp <- FLPar(1:15, params=letters[1:5], iter=3)
#'     propagate(flp, 10, fill.iter=FALSE)
#' 