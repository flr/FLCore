#' Method plot
#' 
#' Standard plot methods for every FLR class. FLR plot methods are based on
#' \code{\link[lattice]{lattice}}, and attempt to show a general view of the
#' object contents.
#' 
#' Users are encouraged to write their own plotting code and make use of the
#' overloaded \code{\link[lattice]{lattice}} methods, for example
#' \code{\link[lattice]{xyplot}} or \code{\link[lattice]{bwplot}}. See also
#' \code{\link{lattice-FLCore}}.
#'
#' @name plot
#' @aliases plot,FLQuant,missing-method plot,FLQuantPoint,missing-method
#' plot,FLPar,missing-method plot,FLStock,missing-method
#' plot,FLStocks,missing-method plot,FLStocks,FLPar-method
#' plot,FLBiol,missing-method plot,FLCohort,missing-method
#' plot,FLIndex,missing-method plot,FLIndices,missing-method
#' plot,FLSR,missing-method
#' @docType methods
#' @section Generic function: plot(x,y)
#' @author The FLR Team
#' @seealso \link[graphics]{plot}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' data(ple4.biol)
#' 
#' # FLQuant
#'   plot(catch.n(ple4)[, 1:20])
#'   plot(catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)
#' 
#' # FLStock
#'   plot(ple4)
#' 
#' # FLBiol
#'   plot(ple4.biol)
#' 