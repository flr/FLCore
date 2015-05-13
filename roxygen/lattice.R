#' Lattice plots
#' 
#' Implementation of Trellis graphics in FLR. Plot methods in the
#' \code{\link[lattice]{lattice}} package are available for object of class
#' \code{FLQuant}, \code{FLQuants} or those derive from \code{FLComp}.
#' 
#' See the help page in \code{\link[lattice]{lattice}} for a full description
#' of each plot method and of all possible arguments.
#' 
#' Plot methods from lattice are called by passing a \link[base]{data.frame}
#' obtained by converting the FLR objects using as.data.frame. For details on
#' this transformation, see \link{as.data.frame-FLCore}.
#' 
#' 
#' @name lattice
#' @aliases lattice-FLCore barchart,formula,FLQuant-method
#' barchart,formula,FLComp-method bwplot,formula,FLQuant-method
#' bwplot,formula,FLComp-method densityplot,formula,FLPar-method
#' dotplot,formula,FLQuant-method dotplot,formula,FLComp-method
#' histogram,formula,FLQuant-method histogram,formula,FLQuants-method
#' histogram,formula,FLComp-method histogram,formula,FLPar-method
#' stripplot,formula,FLQuant-method stripplot,formula,FLComp-method
#' xyplot,formula,FLQuant-method xyplot,formula,FLQuants-method
#' xyplot,formula,FLCohort-method xyplot,formula,FLComp-method
#' @docType methods
#' @section Generic function: barchart(x, data, ...)
#' 
#' bwplot(x, data, ...)
#' 
#' densityplot(x, data, ...)
#' 
#' dotplot(x, data, ...)
#' 
#' histogram(x, data, ...)
#' 
#' stripplot(x, data, ...)
#' 
#' xyplot(x, data, ...)
#' @author The FLR Team
#' @seealso \link[lattice]{xyplot}, \link[lattice]{barchart},
#' \link[lattice]{bwplot}, \link[lattice]{densityplot},
#' \link[lattice]{dotplot}, \link[lattice]{histogram},
#' \link[lattice]{stripplot}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' # xyplot on FLQuant
#' xyplot(data~year|age, catch.n(ple4)[, 1:20])
#' 
#' xyplot(data~year|as.factor(age), catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)
#' 
#' # bwplot on FLQuant with iter
#' flq <- rnorm(100, catch.n(ple4)[, 1:20], catch.n(ple4)[,1:20])
#' bwplot(data~year|as.factor(age), flq)
#' 
#' # now with same style modifications
#' bwplot(data~year|as.factor(age), flq, scales=list(relation='free',
#'   x=list(at=seq(1, 20, by=5), labels=dimnames(catch.n(ple4)[,1:20])$year[seq(1, 20,
#'   by=5)])), cex=0.5, strip = strip.custom(strip.names = TRUE, strip.levels = TRUE,
#'   var.name='age'))
#' 