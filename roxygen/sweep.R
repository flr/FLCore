#' Method sweep
#'
#' Sweep out FLQuant Summaries
#' 
#' Return an \code{FLQuant} or \code{FLCohort} obtained from an input object by
#' sweeping out a summary statistic along the selected dimensions.
#'
#' @name sweep
#' @aliases sweep,FLArray-method sweep,FLQuant-method
#' @docType methods
#' @return An FLQuant or FLCohort with the same shape as \code{x}, but with the
#' summary statistics swept out.
#' @section Generic function: sweep(x, MARGIN, STATS, FUN="-",
#' check.margin=TRUE, \dots{})
#' @author The FLR Team
#' @seealso \code{\link[base]{sweep}}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' mean.f <- apply(harvest(ple4),2,mean)
#' scaled.f <- sweep(harvest(ple4),2,mean.f,"/")
#' 