#' Methods to compute total catch, landings, discards and stock biomass
#' 
#' These methods compute the total catch, landings, discards and stock biomass
#' from the quant-structured values in numbers and weight per individual. The
#' calculation for discards, landings and stock involves the product of the
#' landings/discards/stock in numbers (\code{landings.n}, \code{discards.n} or
#' \code{stock.n}) by the individual weight-at-quant (\code{landings.wt},
#' \code{discards.wt} or \code{stock.wt}), as in
#' 
#' \deqn{L=L_n * L_{wt}}{landings = landings.n * landings.wt}
#' 
#' By selecting \code{slot="catch"}, \code{computeCatch} can calculate in the
#' same way the total catch from the catch-at-quant and weight in the catch.
#' Those two values (in slots \code{catch.n} and \code{catch.wt} can also be
#' calculated by specifying \code{slot="n"} and \code{slot="wt"} respectively.
#' Calling \code{computeCatch} with option \code{slot="all"} will carry out the
#' three calculations. In this case, the returned object will be of class
#' \code{\link{FLQuants}}, with elements names \code{catch}, \code{catch.n} and
#' \code{catch.wt}, which can then be passed directly to the
#' \code{\link{catch<-}} replacement method.
#' 
#' 
#' @name computeCatch
#' @aliases computeCatch computeCatch-methods computeCatch,FLStock-method
#' computeCatch,FLIndex-method computeLandings computeLandings-methods
#' computeLandings,FLStock-method computeDiscards computeDiscards-methods
#' computeDiscards,FLStock-method computeStock computeStock-methods
#' computeStock,FLStock-method computeStock,FLBiol-method
#' @docType methods
#' @section Generic function: computeCatch(object, ...)
#' 
#' computeLandings(object, ...)
#' 
#' computeDiscards(object, ...)
#' 
#' computeStock(object, ...)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' summary(computeLandings(ple4))
#' landings(ple4) <- computeLandings(ple4)
#' catch(ple4) <- computeCatch(ple4, slot="all")
#' 