#' Class FLBiol
#' 
#' A class for modelling age / length or biomass structured populations.
#' 
#' The \code{FLBiol} class is a representation of a biological fish population
#' (in contrast to \code{\link{FLStock}}, which represents a fish stock; the
#' two may not fully coincide). This includes information on abundances,
#' natural mortlity and fecundity.
#' 
#' @name FLBiol
#' @aliases FLBiol FLBiol,FLQuant-method FLBiol,missing-method FLBiol-class
#' FLBiol-methods desc,FLBiol-method desc<-,FLBiol,character-method
#' fec,FLBiol-method fec<-,FLBiol,FLQuant-method m,FLBiol-method
#' m<-,FLBiol,FLQuant-method n,FLBiol-method n<-,FLBiol,FLQuant-method
#' name,FLBiol-method name<-,FLBiol,character-method range,FLBiol-method
#' range<-,FLBiol,numeric-method spwn,FLBiol-method
#' spwn<-,FLBiol,FLQuant-method wt,FLBiol-method wt<-,FLBiol,FLQuant-method
#' @docType class
#' @section Slots: \describe{ \item{n}{Numbers in the population.
#' \code{FLQuant}.} \item{m}{Mortality rate of the population. \code{FLQuant}.}
#' \item{wt}{Mean weight of an individual. \code{FLQuant}.}
#' \item{fec}{Fecundity/maturity/per capita birth rate. \code{FLQuant}.}
#' \item{spwn}{Proportion of mortality before spawning/birth. \code{FLQuant}.}
#' \item{name}{Name of the object. \code{character}.} \item{desc}{Brief
#' description of the object. \code{character}.} \item{range}{Named numeric
#' vector containing the quant and year ranges, and the plusgroup
#' (\code{numeric}).} }
#' @author The FLR Team
#' @seealso \link{as.FLBiol}, \link{as.FLSR}, \link[methods]{coerce},
#' \link[graphics]{plot}, \link{ssb} \link{catch.n,FLBiol-method}
#' @keywords classes
#' @examples
#' 
#' # An FLBiol example dataset
#'   data(ple4.biol)
#'   summary(ple4.biol)
#'
#' # Create an FLBiol object from an existing FLStock object
#'   data(ple4)
#'   flb1 <- as.FLBiol(ple4)
#'   plot(flb1)
#'
#'   # Just extract same dimensions without contents
#'     flb2 <- FLBiol(stock.n(ple4))
#'