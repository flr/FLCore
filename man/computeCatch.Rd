% $Id: computeCatch.Rd 11 2009-01-26 15:00:07Z imosqueira $
\name{computeCatch}
\docType{methods}
% catch
\alias{computeCatch}
\alias{computeCatch-methods}
\alias{computeCatch,FLStock-method}
\alias{computeCatch,FLIndex-method}
 % landings
\alias{computeLandings}
\alias{computeLandings-methods}
\alias{computeLandings,FLStock-method}
% discards
\alias{computeDiscards}
\alias{computeDiscards-methods}
\alias{computeDiscards,FLStock-method}
% stock
\alias{computeStock}
\alias{computeStock-methods}
\alias{computeStock,FLStock-method}
\alias{computeStock,FLBiol-method}

\title{Methods to compute total catch, landings, discards and stock biomass}
\description{
	These methods compute the total catch, landings, discards and stock biomass from the
  quant-structured values in numbers and weight per individual. The calculation for
  discards, landings and stock involves the product of the landings/discards/stock in
  numbers (\code{landings.n}, \code{discards.n} or \code{stock.n}) by the individual
  weight-at-quant (\code{landings.wt}, \code{discards.wt} or \code{stock.wt}), as in

		\deqn{L=L_n * L_{wt}}{landings = landings.n * landings.wt}
	
	By selecting \code{slot="catch"}, \code{computeCatch} can calculate in the same way
	the total catch from the catch-at-quant and weight in the catch. Those two values (in
	slots \code{catch.n} and \code{catch.wt} can also be calculated by specifying 
	\code{slot="n"} and \code{slot="wt"} respectively. Calling \code{computeCatch} with
	option \code{slot="all"} will carry out the three calculations. In this case, the
	returned object will be of class \code{\link{FLQuants}}, with elements names
	\code{catch}, \code{catch.n} and \code{catch.wt}, which can then be passed directly
	to the \code{\link{catch<-}} replacement method.
}
\section{Generic function}{
computeCatch(object, ...) 

computeLandings(object, ...) 

computeDiscards(object, ...) 

computeStock(object, ...) 
}
\section{Method arguments}{\describe{
\item{\code{object} :}{an object of a class for which this method has been defined.}
\item{\code{slot} :}{a character vector to select the calculation to perform in
	\code{computeCatch}. One of "n", "wt", "all" or "catch", the default value.
	The later will compute the total catch (for slot \code{catch}) from \code{catch.n}
	and \code{catch.wt}}
\item{\code{na.rm} :}{a logical indicating whether \code{NA}s should be deleted from the
	sums. Defaults to \code{TRUE}.}
}}
\section{Methods}{\describe{
\item{ signature(object=FLStock) :}{computation on an \code{\link{FLStock}} object.}
\item{ signature(object=FLIndex) :}{computation on an \code{\link{FLIndex}} object.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
data(ple4)
summary(computeLandings(ple4))
landings(ple4) <- computeLandings(ple4)
catch(ple4) <- computeCatch(ple4, slot="all")
}
\keyword{methods}
