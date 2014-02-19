\name{ssbpurec}
\docType{methods}
\alias{ssbpurec}
\alias{ssbpurec-methods}
\alias{ssbpurec,FLStock-method}
\title{Method ssbpurec}
\description{
 Calculates the Spawning Stock Biomass per unit recruit for an \code{FLStock} object.
 
 The method calculates SSB per recruit at zero fishing mortality.
}
\section{Generic function}{
 ssbpurec(object) 
}
\section{Method arguments}{\describe{
\item{\code{object} :}{an object of class \code{FLStock}.}
\item{\code{start, end} :}{The first and last year over which SSB per recruit is
to be calculated. By default the first and last year of the \code{FLStock} object
are used.}
\item{\code{type} :}{The type of calculation to perform. Currently only non-parm
(non parametric) is supported whereby the SSB per recruit is calculated using
mean values over the specified time period.}
\item{\code{recs} :}{The proportion of the year in which recruitment occurs.}
\item{\code{spwns} :}{The proportion of the year in which spawning occurs.}
\item{\code{plusgroup} :}{Is the last age a plusgroup TRUE/FALSE}
}}
\section{Methods}{\describe{
\item{ signature(object=FLStock) :}{Returns the SBB-per-unit-recruit.}
}}
\author{The FLR Team}
\seealso{ \linkS4class{FLStock} }
\examples{

data(ple4)
ssbpurec(ple4)

ssbpurec(ple4, start=1980, end=2000)
}
\keyword{methods}
