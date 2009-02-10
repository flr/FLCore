\name{ssb}
\docType{methods}
\alias{ssb}
\alias{ssb-methods}
\alias{ssb,FLStock-method}
\alias{ssb,FLBiol-method}
\title{Method ssb}
\description{
 Returns the Spawning Stock Biomass of \code{FLStock} and \code{FLBiol} objects.
 
 For \code{\link{FLStock-class}} objects the nature of the calculation depends
 on the units in the harvest slot. See details below.
}

\section{Generic function}{
 ssb(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLStock) :}{
 If spawning occurs at the beginning of the year the calculated SSB is the same
 regardless of the units of the harvest slot. If spawning occurs at any other
 time during the year such that the stock is subject to fishing mortality
 prior to spawning then the calculated SSB will depend on the units of the
 harvest slot.
 
 For an \code{\link{FLStock-class}} with harvest units 'f' SSB is calculated as

    SSB = sum(N*exp(-F*propF-M*propM) * wt * mat)

 For an \code{\link{FLStock-class}} with harvest units 'hr' SSB is calculated as

		SSB = sum(N*(1-harvest)*exp(-M*propM) * wt * mat)

 The units of the harvest slot in the \code{FLStock} object must be specified
 as either 'f' for an instantaneous fishing mortality or else as 'hr' for a
 harvest rate.
}
\item{ signature(object=FLBiol) :}{
 For an \code{\link{FLBiol-class}} the spawning biomass at the beginning of the
 year is calculated.

  	SSB = sum(N * wt * mat)
}
}}

\author{The FLR Team}
\seealso{ \link{FLBiol-class} \link{FLStock-class}}
\examples{
 data(ple4)
 units(harvest(ple4)) # check the units of the harvest slot
 ssb(ple4)
}
\keyword{methods}
