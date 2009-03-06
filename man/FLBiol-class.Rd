\name{FLBiol}
\docType{class}
\alias{FLBiol-class}
\title{Class FLBiol}
\description{
 This is a class for the modelling of age/length or biomass structured populations.
 It is  in its structure a subset of the \code{FLStock-class} class, but lacks the 
 harvesting information of that class. The slots are for mortality (natural or otherwise),
 fecundity/maturity/per capita birth rates, numbers, weight and mortality components 
 before spawning/birthing period. 

 The non-accessor methods for this class calculate mature stock biomass/numbers of
 eggs/etc. and many life-history methods are available that calculate natural mortality 
 (given certain information), the intrinsic rate of increase \emph{r} (using various
 methods), or to apply a Leslie-transition matrix model for estimating the population 
 dynamics.
}
\section{Slots}{\describe{
\item{n}{for the numbers in the population. \code{FLQuant}.}
\item{m}{ for the mortality rate of the population. \code{FLQuant}.}
\item{wt}{ for the weight of the population. \code{FLQuant}.}
\item{fec}{ for the fecundity/maturity/per capita birth rate. \code{FLQuant}.}
\item{spwn}{ for the proportion of mortality before spawning/birth. \code{FLQuant}.}
\item{name}{ name of the object. \code{character}.}
\item{desc}{ brief description of the object. \code{character}.}
\item{range}{ range of the dimensions. \code{numeric}.}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link{as.FLBiol}(FLCore) :}{Coerces object ino an \code{FLBiol} object.}
\item{}{\code{ signature(object=missing) }}
\item{\link{as.FLSR}(FLCore) :}{Coerces an \code{FLBiol} into an \code{FLSR} object.}
\item{}{\code{ signature(object=missing) }}
\item{\link[methods]{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=NULL,to=FLBiol,strict=missing) }}
\item{\link[methods]{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=FLBiol,to=FLStock,strict=missing) }}
\item{\link[methods]{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=FLStock,to=FLBiol,strict=missing) }}
\item{\link{fec}(FLCore) :}{accesses the \code{fec} slot.}
\item{}{\code{ signature(object=missing) }}
\item{\link{fec<-}(FLCore) :}{replacement method for this slot.}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{leslie}(FLCore) :}{applies a Leslie-transition matrix model for the \code{FLBiol} object.}
\item{}{\code{ signature(object=FLBiol,value=FLBiol) }}
\item{\link{m}(FLCore) :}{accesses the mortality slot.}
\item{}{\code{ signature(object=missing) }}
\item{\link{m<-}(FLCore) :}{replacement method for this slot.}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{n}(FLCore) :}{accesses the numbers slot.}
\item{}{\code{ signature(object=missing) }}
\item{\link{n<-}(FLCore) :}{replacement method for this slot.}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link[graphics]{plot}(graphics) :}{default plot method for \code{FLBiol}.}
\item{}{\code{ signature(x=FLBiol,y=missing) }}
\item{\link{spwn}(FLCore) :}{accesses the \code{spwn} slot.}
\item{}{\code{ signature(object=missing) }}
\item{\link{spwn<-}(FLCore) :}{replacement method for this slot.}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{ssb}(FLCore) :}{calculates the spawning stock biomass.}
\item{}{\code{ signature(object=missing) }}
\item{\link{wt}(FLCore) :}{accesses the weight slot.}
\item{}{\code{ signature(object=missing) }}
\item{\link{wt<-}(FLCore) :}{replacement method for this slot.}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{fbar}(FLCore) :}{calculates fbar}
\item{}{\code{ signature(object=FLBiol) }}
\item{\link{fbar}(FLCore) :}{calculates harvest, as fishing mortality. The ratio of F between the plusgroup, if specified, and the previous age, \code{fratio}, defaults to 1.}
\item{}{\code{ signature(object=FLBiol) }}
}}
\author{The FLR Team}
\seealso{ \link{as.FLBiol}, \link{as.FLSR}, \link[methods]{coerce}, \link{fec}, \link{fec<-}, \link{m}, \link{m<-}, \link{n}, \link{n<-}, \link[graphics]{plot}, \link{spwn}, \link{spwn<-}, \link{ssb}, \link{wt}, \link{wt<-}, \link{FLComp} }
\examples{
# An FLBiol example dataser
data(ple4.biol)

summary(ple4.biol)
}
\keyword{classes}
