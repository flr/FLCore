\name{coerce}
\docType{methods}
\alias{coerce,FLlst,list-method}
\alias{coerce,NULL,FLStock-method}
\alias{coerce,NULL,FLIndex-method}
\alias{coerce,NULL,FLBiol-method}
\alias{coerce,NULL,FLFleet-method}
\alias{coerce,NULL,FLQuant-method}
\alias{coerce,NULL,FLCatch-method}
\alias{coerce,FLPar,numeric-method}
\alias{coerce,FLPar,list-method}
\alias{coerce,FLPar,FLQuant-method}
\alias{coerce,FLQuant,FLPar-method}
\alias{coerce,FLCohort,FLQuant-method}
\alias{coerce,FLQuant,FLCohort-method}
\alias{coerce,FLBiol,FLStock-method}
\alias{coerce,FLBiol,FLIndex-method}
\alias{coerce,FLStock,FLBiol-method}
\alias{coerce,FLStock,FLCatch-method}
\alias{coerce,FLStock,FLMetier-method}
\alias{coerce,FLStock,FLFleet-method}
\alias{coerce,FLStock,FLIndex-method}
\alias{coerce,FLComp,FLQuants-method}
\alias{coerce,FLCatch,FLMetier-method}
\alias{coerce,FLCatch,FLMetiers-method}
\alias{coerce,FLCatch,FLStock-method}
\alias{coerce,FLCatch,FLIndex-method}
\alias{coerce,data.frame,FLComp-method}
\alias{coerce,data.frame,FLQuant-method}
\alias{coerce,data.frame,FLStock-method}
\alias{coerce,data.frame,FLIndex-method}
\title{Method coerce}
\description{
  Coercion methods for various sets of classes are generated using the
  \code{\link[methods]{coerce}} function. Users should call the corresponding generated 
  \code{as()} method, with arguments equal to the object to coerce and the name of the 
  class to convert to.

  Coercion combinations work by transferring or transforming relevant slots from the 
  original object and placing them in a new object of the target class. The descriptions 
  below document how slots for each pair of classes are transfered or transformed. In all 
  cases the \code{name} and \code{desc} slots are simply copied accross.
}
\section{Generic function}{
 coerce(from, to, strict) 
}
\section{Methods}{
\describe{
\item{from=FLBiol, to=FLStock:}{\code{n} to \code{stock.n}, \code{wt} to \code{stock.wt},
  \code{m} to \code{m},\code{fec} to \code{mat},\code{spwn} to \code{m.spwn}}
\item{from=FLStock, to=FLBiol:}{\code{stock.n} to \code{n}, \code{stock.wt} to \code{wt},
  \code{m} to \code{m},\code{mat} to \code{fec},\code{m.spwn} to \code{spwn}}
\item{from=FLStock, to=FLCatch:}{\code{catch.n} to \code{catch.n}, \code{catch.wt} to
  \code{catch.wt}, \code{catch} to \code{catch}, \code{landings.n} to \code{landings.n}, 
 \code{landings.wt} to \code{landings.wt}, \code{landings} to \code{landings}, 
 \code{discards.n} to \code{discards.n}, \code{discards.wt} to \code{discards.wt},
 \code{discards} to \code{discards}, \code{name} to \code{name}}
\item{from=FLStock,to=FLMetier}{As before but creating an \code{FLMetier} object with a 
  single catch element.}
\item{from=FLStock,to=FLFleet}{As before but creating an \code{FLFleet} object with a 
  single metier and catch.}
\item{from=FLQuant,to=FLCohort}{An \code{\link{FLCohort-class}} object is created from the
  year-structured data in an \code{\link{FLQuant}}. See \code{\link{FLCohort}} for a 
  description of the exact procedure.}
\item{from=FLCohort,to=FLQuant}{The previous calculation is reversed and an 
  \code{\link{FLQuant}} is returned.}
\item{from=FLlst,to=list}{An standard R \code{\link[base]{list}} object is created from an
  \code{\link{FLlst-class}} by dropping the extra attributes.}
}
}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
data(ple4)
flb <- as(ple4, 'FLBiol')}
\keyword{methods}
