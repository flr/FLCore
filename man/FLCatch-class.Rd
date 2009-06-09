\name{FLCatch}
\docType{class}
\alias{FLCatch-class}
\alias{FLCatch}
\alias{FLCatch-methods}
\alias{FLCatch,FLQuant-method}
\alias{FLCatch,missing-method}
\title{Class FLCatch}
\description{
  An internal class for \code{FLMetier} objects that stores stock or species specific 
  information on landings, discards and catches, catchability and price.

  Catches are computed as the sum of landings and discards.

  Landings, discards and catches are described with four slots each, one intented to deal
  with age (or other quant)-aggregated total estimate and three intented to deal with age
  (or other quant)-disaggregated information (numbers at age, mean weight at age and     
  selectivity at age).

}
\section{Slots}{\describe{
\item{catch.q}{Catchability in the catches. Can be quant-disagggregated or not. \code{FLQuant}.}
\item{landings}{Total landings in weight. \code{FLQuant}.}
\item{landings.n}{Numbers at quant in landings. \code{FLQuant}.}
\item{landings.wt}{Mean weight at quant in catches. \code{FLQuant}.}
\item{landings.sel}{Selectivity at quant in the catches. \code{FLQuant}.}
\item{discards}{Total discards in weight. \code{FLQuant}.}
\item{discards.n}{Numbers at quant in catches. \code{FLQuant}.}
\item{discards.wt}{Mean weight at quant in discards. \code{FLQuant}.}
\item{discards.sel}{Selectivity at quant in the discards. \code{FLQuant}.}
\item{price}{mean price. Can be quant-disagggregated or not. \code{FLQuant}. }
\item{name}{Generic name slot inherited from FLComp class. \code{character}.}
\item{desc}{Generic description slot inherited from FLComp class. \code{character}.}
\item{range}{Generic range slot inherited from FLComp class indicating
 min quant, max quant, plusgroup quant,minyear and maxyear. \code{numeric}.}
}}
\section{Extends}{\describe{
FLComp
}}

\section{Constructor}{
\describe{
\item{}{The \code{FLCatch(object)} constructor method allows simple creation of new FLCatch objects from a species name and optional FLQuants.
}
\item{ signature(object=FLQuant) :}{Creates a new FLCatch object using the dimensions of
  the FLQuant. If no slot is specified, all slots are filled with NA. If one slot is 
  specified, it will receive the value of the FLQuant, while other slots will be filled 
  with NA. It is also possible to specify several slots receiving different values from 
  different FLQuant, if their dimensions 2 to 6 are identical. The first dimension of the 
  slots \code{catch}, \code{discards} and \code{landings} is by default "all".
}
\item{ signature(object=missing) :}{Creates a new FLCatch whose slots are filled with an empty FLQuant with quant="all" and year=1 }
}}



\section{Methods}{\describe{
\item{\link[graphics]{as.data.frame}(base) :}{coerces the FLCatch object to a data.frame. Slot names are passed in the slot column. The name, desc and range slots is passed as an
  attribute of the data.frame}
\item{}{\code{ signature(x=FLCatch, row.names=missing, optional=missing) }}
\item{\link{catch}(FLCore) :}{accessor method for the catch slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{catch.q}(FLCore) :}{accessor method for the catch.q slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{catch.q<-}(FLCore) :}{replacement method for the catch.q slot}
\item{}{\code{ signature(object=FLCatch, value=FLQuant) }}
\item{\link{catch.n}(FLCore) :}{returns catch in numbers by quant as sum of landings.n and discards.n}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{catch.sel}(FLCore) :}{returns catch.sel as calculated from landings.sel and discards.sel}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{catch.sel<-}(FLCore) :}{replacement method for the catch.sel slot}
\item{}{\code{ signature(object=FLCatch, value=FLQuant) }}
\item{\link{catch.wt}(FLCore) :}{returns catch.wt as a weighted mean of landings.wt and discards.wt}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{discards}(FLCore) :}{accessor method for the discards slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{discards<-}(FLCore) :}{replacement method for the discards slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.n}(FLCore) :}{accessor method for the discards.n slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{discards.n<-}(FLCore) :}{replacement method for the discards.n slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.sel}(FLCore) :}{accessor method for the discards.sel slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{discards.sel<-}(FLCore) :}{replacement method for the discards.sel slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.wt}(FLCore) :}{accessor method for the discards.wt slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{discards.wt<-}(FLCore) :}{replacement method for the discards.wt slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings}(FLCore) :}{accessor method for the landings slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{landings<-}(FLCore) :}{replacement method for the landings slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.n}(FLCore) :}{accessor method for the landings.n slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{landings.n<-}(FLCore) :}{replacement method for the landings.n slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.sel}(FLCore) :}{accessor method for the landings.sel slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{landings.sel<-}(FLCore) :}{replacement method for the landings.sel slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.wt}(FLCore) :}{accessor method for the landings.wt slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{landings.wt<-}(FLCore) :}{replacement method for the landings.wt slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{price}(FLCore) :}{accessor method for the price slot}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{price<-}(FLCore) :}{replacement method for the price slot}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{FLFleet}(FLCore) :}{creates a FLFleet object with one single metier from a FLCatch object.
All other FLFleet and FLMetier slots are set to NA.  }
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{FLMetier}(FLCore) :}{creates a FLMetier object from a FLCatch object. Effshare and
vcost slots are set to NA.}
\item{}{\code{ signature(catches=FLCatch) }}
\item{\link[methods]{coerce}(methods) :}{coerces a FLStock object into a FLCatch object. 
  This method is called with as(x,"FLCatch"). In that case, catch.sel is calculated as harvest(x)/max(harvest(x)), catch.q is set to 1 and landings.sel and discards.sel are calculated proportionally to numbers}
\item{}{\code{ signature(from=NULL,to=FLCatch,strict=missing) }}
\item{\link{computeCatch}(FLCore) :}{computes the total catch from the quant-structured
	values in numbers and weight per individual.}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{computeDiscards}(FLCore) :}{computes the total discards from the quant-structured values in numbers and weight per individual.}
\item{}{\code{ signature(object=FLCatch) }}
\item{\link{computeLandings}(FLCore) :}{computes the total landings from the quant-structured values in numbers and weight per individual.}
\item{}{\code{ signature(object=FLCatch) }}

}}

\author{The FLR Team}
\seealso{ \link[base]{as.data.frame}, \link{catch}, \link{catch<-}, \link{catch.q}, \link{catch.q<-},
\link{catch.n}, \link{catch.n<-}, \link{catch.sel}, \link{catch.sel<-}, \link{catch.wt},
\link{catch.wt<-}, \link[methods]{coerce}, \link{computeCatch}, \link{computeDiscards},
\link{computeLandings}, \link{discards}, \link{discards<-}, \link{discards.n}, \link{discards.n<-},
\link{discards.sel}, \link{discards.sel<-}, \link{discards.wt}, \link{discards.wt<-},
\link{FLFleet}, \link{FLMetier}, \link{landings}, \link{landings<-}, \link{landings.n},
\link{landings.n<-}, \link{landings.sel}, \link{landings.sel<-}, \link{landings.wt}, \link{landings.wt<-},
\link{price}, \link{price<-}, \link{FLComp} }
\examples{
data(ple4)
mycatch<-as(ple4,"FLCatch")

landings(mycatch)<-computeLandings(mycatch)
landings(mycatch)

}
\keyword{classes}
