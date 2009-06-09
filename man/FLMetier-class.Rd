\name{FLMetier}
\docType{class}
\alias{FLMetier-class}
\alias{FLMetier}
\alias{FLMetier-methods}
\alias{FLMetier,FLCatch-method}
\alias{FLMetier,FLCatches-method}
\alias{FLMetier,FLQuant-method}
\alias{FLMetier,missing-method}

\title{Class FLMetier}
\description{The \code{FLMetier} class is an internal class of FLFleet storing all 
  information related to the activity of a fleet in a specific metier. A metier refers
  typically to a particular type of activity described by e.g. a specific gear and/or mesh 
  size range and/or target species and/or spatio-temporal distribution etc. The underlying 
  use of the FLMetier class is to distinguish between several types of activities within the 
  same fleet unit.

  The \code{FLMetier} class can contain information about gear used, share of the effort of
  the fleet in that metier, variable costs, as well as catch information. Catches data are   
  stored in a \code{FLCatches} object, which is a list of \code{FLCatch} objects. Each of 
  them stores stock or species specific information such as landings, discards and catches, 
  catchability and price.

  A number of accessors are defined for \code{FLMetier}, allowing to extract directly 
  specific slots both from the \code{FLMetier} class and to the \code{FLCatch} objects it 
  contains. If catch is specified in those, it returns the FLQuant corresponding to the 
  FLCatch object of that name. If catch is missing, it returns a FLQuants object for all 
  stocks of the \code{FLCatches} object. Corresponding replacement methods for these slots 
  exist for the FLQuant objects but not for the FLQuants objects, due to possible 
  differences of dimensions across FLCatch objects.

}
\section{Slots}{\describe{
\item{gear}{Name of the gear. \code{character}.}
\item{effshare}{Proportion of the total effort of the fleet allocated to
  this metier. effshare must not be higher than 1, and the sums of effshare across all 
  metiers within a fleet should be equal to or lower than 1. \code{FLQuant}.}
\item{vcost}{Variable costs per unit of effort.  \code{FLQuant}.}
\item{catches}{List of \code{FLCatch} objects containing catch
  information by stock or species. \code{FLCatches}.}
\item{name}{Name of the object.  \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{range}{ (\code{numeric}) Named vector containing the dimensions of the object}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLMetier} constructor method allows creation of new FLMetier objects from either a FLQuant, a FLCatch or a FLCatches object.
}
\item{ signature(catches=FLCatch) :}{Creates a FLMetier object from a FLCatch object.}
\item{ signature(catches=FLCatches) :}{Creates a FLMetier object from a FLCatches object.}
\item{ signature(catches=FLQuant) :}{Creates a FLMetier object from a FLQuant object.}
\item{ signature(catches=missing) :}{Creates a FLMetier from scratch}
}}

\section{Methods}{\describe{
\item{\link{catch}(FLCore) :}{Accessor method for the catch slot for a given FLCatch
  or across the FLCatches objects  }
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n}(FLCore) :}{Accessor method for the catch.n slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.q}(FLCore) :}{Accessor method for the catch.q slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.sel}(FLCore) :}{Accessor method for the catch.sel slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt}(FLCore) :}{Accessor method for the catch.wt slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards}(FLCore) :}{Accessor method for the discards slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n}(FLCore) :}{Accessor method for the discards.n slot for a given 
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.sel}(FLCore) :}{Accessor method for the discards.sel slot for a given  
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt}(FLCore) :}{Accessor method for the discards.wt slot for a given 
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLFleet}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings}(FLCore) :}{Accessor method for the landings slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n}(FLCore) :}{Accessor method for the landings.n slot for a given  
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.sel}(FLCore) :}{Accessor method for the landings.sel slot for a given 
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt}(FLCore) :}{Accessor method for the landings.wt slot for a given 
  FLCatch or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{price}(FLCore) :}{Accessor method for the price slot for a given FLCatch
  or across the FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLFleet}(FLCore) :}{creates a FLFleet object from a FLMetier object.
  All other FLFleet slots are set to NA.  }
\item{}{\code{signature(object=missing)}}
}}
\author{The FLR Team}
\seealso{ \link{catch}, \link{catch.n}, \link{catch.q}, \link{catch.sel}, \link{catch.wt},
\link{discards}, \link{discards.n}, \link{discards.sel}, \link{discards.wt}, \link{FLFleet},
\link{landings}, \link{landings.n}, \link{landings.sel}, \link{landings.wt}, \link{price}, \link{FLComp} }
\examples{
data(bt4)
mymetier<-metier(bt4, "TBB")
summary(mymetier)

catch(mymetier) #returns a FLQuants object
catch(mymetier, "ple") #returns a FLQuant object
}
\keyword{classes}
