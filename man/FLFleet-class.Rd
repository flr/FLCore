\name{FLFleet}
\docType{class}
\alias{FLFleet-class}
\title{Class FLFleet}
\description{
  The \code{FLFleet} class is a composite object storing some useful information for the 
  description and analysis of fleet data, including basic information about effort and 
  economic data. It can account for simple or complex patterns such as several types of 
  activities and/or catches of several species.

  FLFleet is thus defined along a three-levelled tree structure. First, the root of 
  FLFleet contains data at the vessel/fleet segment levels, independent of activity and 
  catches, such as total effort, capacity, fixed costs and crew share. Then, data dealing 
  with the various types of activity of the fleet are dealt with at the metier level 
  using \code{FLMetier} class, which includes slots for proportion of effort and variable 
  costs. 
 
  Finally, the cath information related to the various species or stocks within each 
  metier are dealt with a \code{FLCatch} class. The tree structure allows a flexible use 
  of the FLFleet object, if for example not all species are caught by all metiers or if 
  the dimensions are not the same across branches.

  There is a certain degree of flexibility left in the slots definition, in the sense 
  that the user may decide to store either raw unit data, for example effort by unit of 
  capacity, variable cost by unit of effort, or to store aggregated data, for example 
  total effort or total variable costs. However, the user should be aware that some 
  computing methods (in FLCore or other related packages) are more specifically intended 
  for one type of data, and these should be used with caution.

  A number of accessors are defined for \code{FLFleet}, allowing to extract directly   
  specific slots at the three-levels FLFleet hierarchy i.e. from the \code{FLFleet} class
  itself, from the \code{FLMetier} objects and from the \code{FLCatch} objects it 
  contains. 
  
  If both catch and metier arguments are specified in those, it returns the FLQuant 
  corresponding to the species. If catch argument is missing but metier argument is
  specified, it returns a FLQuants object for all stocks of the \code{FLCatches} object 
  for the given specified metier. Finally, if catch argument is specified but metier 
  argument  is missing, it returns a FLQuants object across all metiers of the 
  \code{FLMetiers} object for the given specified species.

  Corresponding replacement methods for these slots exist for the FLQuant objects but not 
  for the FLQuants objects, due to possible differences of dimensions across FLMetier 
  and FLCatch objects.
}
\section{Slots}{\describe{
\item{effort}{effort (total or per unit of capacity). \code{FLQuant}.}
\item{fcost}{Fixed costs (total or per unit of capacity).  \code{FLQuant}.}
\item{capacity}{Capacity of the fleet. \code{FLQuant}.}
\item{crewshare}{Crew share (total or per unit of capacity). \code{FLQuant}.}
\item{metiers}{FLMetiers object storing a list of FLMetier objects. \code{FLMetiers}.}
\item{name}{Generic name slot inherited from FLComp class. \code{character}.}
\item{desc}{Generic description slot inherited from FLComp class.  \code{character}.}
\item{range}{Generic range slot inherited from FLComp class indicating
 min quant, max quant, plusgroup quant,minyear and maxyear. \code{numeric}.}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link{catch}(FLCore) :}{Accessor method for the catch slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n}(FLCore) :}{Accessor method for the catch.n slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt}(FLCore) :}{Accessor method for the catch.wt slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.sel}(FLCore) :}{Accessor method for the catch.sel slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.q}(FLCore) :}{Accessor method for the catch.q slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards}(FLCore) :}{Accessor method for the discards slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n}(FLCore) :}{Accessor method for the discards.n slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.sel}(FLCore) :}{Accessor method for the discards.sel slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt}(FLCore) :}{Accessor method for the discards.wt slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings}(FLCore) :}{Accessor method for the landings slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n}(FLCore) :}{Accessor method for the landings.n slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt}(FLCore) :}{Accessor method for the landings.wt slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.sel}(FLCore) :}{Accessor method for the landings.sel slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link{price}(FLCore) :}{Accessor method for the price slot across FLMetiers
and FLCatches objects}
\item{}{\code{ signature(object=missing) }}
\item{\link[methods]{coerce}(methods) :}{coerces a FLStock object into a FLFleet object. This method 
  is called by typing as(x,"FLFleet"). This will first coerce the FLStock object into a
  FLCatch object, with catch.q equal to 1, and then create a FLFleet object with an effort 
  slot equal to max(harvest(x)) so that back calculation of harvest as the product of  
  effort, catchability and selectivity is consistent with harvest(x).    }
\item{}{\code{ signature(from=NULL,to=FLFleet,strict=missing) }}
\item{\link{iter}(FLCore) :}{return the ith iteration of a FLFleet object. The value is of class FLFleet also. }
\item{}{\code{ signature(object="FLFleet") }}
\item{\link{revenue}(FLCore) :}{describe...}
\item{}{\code{ signature(fill...) }}
\item{}{\code{ signature(fill...) }}
\item{\link[methods]{summary}(methods) :}{returns the summary of the FLFleet object}
\item{}{\code{ signature(fill...) }}
}}
\author{The FLR Team}
\seealso{ \link{catch}, \link{catch.n}, \link{catch.wt}, \link{catch.sel},\link{catch.q},
\link[base]{coerce}, \link{discards}, \link{discards.n}, \link{discards.sel}, \link{discards.wt},
\link{landings}, \link{landings.n}, \link{landings.sel}, \link{landings.wt}, \link{price}, \link{FLComp} }
\examples{
data(bt4)

catch(bt4, catch="PLE")   # returns a FLQuants object across metiers
catch(bt4,met="TBB")   # returns a FLQuants object across catches
catch(bt4,met="TBB", catch="ple") # returns a FLQuant

effort(bt4) # a FLFleet slot
effshare(bt4@metiers[[1]]) # a FLMetier slot for metier 1
# => metier(bt4,met="OTH") is missing...exist but not in the NAMESPACE?
catch(bt4@metiers[[1]]@catches[[1]]) # a FLCatch slot for metier 1 and catch 1
}
\keyword{classes}
