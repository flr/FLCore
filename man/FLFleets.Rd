\name{FLFleets}
\docType{methods}
\alias{FLFleets}
\alias{FLFleets-methods}
\alias{FLFleets,ANY-method}
 \alias{FLFleets,missing-method}
 \alias{FLFleets,list-method}
\title{Method FLFleets}
\description{
 Creator method for a FLFleets object
}
\section{Generic function}{
 FLFleets(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{create a FLFleets object from a set of FLFleet objects}
\item{ signature(object=missing) :}{create a FLFleets object as a void list for future use}
\item{ signature(object=list) :}{create a FLFleets object from a list of FLFleet objects}
}}
\author{The FLR Team}
\seealso{\link{FLFleet} \link{FLMetiers} \link{FLMetier} \link{FLCatches} \link{FLCatch} \link{FLComp} }
\examples{
data(bt4) # get an example FLFLeet object
lst.fleets <- FLFleets(bt4,bt4,bt4) #...or from a set of FLFleet objects

# create from nothing:
fleet1 <- FLFleet(name='fleet1',metiers=FLMetiers(FLMetier(name='met1',  catches=FLCatches(FLCatch()))))
fleet2 <- FLFleet(name='fleet2',metiers=FLMetiers(FLMetier(name='met1',catches=FLCatches(FLCatch()))))
lst.fleets <- FLFleets(fleet1,fleet2) 
summary(lst.fleets)
lapply(lst.fleets,summary)
}
\keyword{methods}
