\name{FLFleet}
\docType{methods}
\alias{FLFleet}
\alias{FLFleet-methods}
\alias{FLFleet,FLFleet-method}
\alias{FLFleet,FLMetiers-method}
\alias{FLFleet,FLMetier-method}
\alias{FLFleet,FLCatches-method}
\alias{FLFleet,FLCatch-method}
\alias{FLFleet,FLQuant-method}
\alias{FLFleet,missing-method}
\title{Method FLFleet}
\description{
Creator method for a FLFleet object
}
\section{Generic function}{
 FLFleet(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLMetiers) :}{create a FLFleet object from a FLMetiers object}
\item{ signature(object=FLMetier) :}{create a FLFleet object from a FLMetier object}
\item{ signature(object=FLCatches) :}{create a FLFleet object from a FLCatches object}
\item{ signature(object=FLCatch) :}{create a FLFleet object from a FLCatch object}
\item{ signature(object=FLQuant) :}{create a FLFleet object from a FLQuant object}
\item{ signature(object=missing) :}{create a void FLFleet object for future use}
}}
\author{The FLR Team}
\seealso{ \link{FLMetiers} \link{FLMetier} \link{FLCatches} \link{FLCatch} \link{FLComp} }
\examples{

my.fleet <- FLFleet(name='fleet1',metiers=FLMetiers(FLMetier(name='met1',catches=FLCatches(FLCatch()))))

my.fleet <- FLFleet(FLMetier(name='met1',catches=FLCatches(FLCatch())))

my.fleet <- FLFleet(FLCatches(FLCatch())) # a metier is implicitely created

my.fleet <- FLFleet(FLCatch()) # a metier is implicitely created

my.fleet <- FLFleet()
}
\keyword{methods}
