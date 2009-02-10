\name{FLCatch}
\docType{methods}
\alias{FLCatch}
\alias{FLCatch-methods}
\alias{FLCatch,FLQuant-method}
\alias{FLCatch,missing-method}
\title{Method FLCatch}
\description{
  The \code{FLCatch} constructor method allows simple creation of new FLCatch objects
  from a species name and optional FLQuants.
  
}
\section{Generic function}{
 FLCatch(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Creates a new FLCatch object using the dimensions of
  the FLQuant. If no slot is specified, all slots are filled with NA. If one slot is 
  specified, it will receive the value of the FLQuant, while other slots will be filled 
  with NA. It is also possible to specify several slots receiving different values from 
  different FLQuant, if their dimensions 2 to 6 are identical. The first dimension of the 
  slots \code{catch}, \code{discards} and \code{landings} is by default "all".
}
\item{ signature(object=missing) :}{Creates a new FLCatch whose slots are filled with an empty FLQuant with quant="all" and year=1 }
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
mycatch <- FLCatch(name='PLE')

flq <- FLQuant(rnorm(8),dim=c(2,2,1,1,1,2))
mycatch <- FLCatch(name='PLE',flq)
mycatch <- FLCatch(name='PLE',landings.wt=flq)

flq2 <- FLQuant(rnorm(8)*100,dim=c(2,2,1,1,1,2))
mycatch <- FLCatch(name='PLE',landings.wt=flq,landings.n=flq2)
}
\keyword{methods}
