\name{FLMetier}
\docType{methods}
\alias{FLMetier}
\alias{FLMetier-methods}
\alias{FLMetier,FLCatch-method}
\alias{FLMetier,FLCatches-method}
\alias{FLMetier,FLQuant-method}
\alias{FLMetier,missing-method}
\title{Method FLMetier}
\description{
   The \code{FLMetier} constructor method allows creation of new FLMetier objects
  from either a FLQuant, a FLCatch or a FLCatches object.
}
\section{Generic function}{
 FLMetier(catches) 
}
\section{Methods}{\describe{
\item{ signature(catches=FLCatch) :}{Creates a FLMetier object from a FLCatch object.}
\item{ signature(catches=FLCatches) :}{Creates a FLMetier object from a FLCatches object.}
\item{ signature(catches=FLQuant) :}{Creates a FLMetier object from a FLQuant object.}
\item{ signature(catches=missing) :}{Creates a FLMetier from scratch}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(8),dim=c(2,2,1,1,1,2))
mycatch <- FLCatch(name="YFT",landings.wt=flq) #creates a FLCatch object
mymetier <- FLMetier(mycatch)
}
\keyword{methods}
