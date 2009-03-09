\name{harvest}
\docType{methods}
\alias{harvest,FLBiol-method}
\alias{harvest,FLBiol,missing-method}
\title{Harvest calculations for FLBiol}
\description{
  Calculates the fishing mortality (F), based on abundance changes by year and age, 
  and the difference between total mortality (Z) and natural mortality (M), for an object
  of class \code{FLBiol}.
}

\section{Generic function}{
 harvest(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLBiol) :}{Uses the method above to calculate an approximation of harvest rate}
}}
\author{The FLR Team}
\seealso{ \link{FLComp}, \link{FLBiol-class}}
\examples{
flb<-as(ple4, 'FLBiol')
harvest(flb)
}
\keyword{methods}
