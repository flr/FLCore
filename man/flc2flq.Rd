\name{flc2flq}
\docType{methods}
\alias{flc2flq}
\alias{flc2flq-methods}
\alias{flc2flq,FLCohort-method}
\title{Coerce FLCohort into FLQuant.}
\description{
Coerces \code{FLCohort} objects into \code{FLQuant} objects. It's also implemented with \code{seAs}, to be used like \code{as(flcobject, "FLQuant")} and it will be deprecated in the near future.
}
\section{Generic function}{
flc2flq(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLCohort) :}{Coerce FLCohort into FLQuant.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
data(ple4)
flc <- FLCohort(catch.n(ple4))
flq <- flc2flq(flc)
all.equal(flq, catch.n(ple4))
}
\keyword{methods}
