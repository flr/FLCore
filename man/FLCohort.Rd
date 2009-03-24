\name{FLCohort}
\docType{methods}
\alias{FLCohort}
\alias{FLCohort-methods}
\alias{FLCohort,FLQuant-method}
\alias{FLCohort,FLCohort-method}
\title{Method FLCohort}
\description{
Creator method for the FLCohort class.
}
\section{Generic function}{
 FLCohort(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Creates a FLCohort object from a FLQuant object. It simply shifts the matrix so that cohorts instead of years are located in the columns (second dimensions) of the array.}
}}
\author{The FLR Team}
\seealso{ \link{FLCohort-class} }
\examples{
flq <- catch.n(ple4)
flc <- FLCohort(flq)

dimnames(flc)

dim(flc)

# compare with the original FLQuant dimensions
dim(flq)

}
\keyword{methods}

