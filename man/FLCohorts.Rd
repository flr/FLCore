\name{FLCohorts}
\docType{methods}
\alias{FLCohorts}
\alias{FLCohorts-methods}
\alias{FLCohorts,ANY-method}
\alias{FLCohorts,missing-method}
\alias{FLCohorts,list-method}
\alias{FLCohorts,FLCohorts-method}
\title{Method FLCohorts}
\description{
 Method for creating an object of class \code{FLCohorts}.
}
\section{Generic function}{
 FLCohorts(object, ...) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an FLCohorts object}
\item{ signature(object=missing) :}{Returns an empty FLCohorts object}
\item{ signature(object=list) :}{Returns an FLCohorts object}
}}
\author{The FLR Team}
\seealso{\link{FLlst-class}}
\examples{
coh <- FLCohort(catch.n(ple4))
cos <- FLCohorts(ple4=coh)
}
\keyword{methods}
