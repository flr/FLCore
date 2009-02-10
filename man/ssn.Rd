\name{ssn}
\docType{methods}
\alias{ssn}
\alias{ssn-methods}
\alias{ssn,FLBiol-method}
\title{Method ssn}
\description{
 Returns the Spawning Stock Numbers of \code{\link{FLBiol-class}} objects.
}

\section{Generic function}{
 ssn(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLBiol) :}{
 For a given \code{FLBiol} object, the spawning stock numbers are calculated 
 as follows:

 \deqn{}{SSN = sum(n * fec * exp(-spwn * m))}

 In Leslie matrix type models it is the spawning stock * numbers birthed (realised 
 fecundity) that is the key recruitment driver, not SSB.
}
}}

\author{The FLR Team}
\seealso{ \link{FLComp} \link{FLStock}}
\examples{\dontrun{
 data(ple4.biol)
 ssn(ple4.biol)
}
}
\keyword{methods}
