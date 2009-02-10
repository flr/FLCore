\name{names}
\docType{methods}
\alias{names,FLArray-method}
\alias{names,FLlst-method}
\title{Method names}
\description{
 The \code{names} method returns the names of the dimnames of an object
}
\section{Generic function}{
 names(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Returns the names of the dimnames of x}
}}
\author{The FLR Team}
\seealso{ \link[base]{names} }
\examples{
data(ple4)
names(catch.n(ple4))
}
\keyword{methods}
