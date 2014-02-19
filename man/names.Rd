\name{names}
\docType{methods}
\alias{names,FLArray-method}
\alias{names,FLlst-method}
\alias{names,FLPar-method}
\alias{names<-,FLPar,character-method}
\title{Method names}
\description{
 The \code{names} method returns the names of the dimnames of an object. For some classes,
 the names attribute can be modified directly using names<-.
}
\section{Generic function}{
 names(x) 
 names<-(x, value) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Returns the names of the dimnames of x}
\item{ signature(x=FLPar) :}{Returns the names of the dimnames of x}
\item{ signature(x=FLlst) :}{Returns the names of the elements of x}
}}
\author{The FLR Team}
\seealso{ \link[base]{names} }
\examples{
data(ple4)
names(catch.n(ple4))
}
\keyword{methods}
