\name{FLIndices}
\docType{methods}
\alias{FLIndices}
\alias{FLIndices-methods}
\alias{FLIndices,ANY-method}
 \alias{FLIndices,missing-method}
 \alias{FLIndices,list-method}
\title{Method FLIndices}
\description{
 Method for creating an object of class \code{FLIndices}.
}
\section{Generic function}{
 FLIndices(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an FLIndices object}
\item{ signature(object=missing) :}{Returns an empty FLIndices object}
\item{ signature(object=list) :}{Returns an FLIndices object}
}}
\author{The FLR Team}
\seealso{\link{FLIndices-class},  \link{FLlst-class}, \link{FLIndex-class},}
\examples{
ind <- FLIndex(index=FLQuant(1:10))
inx <- FLIndices(first=ind, second=ind)
}
\keyword{methods}
