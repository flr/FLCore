\name{FLQuants}
\docType{methods}
\alias{FLQuants}
\alias{FLQuants-methods}
\alias{FLQuants,ANY-method}
 \alias{FLQuants,missing-method}
 \alias{FLQuants,list-method}
 \alias{FLQuants,FLQuants-method}
\title{Method FLQuants}
\description{
 Method for creating an object of class \code{FLQuants}.
}
\section{Generic function}{
 FLQuants(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an FLQuants object with the given named elements}
\item{ signature(object=missing) :}{Returns an FLQuants object}
\item{ signature(object=list) :}{Returns an FLQuants object with the provided list as its core}
}}
\author{The FLR Team}
\seealso{\link{FLQuants-class},  \link{FLlst-class}, \link{FLQuant-class},}
\examples{
flq <- FLQuant(1:10)
FLQuants(a=flq, b=flq)
}
\keyword{methods}
