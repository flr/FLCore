\name{iters}
\docType{methods}
\alias{iters}
\alias{iters-methods}
\alias{iters,FLQuant-method}
\title{Method iters}
\description{
    Displays all the iterations of an \code{FLQuant} object.
}
\section{Generic function}{
 iters(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Displays all the iterations of the object}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
    a <- FLQuant(1:24, dim = c(2,3,1,1,1,4))
    a
    iters(a)
    }
\keyword{methods}
