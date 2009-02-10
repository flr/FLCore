\name{E}
\docType{methods}
\alias{E}
\alias{E-methods}
\alias{E,FLQuant-method}
\title{Method E}
\description{
 Calculates the \emph{expected} value along the iteration dimension (Monte Carlo mean). 
 This is currently approximated by the sample median.
}
\section{Generic function}{
 E(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Returns the median along the \emph{iter} dimension, as in \code{apply(object, 1:5, median, na.rm=TRUE)}.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- rnorm(100, FLQuant(20, dim=c(2,10,1,1,1,1)), 15)
E(flq)
}
\keyword{methods}
