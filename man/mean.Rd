\name{mean}
\docType{methods}
\alias{mean,FLPar-method}
\alias{mean,FLQuant-method}
\title{Method mean}
\description{
  Calculates the arithmetic mean. Can be used directly on an object or with apply etc.
}
\section{Generic function}{
 mean(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLPar) :}{Returns the mean of x}
\item{ signature(x=FLQuantPoint) :}{Returns the mean of x}
}}
\author{The FLR Team}
\seealso{
\link{median}
\link{apply}}
\examples{
flp <- FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
mean(flp)
}
\keyword{methods}
