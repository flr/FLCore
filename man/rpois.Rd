\docType{methods}
\alias{rpois,numeric,FLQuant-method}
\name{rpois}
\docType{methods}
\alias{rpois,numeric,FLQuant-method}
\title{Method rpois}
\description{
  Generates random numbers following a Poisson distribution. \emph{lambda}, the (non-negative) mean can be specified as an object of class \code{\linkS4class{FLQuant}}.
}
\section{Generic function}{
 rpois(n, lambda) 
}
\section{Methods}{\describe{
\item{ signature(n=numeric, lambda=FLQuant) :}{Generate a Poisson-distributed \code{FLQuant} object with 'n' iters.}
}}
\author{The FLR Team}
\seealso{ \link{rpois}, \link{FLQuant} }
\examples{
   data(ple4)
   rpois(50,lambda=harvest(ple4))
}
\keyword{methods}
