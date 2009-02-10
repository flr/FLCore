\name{rgamma}
\docType{methods}
\alias{rgamma,numeric,FLQuantPoint,missing,missing-method}
\title{Method rgamma}
\description{
Random generation for the Gamma distribution with parameters 'shape' and 'scale'.  'shape' can be of class \code{\link{FLQuantPoint}} in which case 'shape' and 'scale' are set as \eqn{\hat{x}^2 / \sigma^2}{mean^2 / var} and \eqn{\sigma^2 / \hat{x}}{var / mean^2}.
}
\section{Generic function}{
 rgamma(n,shape,rate,scale) 
}
\section{Methods}{\describe{
\item{ signature(n=numeric,shape=FLQuantPoint,rate=missing,scale=missing) :}{Returns an FLQuant for Gamma-distributed values.}
}}
\author{The FLR Team}
\seealso{ \link[stats]{rgamma}, \link{FLQuantPoint-class} }
\examples{
flq <- FLQuant(rnorm(1000,mean=10,sd=2),dim=c(1,10,1,1,1,100))
flqp <- FLQuantPoint(flq)
rgamma(10,shape=flqp)
}

\keyword{methods}
