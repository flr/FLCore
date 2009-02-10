\name{quantile}
\docType{methods}
\alias{quantile,FLQuant-method}
\alias{quantile,FLQuantPoint-method}
\title{Method quantile}
\description{
  Quantiles for \code{\link{FLQuant-class}} objects can be obtained with this method. 
  Default quantiles returned are \code{seq(0, 1, 0.25)}, but they can be specified using
  the \code{probs} argument. The returned \code{\link{FLQuant-class}} object uses the 
  sixth dimension (\emph{iter}) to store the requested quantiles, with appropriate 
  dimnames.

  For objects of class \code{\link{FLQuantPoint-class}}, quantile is merely an accessor
  for two elements of the sixth dimension, \code{lowq} and \code{uppq}. You could 
  use the \code{\link{lowq}} and \code{\link{uppq}} methods instead.
}
\section{Generic function}{
 quantile(x, ...) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Describe method}
\item{ signature(x=FLQuantPoint) :}{Describe method}
}}
\author{The FLR Team}
\seealso{\link[stats]{quantile}, \link{FLQuant-class}, \link{FLQuantPoint-class} }
\examples{
# Normally distributed FLQuant, with log-normal random mean and fixed sd of 20
flq <- rnorm(100, FLQuant(rlnorm(20), dim=c(2,10)), 20)

# obtains all standard quantiles (0, 0.25, 0.5, 0.75 and 1)
quantile(flq)
# select one of them by name
quantile(flq)[,,,,,'0.75']
# calculates the 0.05 quantile only
quantile(flq, 0.05)

# creates an FLQuantPoint from previous FLQuant
flp <- FLQuantPoint(flq)
# return each of the two quantiles (025 and 0.75)
quantile(flp, 0.25)
quantile(flp, 0.75)
}
\keyword{methods}
