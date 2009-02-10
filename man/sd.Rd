\name{sd}
\docType{methods}
\alias{sd,FLModel,missing-method}
\title{Standard deviation of an FLModel object}
\description{\code{sd} computes the standard deviation of the parameter estimates in
  an FLModel object, either by calculating the diagonal of the square root of the
  variance-covariance matrix or, if multiple parameter estimates, as the standard
  deviation of each parameter.
}
\section{Generic function}{
 sd(x, na.rm) 
}
\section{Methods}{\describe{
\item{signature(x=FLModel,na.rm=missing) :}{Standard deviation of a FLModel object}
}}
\author{The FLR Team}
\seealso{\code{\link[stats]{sd}}, \code{\link{FLModel}} }
\examples{
data(nsher)
sd(nsher)
}
\keyword{methods}
