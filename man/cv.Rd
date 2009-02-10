\name{cv}
\docType{methods}
\alias{cv}
\alias{cv-methods}
\alias{cv,FLQuant-method}
\alias{cv,FLModel-method}
\title{Coefficient of Variation of FLR objects with multiple iterations}
\description{
  The Coefficient of Variation of an object with mutiple iterations along the sixth 
  (\code{iter}) dimension can be calculated using \code{cv()}. An object of the same 
  class, with length=1 on the sixth dimension, will be returned.

  CV of \code{x} is calculated as \eqn{\frac{sd(x)}{\hat{x}}}{sd(x)/mean(x)}.

  For objects of class \code{\link{FLModel}}, \code{cv} returns the result of

  \deqn{\frac{\sqrt{diag(\Sigma)}}{\Theta}}{sqrt(diag(sigma))/theta}

  where \eqn{\Sigma}{sigma} is the variance-covariance matrix of the \eqn{\Theta}{theta}
  parameter set.

}
\section{Generic function}{
 cv(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Works along the \code{iter} dimension of an 
  \code{FLQuant}}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(200, 5, 10), dim=c(5,10), iter=100)
cv(flq)
}
\keyword{methods}
