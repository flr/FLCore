\name{var}
\docType{methods}
\alias{var,FLPar,missing,missing,missing-method}
\alias{var,FLPar-method}
\title{Variance of an FLPar}
\description{\code{var} computes the variance of an \code{\link{FLPar}} object along the 
  first dimension (\code{iter}) returning a value for each column (\code{param})

  By default, arguments \code{na.rm} and \code{use} have values of \code{FALSE} and 
  \code{'all.obs'} respectively. See the \code{\link[stats]{var}} help page for more 
  information on possible argument values.
}
\section{Generic function}{
 var(x, y, na.rm, use) 
}
\section{Methods}{\describe{
\item{signature(x=FLPar,y=missing,na.rm=missing,use=missing) :}{Returns the variance of each parameter, computed along the \emph{iter} dimension.}
}}
\author{The FLR Team}
\seealso{\code{\link[stats]{var}}, \code{\link{FLPar-class}} }
\examples{
flp <- FLPar(rnorm(200), params=c('a', 'b'))
var(flp)}
\keyword{methods}
