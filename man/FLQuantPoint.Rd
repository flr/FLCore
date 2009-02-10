\name{FLQuantPoint}
\docType{methods}
\alias{FLQuantPoint}
\alias{FLQuantPoint-methods}
\alias{FLQuantPoint,FLQuant-method}
\title{Method FLQuantPoint}
\description{
  Objects of class \code{\link{FLQuantPoint-class}} can be created using this method, 
  applied to an object of class \code{\link{FLQuant}} with length two or more along its
  sixth dimension (\emph{iter}).
}
\section{Generic function}{
 FLQuantPoint(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
flqp <- FLQuantPoint(flq)
}
\keyword{methods}
