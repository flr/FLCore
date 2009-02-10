\name{window}
\docType{methods}
\alias{window,FLQuant-method}
\alias{window,FLComp-method}
\alias{window,FLlst-method}
\alias{window,FLFleet-method}
\title{Extract time (year) windows of an FLR object}
\description{
  This method extracts a section of or extends an \code{FLQuant} or other FLR objects 
  along the \code{year} dimension. If a frequency is specified, the new object contains 
  data at only those year steps.

  Although objects of class \code{\link{FLQuant}} do have another temporal dimension,
  \code{season}, currently window only works along the \code{year} dimension. To subset
  along other dimensions, refer to \link{Extract-FLCore}.
}
\section{Generic function}{
 window(x) 
}
\section{Methods}{\describe{
\item{signature(x=FLQuant) :}{Subset along the \code{year} dimension}
\item{signature(x=FLComp) :}{The method is applied to each slot of class \code{FLQuant}.}
\item{signature(x=FLlst) :}{The window method is applied to each element in the list.}
}}
\author{The FLR Team}
\seealso{\link[stats]{window}, \link{Extract-FLCore}}
\examples{
flq <- FLQuant(rnorm(50), dimnames=list(age=1:5, year=1990:2000))
window(flq, start=1995, end=1998)
window(flq, start=1990, end=2010, frequency=2)
}
\keyword{methods}
