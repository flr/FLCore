\name{expand}
\docType{methods}
\alias{expand}
\alias{expand-methods}
\alias{expand,FLArray-method}
\alias{expand,FLQuant-method}
\alias{expand,FLComp-method}
\alias{expand,FLStock-method}
\title{Trim FLR objects using named dimensions}
\description{
  Need to add
}
\section{Generic function}{
 expand(x) 
}
\section{Methods}{\describe{
\item{ signature(x=FLArray) :}{Describe method}
\item{ signature(x=FLComp) :}{Describe method}
\item{ signature(x=FLStock) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
data(ple4)

expand(ple4, year=1957:2003)
}
\keyword{methods}
