\name{range}
\docType{methods}
\alias{range-methods}
\alias{range,FLComp,missing-method}
\alias{range,FLComp-method}
\alias{range,FLlst-method}
\alias{range<-}
\alias{range<--methods}
\alias{range<-,FLComp-method}
\title{Method range}
\description{
  Extraction and modification of the \emph{range} slot from objects of any class
  inheriting from \code{\linkS4class{FLComp}}.
}
\section{Generic function}{
 range(x, i) 
 range<-(x, i, value) 
}
\section{Methods}{\describe{
\item{ signature(x=FLComp, i=missing) :}{Returns the \emph{range} slot.}
\item{ signature(x=FLComp, i=character) :}{Returns the selected element(s) from the \emph{range} slot.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# example FLStock
data(ple4)

range(ple4)

range(ple4, 'plusgroup')

range(ple4, 'plusgroup') <- 14

}
\keyword{methods}
