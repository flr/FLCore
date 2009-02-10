\name{mcf}
\docType{methods}
\alias{mcf}
\alias{mcf-methods}
\alias{mcf,FLComp-method}
\alias{mcf,list-method}
\title{Method mcf}
\description{
 This method makes FLQuants compatible with respect to their dimensionality. Hence, the 
 FLQuants in the returned object all heve the same dimensions, padded with NAs if 
 necessary 
}
\section{Generic function}{
 mcf(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLComp) :}{All FLQuants in an FLComp object are made compatible with respect to their dimensionality }
\item{ signature(object=list) :}{All FLQuants in an list are made compatible with respect to their dimensionality}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
