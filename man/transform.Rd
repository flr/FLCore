\name{transform}
\docType{methods}
\alias{transform,FLComp-method}
\title{Transform elements of a complex FLR object}
\description{
  Modification of individual elements of a complex FLR object can be carried out using
  \code{transform}. A series of named arguments, corresponding to the slots to modify
  can be provided to the method. Existing slots can be referred simply by its name on the
  right handside on the argument expressions (see example below).
}
\section{Generic function}{
 transform(\_data, \dots)
}
\section{Methods}{\describe{
\item{signature(\_data=FLComp) :}{Method for all complex FLR classes that extend 
  \code{\link{FLComp-class}}}
}}
\author{The FLR Team}
\seealso{\code{\link[base]{transform}}}
\examples{
data(ple4)
ple4 <- transform(ple4, m.spwn=m.spwn+0.2)
m.spwn(ple4)
}
\keyword{methods}
