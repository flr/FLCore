\name{bubbles}
\docType{methods}
\alias{bubbles}
\alias{bubbles-methods}
\alias{bubbles,formula,FLQuant-method}
\alias{bubbles,formula,FLQuants-method}
\alias{bubbles,formula,FLCohort-method}
\alias{bubbles,formula,data.frame-method}
\title{Bubbles plot}
\description{
This method plots three dimensional data like matrices by age and year or age-class, very common in fisheries. The bubbles are proportional to the values on the matrix. Note that \code{bubbles} accept an argument \code{bub.scale} to control the relative size of the bubbles. Positive and negative values have separate colours.
}
\section{Generic function}{
 bubbles(x, data) 
}
\section{Methods}{\describe{
\item{ signature(x=formula, data=FLQuant) :}{Produce bubbles plot of the object.}
\item{ signature(x=formula, data=FLQuants) :}{Produce a lattice of bubbles plot of the objects. Commonly used to plot residuals of VPA assessments.}
\item{ signature(x=formula, data=FLCohort) :}{Produce bubbles plot of the object.}
\item{ signature(x=formula, data=data.frame) :}{Produce bubbles plot of the object.}
}}
\author{The FLR Team}
\seealso{ \link[lattice]{lattice}, \code{\linkS4class{FLQuant}}, \code{\linkS4class{FLQuants}},\code{\linkS4class{FLCohort}}}
\examples{
data(ple4)
bubbles(age~year, data=catch.n(ple4))
bubbles(age~year, data=catch.n(ple4), bub.scale=5)
bubbles(age~cohort, data=FLCohort(catch.n(ple4)), bub.scale=5)

qt01 <- log(catch.n(ple4)+1)
qt02 <- qt01+rnorm(length(qt01))
flqs <- FLQuants(qt01=qt01, qt02=qt02)
bubbles(age~year|qname, data=flqs, bub.scale=1)

qt03 <- FLQuant(rnorm(100),dimnames=list(age=as.character(1:10),year=as.character(1:10)))
bubbles(age~year, data=qt03, bub.scale=7, col=c("black","red"))
}
\keyword{methods}
