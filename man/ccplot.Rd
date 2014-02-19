\name{ccplot}
\docType{methods}
\alias{ccplot}
\alias{ccplot-methods}
\alias{ccplot,formula,FLCohort-method}
\title{Catch-curves plot}
\description{
Catch-curves are essential to explore the mortality carried out on a stock. It shows the trends on different cohorts by age.
}
\section{Generic function}{
ccplot(x,data) 
}
\section{Methods}{\describe{
\item{ signature(x=formula,data=FLCohort) :}{Trends on cohorts by age.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
data(ple4)
ccplot(data~age, data=FLCohort(ple4@catch.n), type="l")
}
\keyword{methods}
