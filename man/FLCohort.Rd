% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericMethods.R, R/classesArr.R, R/FLCohort.R
\docType{class}
\name{FLCohort}
\alias{FLCohort}
\alias{FLCohort-methods}
\alias{FLCohort-class}
\alias{FLCohort,FLQuant-method}
\alias{FLCohort,FLCohort-method}
\alias{FLCohort,array-method}
\alias{FLCohort,vector-method}
\alias{FLCohort,missing-method}
\title{Class FLCohort}
\usage{
FLCohort(object, ...)

\S4method{FLCohort}{FLQuant}(object, ...)

\S4method{FLCohort}{FLCohort}(object, units = units(object))

\S4method{FLCohort}{array}(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

\S4method{FLCohort}{vector}(
  object,
  dim = c(length(object), rep(1, 5)),
  dimnames = "missing",
  units = "NA",
  iter = 1
)

\S4method{FLCohort}{missing}(object, dim = rep(1, 6), dimnames = "missing", units = "NA", iter = 1)
}
\arguments{
\item{object}{Input numeric object}

\item{...}{Additonal arguments}
}
\description{
A class for modelling cohorts.
}
\details{
This class represents cohorts in columns. It simply shifts the typical
matrix representation where cohorts are found on the diagonals, into a
matrix where cohorts are found in columns. It is very usefull for all
analysis that want to make use of cohorts instead of years.
}
\section{Slots}{
 \describe{ \item{.Data}{Internal S4 data representation.
\code{array}.} \item{units}{The data units in some understandable metric.
\code{character}} }
}

\section{Constructor}{

Objects of this class are generally constructed from an \linkS4class{FLQuant}
object.
}

\examples{

data(ple4)
flq <- catch.n(ple4)
flc <- FLCohort(flq)
plot(trim(flc, cohort=1960:2000))

}
\seealso{
\link{[}, \link{as.data.frame}, \link{bubbles}, \link{ccplot},
\link{FLCohort,FLQuant-method}, \link{flc2flq}, \link[graphics]{plot},
\link{quant}, \link{trim}, \link{units},
\link{units<-,FLCohort,character-method}, \link[lattice]{xyplot},
\link[base]{array}
}
\author{
The FLR Team
}
\keyword{classes}
