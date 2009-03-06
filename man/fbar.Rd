\name{fbar}
\alias{fbar}
\alias{fbar-methods}
\alias{fbar,FLStock-method}
\alias{fbar,FLBiol-method}
\title{Calculates mean harvest rate or fishing mortality}
\description{
  The mean harvest rate of fishing mortality
}
\usage{
fbar(object, ...)
}
\arguments{
  \item{object}{An FLStock or FLBiol object}
  \item{...}{Any extra arguments, currently unused}
}
\details{
  The average fishing mortality for the years between \emph{minfbar} and \emph{maxfbar},
  as found in the \code{range} slot, is returned.
}
\value{
An object of class \code{\link{FLQuant-class}}
}
\author{FLR Team}
\examples{
data(ple4)
fbar(ple4)
}
\keyword{methods}
