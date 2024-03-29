% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLStock.R
\name{meanwt}
\alias{meanwt}
\alias{meanwtCatch}
\title{Calculate the mean weight in stock and catch}
\usage{
meanwt(object)

meanwtCatch(object)
}
\arguments{
\item{object}{An age-structured FLStock object}
}
\value{
An FLQuant object
}
\description{
Average weight in the stock numbers or catch-at-age.
}
\examples{
data(ple4)
meanwt(ple4)
meanwtCatch(ple4)
}
\seealso{
\link{FLComp}
}
\author{
The FLR Team
}
\keyword{ts}
