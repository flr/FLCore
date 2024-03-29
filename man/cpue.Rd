% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oem.R
\name{cpue}
\alias{cpue}
\alias{cpue-methods}
\alias{cpue,FLStock,missing-method}
\alias{cpue,FLStock-method}
\title{cpue, a method to generate an observation of a CPUE index of abundance}
\usage{
cpue(object, index, ...)

\S4method{cpue}{FLStock,missing}(
  object,
  sel.pattern = harvest(object),
  effort = units(harvest(object)),
  biomass = TRUE
)
}
\arguments{
\item{object}{The object from which to generate the observation.}

\item{effort}{Units of index to use to mimic effort series in the fishery, "f" or "hr"}

\item{sel}{The selectivity of the survey, defaults to be 1 for all ages.}

\item{mass}{Is the index to be in weight at age?}
}
\value{
An FLQuant for the index of abundance, age-disaggregated
}
\description{
The observation of stock abundance by CPUE series from commercial fleets is an
important step in the generation of management advice that needs to replicated
on an Operating Model during any simulation exercise. This method gemnerates
an observation of biomass or numbers-at-age from an FLstock being used as OM.
}
\examples{

data(ple4)

cpue(ple4)
# Am aggregated biomass CPUE
quantSums(cpue(ple4))

\dontrun{
plot(FLQuants(om=stock(ple4), cpue=quantSums(cpue(ple4)),
  hr=quantSums(cpue(ple4, effort="hr"))))
}
}
\seealso{
\link{FLComp}
}
\author{
Laurie Kell & Iago Mosqueira, FLR Team.
}
\keyword{methods}
