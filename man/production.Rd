% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericMethods.R, R/FLStock.R
\name{production}
\alias{production}
\alias{production,FLStock-method}
\title{Returns the computed yearly production}
\usage{
production(object, ...)

\S4method{production}{FLStock}(object, what = "ssb", ...)
}
\arguments{
\item{object}{An object with biomass and catch data.}

\item{what}{One of the production options: "ssb", "biomass", or "exploitation".}
}
\value{
The production by year, of class FLQuant.
}
\description{
Returns the computed yearly production
}
\details{
Production can be calculated for an FLStock based on the spawning stock
biomass ("ssb"), total biomass ("biomass"), or exploitation ("exploitation").
}
\examples{
data(ple4)
# For SSB
production(ple4, "ssb")
# For total biomass
production(ple4, "biomass")
}
\author{
Laurie Kell (Sea++), Iago Mosqueira (WMR)
}
\keyword{methods}
