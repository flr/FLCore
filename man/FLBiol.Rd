\docType{class}
\name{FLBiol}
\alias{FLBiol}
\alias{FLBiol,FLQuant-method}
\alias{FLBiol,missing-method}
\alias{FLBiol-class}
\alias{FLBiol-methods}
\alias{catch.n,FLBiol-method}
\title{Class FLBiol}
\description{
A class for modelling age / length or biomass structured
populations.
}
\details{
The \code{FLBiol} class is a representation of a biological
fish population. This includes information on abundances,
natural mortlity and fecundity.
}
\section{Slots}{
  \describe{ \item{n}{for the numbers in the population.
  \code{FLQuant}.} \item{m}{ for the mortality rate of the
  population. \code{FLQuant}.} \item{wt}{ for the weight of
  the population. \code{FLQuant}.} \item{fec}{ for the
  fecundity/maturity/per capita birth rate.
  \code{FLQuant}.} \item{spwn}{ for the proportion of
  mortality before spawning/birth. \code{FLQuant}.}
  \item{name}{ name of the object. \code{character}.}
  \item{desc}{ brief description of the object.
  \code{character}.} \item{range}{ range of the dimensions.
  \code{numeric}.} }
}
\examples{
# An FLBiol example dataser
data(ple4.biol)

summary(ple4.biol)
}
\author{
The FLR Team
}
\seealso{
\link{as.FLBiol}, \link{as.FLSR}, \link[methods]{coerce},
\link[graphics]{plot}, \link{ssb}
}
\keyword{classes}

