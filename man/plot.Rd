\name{plot}
\docType{methods}
\alias{plot,FLQuant,missing-method}
\alias{plot,FLQuantPoint,missing-method}
\alias{plot,FLPar,missing-method}
\alias{plot,FLStock,missing-method}
\alias{plot,FLStocks,missing-method}
\alias{plot,FLStocks,FLPar-method}
\alias{plot,FLBiol,missing-method}
\alias{plot,FLCohort,missing-method}
\alias{plot,FLIndex,missing-method}
\alias{plot,FLIndices,missing-method}
\alias{plot,FLSR,missing-method}
\title{Method plot}
\description{
  Standard plot methods for every FLR class. FLR plot methods are based on
  \code{\link[lattice]{lattice}}, and attempt to show a general view of the object 
  contents.

  Users are encouraged to write their own plotting code make use of the overloaded
  \code{\link[lattice]{lattice}} methods, for example \code{\link[lattice]{xyplot}} or
  \code{\link[lattice]{bwplot}}. See also \code{\link{lattice-FLCore}}.
}
\section{Generic function}{
 plot(x,y) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant,y=missing) :}{Plot of an \emph{FLQuant} conditioned on all dimension of length > 1.}
\item{ signature(x=FLQuantPoint,y=missing) :}{Box and whiskers plot of the yearly time series, conditioned on all dimension of length > 1.}
\item{ signature(x=FLPar,y=missing) :}{Densityplot per parameter.}
\item{ signature(x=FLStock,y=missing) :}{Times series of catch and landings, recruitment, harvest and spawning stock biomass.}
\item{ signature(x=FLStocks,y=missing) :}{Times series of catch, recruitment, harvest and spawning stock biomass.}
\item{ signature(x=FLBiol,y=missing) :}{Time series of SSB and recruitment.}
\item{ signature(x=FLCohort,y=missing) :}{Plot of an \emph{FLQuant} conditioned on all dimension of length > 1.}
\item{ signature(x=FLIndex,y=missing) :}{Either a time series of the standardised index by quant, type='ts', or and splom plot of a log-linear regression between quants, type='splom'.}
\item{ signature(x=FLSR,y=missing) :}{A six-panelled plot showing the model fit, residuals by year, AR(1) residuals, residuals by SSB, residuals by estimated recruits and a normal Q-Q plot.}
}}
\author{The FLR Team}
\seealso{ \link[graphics]{plot} }
\examples{
data(ple4)
data(ple4.biol)

# FLQuant
plot(catch.n(ple4)[, 1:20])
plot(catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)

# FLStock
plot(ple4)

# FLBiol
plot(ple4.biol)
}
\keyword{methods}
