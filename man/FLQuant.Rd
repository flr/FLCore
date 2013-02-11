\docType{class}
\name{FLQuant}
\alias{FLQuant}
\alias{FLQuant,array-method}
\alias{FLQuant-class}
\alias{FLQuant,FLQuant-method}
\alias{FLQuant,matrix-method}
\alias{FLQuant-methods}
\alias{FLQuant,missing-method}
\alias{FLQuant,vector-method}
\title{FLQuant class for numerical data}
\description{
  The \code{FLQuant} class is a six-dimensional
  \code{\link[base]{array}} designed to store most
  quantitative data used in fisheries and population
  modelling.
}
\details{
  The sixth dimensions are named. The name of the first
  dimension can be altered by the user from its default,
  \code{quant}. This could typically be \code{age} or
  \code{length} for data related to natural populations.
  The only name not accepted is 'cohort', as data
  structured along cohort should be stored using the
  \code{\link{FLCohort}} class instead. Other dimensions
  are always names as follows: \code{year}, for the
  calendar year of the datapoint; \code{unit}, for any kind
  of division of the population, e.g. by sex;
  \code{season}, for any temporal strata shorter than year;
  \code{area}, for any kind of spatial stratification; and
  \code{iter}, for replicates obtained through bootstrap,
  simulation or Bayesian analysis.

  In addition, \code{FLQuant} objects contain a
  \code{units} attribute, of class
  \code{\link[base]{character}}, intended to contain the
  units of measurement relevant to the data.
}
\section{Slots}{
  \describe{ \item{.Data}{A 6-D array for numeric data.
  \code{array}.} \item{units}{Units of measurement.
  \code{character}.} }
}
\examples{
# creating a new FLQuant
flq <- FLQuant()
flq <- FLQuant(1:10, dim=c(2,5))
summary(flq)

# Vectors are used column first...
dim(FLQuant(1:10))
# ...while matrices go row first.
dim(FLQuant(matrix(1:10)))

FLQuant(matrix(rnorm(100), ncol=20))

FLQuant(array(rnorm(100), dim=c(5,2,1,1,1,10)))
FLQuant(array(rnorm(100), dim=c(5,2)), iter=10)

# working with FLQuant objects
flq <- FLQuant(rnorm(200), dimnames=list(age=1:5, year=2000:2008), units='diff')
summary(flq)

flq[1,]
flq[,1]
flq[1,1] <- 0

units(flq)
quant(flq)

xyplot(data ~ year, data=flq, type='b', main='FLQ Test Plot', groups=age,
  ylab='diff', xlab='', pch=19, auto.key=TRUE)
}
\author{
  The FLR Team
}
\seealso{
  \link[base]{[}, \link[base]{[<-}, \link[base]{apply},
  \link[methods]{Arith}, \link[base]{as.data.frame},
  \link{as.FLQuant}, \link[lattice]{barchart},
  \link{bubbles}, \link[lattice]{bwplot}, \link{catch<-},
  \link{catch.n<-}, \link{catch.wt<-},
  \link[methods]{coerce}, \link{cv}, \link{dimnames<-},
  \link{dims}, \link{discards<-}, \link{discards.n<-},
  \link{discards.wt<-}, \link[lattice]{dotplot}, \link{E},
  \link{fec<-}, \link{FLCatch}, \link{FLCohort},
  \link{FLMetier}, \link{FLQuant}, \link{FLQuantPoint},
  \link{FLSR}, \link{harvest<-}, \link[lattice]{histogram},
  \link{iter}, \link{iter<-}, \link{iters},
  \link{landings<-}, \link{landings.n<-},
  \link{landings.wt<-}, \link{m<-}, \link{n<-},
  \link[base]{names}, \link[graphics]{plot},
  \link{price<-}, \link[base]{print}, \link{propagate},
  \link{quant}, \link{quant<-}, \link{quantile},
  \link[stats]{rlnorm}, \link[stats]{rnorm},
  \link[methods]{show}, \link{spwn<-},
  \link[lattice]{stripplot}, \link[base]{summary},
  \link{trim}, \link[base]{units}, \link[base]{units<-},
  \link[stats]{window}, \link{wt<-},
  \link[lattice]{xyplot}, \link[base]{array}
}
\keyword{classes}

