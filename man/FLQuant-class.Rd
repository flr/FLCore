% Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
% Maintainer: Iago Mosqueira, Cefas
% $Id$
\name{FLQuant}
\docType{class}
\alias{FLQuant-class}
\alias{FLQuant}
\alias{FLQuant-methods}
\alias{FLQuant,missing-method}
\alias{FLQuant,vector-method}
\alias{FLQuant,array-method}
\alias{FLQuant,matrix-method}
\alias{FLQuant,FLQuant-method}

\title{FLQuant class for numerical data}
\description{
  The \code{FLQuant} class is a six-dimensional \code{\link[base]{array}} designed to 
  store most quantitative data used in fisheries and population modelling.

  The sixth dimensions are named. The name of the first dimension can be altered
  by the user from its default, \code{quant}. This could typically be \code{age} or 
  \code{length} for data related to natural populations. The only name not accepted is 
  'cohort', as data structured along cohort should be stored using the 
  \code{\link{FLCohort}} class instead. Other dimensions are always names as follows: 
  \code{year}, for the calendar year of the datapoint; \code{unit}, for any kind of 
  division of the population, e.g. by sex; \code{season}, for any temporal strata shorter 
  than year; \code{area}, for any kind of spatial stratification; and \code{iter}, for 
  replicates obtained through bootstrap, simulation or Bayesian analysis.

  In addition, \code{FLQuant} objects contain a \code{units} attribute, of class 
  \code{\link[base]{character}}, intended to contain the units of measurement relevant to 
  the data.

}
\section{Slots}{\describe{
\item{.Data}{A 6-D array for numeric data. \code{array}.}
\item{units}{Units of measurement. \code{character}.}
}}
\section{Extends}{\describe{
FLArray
array
}}

\section{Constructor}{
\describe{
\item{}{The \code{FLQuant} constructor method allows simple creation of new FLQuant objects from vectors, matrices and arrays.}
\item{}{R standard recyling rules (row first) are applied in most cases, so if an input object smaller than the required dimensions is given, it will be reused. An exception is made for objects of class vector, which will be considered to go along the second dimension (\code{year}) instead of the first one. To recover the standard behaviour, a matrix can be created.}
\item{}{The \code{iter} argument takes preference over \code{dim} but not over \code{dimnames}, so it is preferable to ensure consistency between arguments. \code{dim} and \code{dimnames} must match and an error is thrown otherwise.}
\item{ signature(object=missing) :}{Describe method}
\item{ signature(object=vector) :}{Describe method}
\item{ signature(object=array) :}{Describe method}
\item{ signature(object=matrix) :}{Describe method}
\item{ signature(object=FLQuant) :}{Describe method}
}}
\section{Constructor Arguments}{
  \describe{
  \item{object}{An input object.}
  \item{dim}{The dimensions of the object. A vector of class \code{numeric}.}
  \item{dimnames}{The dimension names, as a \code{list}.}
  \item{quant}{A \code{character} string with the name of the first dimension. Defaulta to 'quant'}
  \item{units}{A \code{character} string with the units of measurement. Defaults to 'NA'}
  \item{iter}{The number of iterations, i.e. the length of the sixth dimension, on the output object.}
  \item{fill.iter}{TRUE}
  }
}
\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Extract elements}
\item{}{\code{ signature(x=FLQuant,i=ANY,j=ANY,drop=missing) }}
\item{\link[base]{[<-}(base) :}{Substitute elements}
\item{}{\code{ signature(x=FLQuant,i=ANY,j=ANY,value=missing) }}
\item{\link[base]{apply}(base) :}{Operate with a function along certain dimensions}
\item{}{\code{signature(X=FLQuant,MARGIN=numeric,FUN=function)}}
\item{\link[methods]{Arith}(methods) :}{Arithmetic operations}
\item{}{\code{ signature(e1=FLQuant,e2=FLQuant) }}
\item{\link[base]{as.data.frame}(base) :}{Convert to a data.frame with 7 columns}
\item{}{\code{ signature(x=FLQuant,row.names=ANY,optional=ANY) }}
\item{\link{as.FLQuant}(FLCore) :}{Convert into an FLQuant}
\item{}{\code{ signature(x=missing) }}
\item{\link[lattice]{barchart}(lattice) :}{Plot a barchart following the given formula}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{bubbles}(FLCore) :}{Plot a bubble following the given formula, with size of the bubbles given by the values in the object}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link[lattice]{bwplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link[methods]{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=NULL,to=FLQuant,strict=missing) }}
\item{\link{cv}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{dimnames<-}(base) :}{Describe method}
\item{}{\code{ signature(x=missing,value=missing) }}
\item{\link{dims}(FLCore) :}{Describe method}
\item{}{\code{ signature(obj=missing) }}
\item{\link{discards<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{discards.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link[lattice]{dotplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{E}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{fec<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{FLCatch}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLCohort}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLMetier}(FLCore) :}{Describe method}
\item{}{\code{ signature(catches=missing) }}
\item{\link{FLQuant}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLQuantPoint}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=missing) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=character) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=function) }}
\item{\link{FLSR}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLQuant,model=formula) }}
\item{\link{harvest<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link[lattice]{histogram}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link{iter}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{iter<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link{iters}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link{landings.wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{m<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{m<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link{n<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link[base]{names}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link[graphics]{plot}(graphics) :}{Describe method}
\item{}{\code{ signature(x=FLQuant,y=missing) }}
\item{\link{price<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLCatch,value=FLQuant) }}
\item{\link[base]{print}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{propagate}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{quant}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{quant<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link{quantile}(stats) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link[stats]{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=FLQuant,sdlog=FLQuant) }}
\item{\link[stats]{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=FLQuant,sdlog=numeric) }}
\item{\link[stats]{rlnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,meanlog=numeric,sdlog=FLQuant) }}
\item{\link[stats]{rnorm}(stats) :}{Describe method}
\item{}{\code{ signature(n=numeric,mean=FLQuant,sd=FLQuant) }}
\item{\link[methods]{show}(methods) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{spwn<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link[lattice]{stripplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
\item{\link[base]{summary}(base) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{trim}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{units}(base) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link[base]{units<-}(base) :}{Describe method}
\item{}{\code{ signature(x=FLQuant,value=character) }}
\item{\link[stats]{window}(stats) :}{Describe method}
\item{}{\code{ signature(x=missing) }}
\item{\link{wt<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLBiol,value=FLQuant) }}
\item{\link[lattice]{xyplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuant) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{[}, \link[base]{[<-}, \link[base]{apply}, \link[methods]{Arith}, \link[base]{as.data.frame}, \link{as.FLQuant}, \link[lattice]{barchart}, \link{bubbles}, \link[lattice]{bwplot}, \link{catch<-}, \link{catch.n<-}, \link{catch.wt<-}, \link[methods]{coerce}, \link{cv}, \link{dimnames<-}, \link{dims}, \link{discards<-}, \link{discards.n<-}, \link{discards.wt<-}, \link[lattice]{dotplot}, \link{E}, \link{fec<-}, \link{FLCatch}, \link{FLCohort}, \link{FLMetier}, \link{FLQuant}, \link{FLQuantPoint}, \link{FLSR}, \link{harvest<-}, \link[lattice]{histogram}, \link{iter}, \link{iter<-}, \link{iters}, \link{landings<-}, \link{landings.n<-}, \link{landings.wt<-}, \link{m<-}, \link{n<-}, \link[base]{names}, \link[graphics]{plot}, \link{price<-}, \link[base]{print}, \link{propagate}, \link{quant}, \link{quant<-}, \link{quantile}, \link[stats]{rlnorm}, \link[stats]{rnorm}, \link[methods]{show}, \link{spwn<-}, \link[lattice]{stripplot}, \link[base]{summary}, \link{trim}, \link[base]{units}, \link[base]{units<-}, \link[stats]{window}, \link{wt<-}, \link[lattice]{xyplot}, \link[base]{array}}
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
\keyword{classes}
