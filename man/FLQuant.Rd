% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericMethods.R, R/classesArr.R, R/FLQuant.R
\docType{class}
\name{FLQuant}
\alias{FLQuant}
\alias{FLQuant-methods}
\alias{FLQuant-class}
\alias{FLQuant,FLQuant-method}
\alias{FLQuant,missing-method}
\alias{FLQuant,vector-method}
\alias{FLQuant,array-method}
\alias{FLQuant,matrix-method}
\title{FLQuant class for numerical data}
\usage{
FLQuant(object, ...)

\S4method{FLQuant}{missing}(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1
)

\S4method{FLQuant}{vector}(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

\S4method{FLQuant}{array}(
  object,
  dim = rep(1, 6),
  dimnames = "missing",
  quant = NULL,
  units = "NA",
  iter = 1,
  fill.iter = TRUE
)

\S4method{FLQuant}{matrix}(object, dim = lapply(dimnames, length), dimnames = "missing", ...)

\S4method{FLQuant}{FLQuant}(
  object,
  quant = attributes(object)[["quant"]],
  units = attributes(object)[["units"]],
  dimnames = attributes(object)[["dimnames"]],
  iter = dim(object)[6],
  fill.iter = TRUE,
  dim = attributes(object)[["dim"]]
)
}
\arguments{
\item{object}{Input numeric object}

\item{...}{Additional arguments}

\item{dim}{Vector of dimension lengths}

\item{dimnames}{List of dimension names}

\item{quant}{Character vector for name of first dimension}

\item{units}{Character vctor of units of measurement, see \link[FLCore]{uom}}

\item{iter}{Number of iterations, i.e. length of the 6th dimension}

\item{fill.iter}{Should iterations be filled with the same content as the first?}
}
\description{
The \code{FLQuant} class is a six-dimensional \code{\link[base]{array}}
designed to store most quantitative data used in fisheries and population
modelling.
}
\details{
The six dimensions are named. The name of the first dimension can be
altered by the user from its default, \code{quant}. This could typically be
\code{age} or \code{length} for data related to natural populations. The
only name not accepted is 'cohort', as data structured along cohort should
be stored using the \code{\link{FLCohort}} class instead. Other dimensions
are always names as follows: \code{year}, for the calendar year of the
datapoint; \code{unit}, for any kind of division of the population, e.g. by
sex; \code{season}, for any temporal strata shorter than year; \code{area},
for any kind of spatial stratification; and \code{iter}, for replicates
obtained through bootstrap, simulation or Bayesian analysis.

In addition, \code{FLQuant} objects contain a \code{units} attribute, of
class \code{\link[base]{character}}, intended to contain the units of
measurement relevant to the data.
}
\section{Slots}{
 \describe{
\item{.Data}{A 6-D array for numeric data. \code{\link[base]{array}}.}
\item{units}{Units of measurement. \code{\link[base]{character}}.} }
}

\section{Validity}{
 \describe{
\item{Dimensions:}{Array must have 6 dimensions}
\item{Content:}{Array must be of class \code{numeric}}
\item{Dimnames:}{Dimensions 2 to 6 must be named "year", "unit", "season", "area" and "iter"}
}
}

\section{Constructor}{

The \code{FLQuant} method provides a flexible constructor for objects of the class.
Inputs can be of class:
\describe{
\item{\code{vector}:}{A numeric vector will be placed along the year dimension by default.}
\item{\code{matrix}:}{A matrix will be placed along dimensions 1 and 2, unless otherwise specified by 'dim'. The matrix dimnames will be used unless overriden by 'dimnames'.}
\item{\link[base]{array}:}{As above}
\item{\link[base]{missing}:}{If no input is given, an empty \code{FLQuant}  (NA) is returned, but dimensions and dimnames can still be specified.} }

Additional arguments to the constructor:
\describe{
\item{units:}{The units of measurement, a \code{\link[base]{character}} string.}
\item{dim:}{The dimensions of the object, a \code{\link[base]{numeric}} vector of length 6.}
\item{dimnames:}{A \code{\link[base]{list}} object providing the dimnames of the array. Only those different from the default ones need to be specified.}
\item{quant:}{The name of the first dimension, if different from 'quant', as a \code{\link[base]{character}} string.} }
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

plot(flq)


FLQuant()
summary(FLQuant())

FLQuant(1:10)

FLQuant(array(rnorm(9), dim=c(3,3,3)))
FLQuant(matrix(rnorm(12), nrow=4, ncol=3))

FLQuant(FLQuant(array(rnorm(9), dim=c(3,3,3)), units='kg'), units='t')
}
\seealso{
\code{\linkS4class{FLQuant}}
}
\author{
The FLR Team
}
\concept{FLCoreClasses}
\keyword{classes}
