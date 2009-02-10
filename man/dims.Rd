\name{dims}
\docType{methods}
\alias{dims}
\alias{dims-methods}
\alias{dims,FLQuant-method}
\alias{dims,FLPar-method}
\alias{dims,FLCohort-method}
\alias{dims,FLIndex-method}
\alias{dims,FLComp-method}
\alias{dims,FLFleet-method}
\alias{dims,FLFleets-method}
\title{List with information on object dimensions}
\description{
  \code{dims} return a named list with information on the dimensions and dimension names
  of a given object \code{obj}. The list returned could be extended in the future and 
  currently contains, depending on the class of the object, some of the following:
  \describe{
    \item{quant}{Length of the first dimensions, i.e. number of ages, lengths, etc.}
    \item{min}{First quant}
    \item{max}{Last quant}
    \item{year}{Number of years}
    \item{minyear}{First year in series}
    \item{maxyear}{Last year in series}
    \item{cohort}{Number of cohorts}
    \item{mincohort}{First cohort in series}
    \item{maxcohort}{Last cohort in series}
    \item{unit}{Lengt of the third (\code{unit}) dimension}
    \item{season}{Lengt of the fourth (\code{season}) dimension}
    \item{area}{Lengt of the fifth (\code{area}) dimension}
    \item{iter}{Lengt of the sixth (\code{iter}) dimension}
    \item{metiers}{Names of the metiers found in an \code{FLFleet} or \code{FLFleets} object}
    \item{spp}{Names of the catches found in an \code{FLFleet} or \code{FLFleets} object}
  }
  Values in the returned list are of class \code{numeric}, unless dimnames are strings with
  no numeric translation, in which case the result is \code{NA}. In the case of 
  \code{metiers} and \code{spp}, the result is of class \code{character}.

  Please note that the name of the first element in the returned list changes with the
  name of the first dimension on the input object. Use \code{\link{quant}} to obtain the
  name and extract the relevant element from the result list.
}
\section{Generic function}{
 dims(obj) 
}
\section{Methods}{\describe{
\item{ signature(obj=FLQuant) :}{Describe method}
\item{ signature(obj=FLComp) :}{Describe method}
}}
\author{The FLR Team}
\seealso{\code{\link[base]{dimnames}}, \code{\link{FLQuant}}}
\examples{
flq <- FLQuant(rnorm(96), dim=c(3,8,1,4), quant='age')
dims(flq)

# Number of seasons
dims(flq)$season

# Length of first dimension
dims(flq)[[quant(flq)]]
}
\keyword{methods}
