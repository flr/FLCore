\name{FLCohort}
\docType{class}
\alias{FLCohort-class}
\alias{FLCohort}
\alias{FLCohort-methods}
\alias{FLCohort,FLQuant-method}
\alias{FLCohort,FLCohort-method}

\title{Class FLCohort}
\description{
This class represents cohorts in columns. It simply shifts the typical matrix representation where cohorts are found on the diagonals, into a matrix where cohorts are found in columns. It is very usefull for all analysis that want to make use of cohorts instead of years.
}
\section{Slots}{\describe{
\item{.Data}{Internal S4 data representation. \code{array}.}
\item{units}{The data units in some understandable metric. \code{character}}
}}
\section{Extends}{\describe{
FLArray
array
}}

\section{Constructor}{
\describe{
\item{}{The \code{FLCohort(object)} constructor method allows simple creation of new FLCatch with the methods described below.
}
\item{ signature(object=FLQuant) :}{Creates a FLCohort object from a FLQuant object. It simply shifts the matrix so that cohorts instead of years are located in the columns (second dimensions) of the array.}
}}
\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Subset method}
\item{}{\code{ signature(x=FLCohort,i=ANY,j=ANY,drop=missing) }}
\item{\link[base]{as.data.frame}(base) :}{Coerce to data.frame.}
\item{}{\code{ signature(x=FLCohort,row.names=ANY,optional=ANY) }}
\item{\link{bubbles}(FLCore) :}{Bubbles plot.}
\item{}{\code{ signature(x=formula,data=FLCohort) }}
\item{\link{ccplot}(FLCore) :}{Catch curves plot.}
\item{}{\code{ signature(x=formula,data=FLCohort) }}
\item{\link{flc2flq}(FLCore) :}{Coerce to FLQuant (deprecated).}
\item{}{\code{ signature(object=missing) }}
\item{\link{FLCohort}(FLCore) :}{Creator method based on FLQuant objects.}
\item{}{\code{ signature(object=FLQuant) }}
\item{\link[graphics]{plot}(graphics) :}{Simple plot}
\item{}{\code{ signature(x=FLCohort,y=ANY) }}
\item{\link{quant}(FLCore) :}{Extract the quant dimension definition.}
\item{}{\code{ signature(object=missing) }}
\item{\link{trim}(FLCore) :}{Subset based on limiting dimnames.}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{units}(base) :}{Extract the information about data units.}
\item{}{\code{ signature(x=missing) }}
\item{\link[base]{units<-}(base) :}{Replace data units information.}
\item{}{\code{ signature(x=FLCohort,value=character) }}
\item{\link[lattice]{xyplot}(lattice) :}{Lattice's xyplot method.}
\item{}{\code{ signature(x=formula,data=FLCohort) }}
}}
\author{The FLR Team}
\seealso{ \link{[}, \link{as.data.frame}, \link{bubbles}, \link{ccplot}, \link{FLCohort,FLQuant-method}, \link{flc2flq}, \link[graphics]{plot}, \link{quant}, \link{trim}, \link{units}, \link{units<-,FLCohort,character-method}, \link[lattice]{xyplot}, \link[base]{array}}
\examples{
data(ple4)
flq <- catch.n(ple4)
flc <- FLCohort(flq)
plot(trim(flc, cohort=1960:2000))
}
\keyword{classes}

