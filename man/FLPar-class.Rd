\name{FLPar}
\docType{class}
\alias{FLPar-class}
\alias{FLPar}
\alias{FLPar-methods}
\alias{FLPar,array-method}
\alias{FLPar,missing-method}
\alias{FLPar,vector-method}
\alias{FLPar,FLPar-method}
\title{Class FLPar}
\description{
 The \code{FLPar} class is a class for storing the parameters of a model. It is based on 
 the array class which can store Monte Carlo samples and the names of the relevant 
 parameter vectors.

 Methods for this class include subsetting and replacement as they exist for the 
 \code{FLQuant} class. There are methods for extracting statistics of the sample (mean, 
 median etc.) and for plotting the parameter samples.
}
\section{Slots}{\describe{
\item{.Data}{Describe slot. \code{array}.}
\item{units}{Units of measurement. \code{character}.}
}}
\section{Extends}{\describe{
array
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLPar(object)} constructor method allows simple creation of new FLPar with the methods described below.
}
\item{signature(object=array) :}{Describe method}
\item{signature(object=missing) :}{Describe method}
\item{signature(object=vector) :}{Describe method}
}}
\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Subsetting method to access the parameter values.}
\item{}{\code{ signature(x=FLPar,i=ANY,j=ANY,drop=missing) }}
\item{\link[base]{[<-}(base) :}{Replacement method for the parameter values.}
\item{}{\code{ signature(x=FLPar,i=ANY,j=ANY,value=missing) }}
\item{\link[base]{as.data.frame}(base) :}{Creates a data frame from the object.}
\item{}{\code{ signature(x=FLPar,row.names=ANY,optional=ANY) }}
\item{\link[lattice]{densityplot}(lattice) :}{applies the densityplot method from \code{lattice}}
\item{}{\code{ signature(x=formula,data=FLPar) }}
\item{\link[lattice]{histogram}(lattice) :}{applies the histogram method from \code{lattice}}
\item{}{\code{ signature(x=formula,data=FLPar) }}
\item{\link{iter}(FLCore) :}{extracts the relevant iteration for the given parameter sample.}
\item{}{\code{ signature(object=missing) }}
\item{\link{iter<-}(FLCore) :}{replacement method for the parameter iteration.}
\item{}{\code{ signature(object=missing,value=missing) }}
\item{\link[base]{mean}(base) :}{calculates the mean of the parameter samples.}
\item{}{\code{ signature(x=missing) }}
\item{\link[stats]{median}(stats) :}{calculates the median of the parameter samples.}
\item{}{\code{ signature(x=missing,na.rm=missing) }}
\item{\link[graphics]{plot}(graphics) :}{default plot method for \code{FLPar}}
\item{}{\code{ signature(x=FLPar,y=missing) }}
\item{\link[lattice]{splom}(lattice) :}{applies the splom method from \code{lattice}}
\item{}{\code{ signature(x=FLPar,data=missing) }}
\item{\link[base]{summary}(base) :}{Summarises the \code{FLPar} object.}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{units}(base) :}{extracts the units of the parameters.}
\item{}{\code{ signature(x=missing) }}
\item{\link[base]{units<-}(base) :}{replacement method for the units of the object.}
\item{}{\code{ signature(x=FLPar,value=character) }}
\item{\link[stats]{var}(stats) :}{calculates the variance of the parameter samples.}
\item{}{\code{ signature(x=missing,y=missing,na.rm=missing,use=missing) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame}, \link[lattice]{densityplot}, \link[lattice]{histogram}, \link{iter}, \link{iter<-}, \link[base]{mean}, \link[stats]{median}, \link[graphics]{plot}, \link[lattice]{splom}, \link[base]{summary}, \link{units,FLPar-method}, \link{units<-,FLPar,character-method}, \link[stats]{var} }
\examples{
FLPar(rnorm(4), params=c('a','b','c','sigma2'))
}
\keyword{classes}
