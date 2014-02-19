\name{Extract}
\docType{methods}
\alias{Extract-FLCore}
% [
\alias{[,FLArray,ANY,ANY-method}
\alias{[,FLArray,array,ANY-method}
\alias{[,FLComp,ANY,ANY-method}
\alias{[,FLlst,ANY,missing-method}
\alias{[,FLPar,ANY,ANY-method}
\alias{[,FLStock,ANY,ANY-method}
\alias{[,FLCohort,ANY,ANY-method}
\alias{[,FLIndex,ANY,ANY-method}
% [[

% [<-
\alias{[<-,FLArray,ANY,ANY,ANY-method}
\alias{[<-,FLComp,ANY,ANY,ANY-method}
\alias{[<-,FLlst,ANY,missing,ANY-method}
\alias{[<-,FLPar,ANY,ANY,ANY-method}
\alias{[<-,FLStock,ANY,ANY,FLStock-method}
% [[<-
\alias{[[<-,FLlst,ANY,missing-method}
% $<-
\alias{$<-,FLlst,character-method}

\title{Extract or Replace Parts of an FLR Object}
\description{
	Operators acting on FLQuant, FLCohort, FLPar, FLComp, and derived classes to extract or
  replace sections of an object.

  Please note the difference between referencing sections of an object by position, using
  values of class \code{numeric}, or by dimnames, of class \code{character}. See examples
  below.

  All classes that are derived from \code{FLComp} (for example, \code{FLStock} and 
  \code{FLBiol}) can be subset along the six dimensions of their \code{FLQuant} slots.

  Classes that are derived from \code{FLlst} (for example, \code{FLStocks} and 
 \code{FLBiols}) can be subset in a similar way to ordinary list objects.

}
\section{Generic function}{
\describe{
  \item{}{[x,i,j,drop]}
  \item{}{[<-(x,i,j,value)}
  \item{}{[[<-(x,i,j,value)}
  \item{}{\$<-(x,name,value)}
}
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLQuant} object, subset along any of the six dimensions (quant, year, unit, season, area and iter).}
\item{ signature(x=FLPar,i=ANY,j=ANY,drop=missing) :}{Subset an \code{FLPar} object.}
\item{ signature(x=FLStock,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLStock} where all the \code{FLQuant} slots have been subset by quant, year, unit, season, area and iter.}
\item{ signature(x=FLBiol,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLBiol} where all the \code{FLQuant} slots have been subset by quant, year, unit, season, area and iter.}
\item{ signature(x=FLCohort,i=ANY,j=ANY,drop=missing) :}{Describe method}

% [[<-
\item{ signature(x=FLlst,i=ANY,j=missing,value=missing) :}{Returns the specified element of the list.  For example, for an \code{FLStocks} object, stocks[[1]] will return a single \code{FLStock}.}

% $
\item{ signature(x=FLlst,name=character,value=missing) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link[base]{Extract} }
\examples{
flq <- FLQuant(rnorm(50), dimnames=list(age=1:5, year=1990:2000, season=1:4))
flq[1,]
flq[,1:5]
flq[,'1990']
flq[1:2,,,c(1,3)]

}
\keyword{methods}
