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
\alias{[,FLCatch,ANY,ANY-method}
\alias{[,FLCohort,ANY,ANY-method}
\alias{[,FLFleet,ANY,ANY-method}
\alias{[,FLFleet,ANY,missing-method}
\alias{[,FLMetier,ANY,missing-method}
\alias{[,FLIndex,ANY,ANY-method}
% [[
\alias{[[,FLFleet,ANY,missing-method}
\alias{[[,FLMetier,ANY,missing-method}

% [<-
\alias{[<-,FLArray,ANY,ANY,ANY-method}
\alias{[<-,FLComp,ANY,ANY,ANY-method}
\alias{[<-,FLlst,ANY,missing,ANY-method}
\alias{[<-,FLPar,ANY,ANY,ANY-method}
\alias{[<-,FLStock,ANY,ANY,FLStock-method}
\alias{[<-,FLCatch,ANY,ANY,FLCatch-method}
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

  An FLFleet object has three levels (fleet, metier and catch).  The \code{[} and 
  \code{[[} operators can be used to access the different levels of an \code{FLFleet}   
  object (i.e \code{FLMetier} and \code{FLCatch} objects).

  Using the single \code{[} operator returns an object of the same type but with only a
  subset of the metier and catch objects that were in the original object.  For example, 
  using \code{[} on an \code{FLFleet} object returns an \code{FLFleet} object, but with 
  only a selected subset of \code{FLMetier} and \code{FLCatch} objects.  Using \code{[} 
  on an \code{FLMetier} object returns an \code{FLMetier} object, but with only a 
  selected subset of \code{FLCatch} objects.

  More than one metier or catch may be specified.  However, if multiple catches are 
  specified it must be possible to extract all of them from the metiers that are 
  specified.  For example, fleet[1:2,1:2] will extract both the first and second 
  \code{FLCatch} objects from the first and second \code{FLMetier} objects.  If either of 
  the two metiers has only a single catch an error will be returned.

  Using the double \code{[[} operators on an \code{FLFleet} or \code{FLMetier} object 
  returns an object of the next level down.  For example, using \code{[[} on an 
  \code{FLMetier} returns an \code{FLCatch} object.  The \code{[[} operators can be 
  combined to extract a single \code{FLCatch} from an \code{FLFleet} object (see below).

  Objects of class \code{\link{FLIndex-class}} might have a combination of \code{FLQuant} 
  of various lengths along the first dimension. When extracting from an \code{FLIndex} 
  object, indices for the first dimension are ignored for those slots whose first 
  dimensions is of length=1.
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
\item{ signature(x=FLlst,i=ANY,j=missing,drop=missing) :}{Returns a subset of an \code{FLlst} object as an \code{FLlst} object.  This also applies to classes derived from \code{FLlst} including \code{FLStocks} and \code{FLFleets}.  It behaves in a similar way to the list operator \code{[}}
\item{ signature(x=FLPar,i=ANY,j=ANY,drop=missing) :}{Subset an \code{FLPar} object.}
\item{ signature(x=FLStock,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLStock} where all the \code{FLQuant} slots have been subset by quant, year, unit, season, area and iter.}
\item{ signature(x=FLCatch,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLCatch} where all the \code{FLQuant} slots have been subset by quant, year, unit, season, area and iter.}
\item{ signature(x=FLBiol,i=ANY,j=ANY,drop=missing) :}{Returns an \code{FLBiol} where all the \code{FLQuant} slots have been subset by quant, year, unit, season, area and iter.}
\item{ signature(x=FLCohort,i=ANY,j=ANY,drop=missing) :}{Describe method}
\item{ signature(x=FLFleet,i=ANY,j=ANY) :}{Returns an \code{FLFleet} object that contains only the i \code{FLMetier}s and the j \code{FLCatch}es of the \code{FLFleet} object.  i and j are vectors (numerics or characters) that describe the position of the objects in the \code{FLMetiers} or \code{FLCatches} lists.  This enables mutiple \code{FLMetier}s and \code{FLCatch}es to be extracted.}
\item{ signature(x=FLFleet,i=ANY,j=missing) :}{Returns an \code{FLFleet} object that contains only the i \code{FLMetier}s of the \code{FLFleet} object.  i is a vector (numeric or character) that describes the position of the \code{FLMetier} in the \code{FLMetiers} lists.}
\item{ signature(x=FLMetier,i=ANY,j=missing) :}{Returns an \code{FLMetier} that contains the i \code{FLCatch}es of the \code{FLMetier}.  i is a vector (numeric or character) that describes the position of the \code{FLCatch}es in the \code{FLCatches} list.}

% [[<-
\item{ signature(x=FLlst,i=ANY,j=missing,value=missing) :}{Returns the specified element of the list.  For example, for an \code{FLStocks} object, stocks[[1]] will return a single \code{FLStock}.}
\item{ signature(x=FLFleet,i=ANY,j=missing) :}{Returns the ith \code{FLMetier} of the \code{FLFleet}.  i can be a numeric describing the position of the \code{FLMetier} in the \code{FLMetiers} list, or a character string of the catch names.}
\item{ signature(x=FLMetier,i=ANY,j=missing) :}{Returns the ith \code{FLCatch} of the \code{FLMetier}.  i can be a numeric describing the position of the \code{FLCatch} in the \code{FLCatches} list, or a character string of the catch names.  This can be used with the \code{FLFleet} accessor above to extract a single \code{FLCatch} from an \code{FLFleet} object.}

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

# load a fleet
data(bt4)
# extract the first metier
met <- bt4[[1]]
# extract the first catch of that metier
catch <- met[[1]]
# do it all once to get the catch
catch <- bt4[[1]][[1]]
}
\keyword{methods}
