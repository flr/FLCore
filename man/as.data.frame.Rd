%$Id$
\name{as.data.frame}
\docType{methods}
\alias{as.data.frame-FLCore}
\alias{as.data.frame,FLArray,missing,missing-method}
\alias{as.data.frame,FLQuant,missing,missing-method}
\alias{as.data.frame,FLCohort,missing,missing-method}
\alias{as.data.frame,FLComp,missing,missing-method}
\alias{as.data.frame,FLCatch,missing,missing-method}
\alias{as.data.frame,FLFleet,missing,missing-method}
\alias{as.data.frame,FLQuants,missing,missing-method}
\alias{as.data.frame,FLCohorts,missing,missing-method}
\alias{as.data.frame,FLPar,ANY,ANY-method}
\alias{as.data.frame,FLCohort,ANY,ANY-method}
\title{Method as.data.frame}
\description{
	This method converts an FLQuant or any other FLR object composed of FLQuants into
	a \code{\link[base]{data.frame}}.

	For a single \link{FLQuant}, the \code{data.frame} returned has 7 columns:  \code{quant},
	\code{year}, \code{unit}, \code{season}, \code{area}, \code{iter} and \code{data}.
	The last column contains the actual values stored in the original object, while the
	first six contain the corresponding dimensions. The \code{year} and \code{data} columns
	are of class \link[base]{numeric}, while the other five are of class \link[base]{factor}.

	When converting an \code{\link{FLCohort-class}} object, the \code{year} column is 
  substituted by \code{cohort}.

	The \code{data.frame} returned for complex objects, i.e. those the inherit from class
	\code{\link{FLComp}}, has an extra column, \code{slot}, that holds the name of the
	slot in the original object.

	The data.frame obtained from an \code{\link{FLQuants-class}} object also has an extra 
  column, named \code{qname}, that refers to the name of each \code{FLQuant} object in 
  the list.
  This column is named \code{cname} when an \code{\link{FLCohorts-class}} object is
  converted.

	Objects of class \link{FLQuants} can also be converted into a wide-format table, where
	data from the list elements are placed in separate colums, using
	\link{model.frame,FLlst-method}.
}
\section{Generic function}{
 as.data.frame(x, row.names, optional) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuant,row.names=ANY,optional=ANY) :}{Converts an FLQuant into a
	data.frame}
\item{ signature(x=FLComp,row.names=missing,optional=missing) :}{Converts objects of 
	any class inheriting from FLComp into a data.frame}
\item{ signature(x=FLCatch,row.names=missing,optional=missing) :}{Converts objects of 
	class FLCatch into a data.frame}
\item{ signature(x=FLQuants,row.names=missing,optional=missing) :}{Converts objects of 
	class FLQuants into a data.frame}
\item{ signature(x=FLCohorts,row.names=missing,optional=missing) :}{Converts objects of 
	class FLCohorts into a data.frame}
\item{ signature(x=FLPar,row.names=ANY,optional=ANY) :}{Converts objects of 
	class FLPar into a data.frame}
\item{ signature(x=FLCohort,row.names=ANY,optional=ANY) :}{Converts objects of 
	class FLCohort into a data.frame}
}}
\author{The FLR Team}
\seealso{ \link[base]{as.data.frame}, \link[stats]{model.frame}, \link{model.frame,FLlst-method}}
\examples{
data(ple4)
fdf <- as.data.frame(catch.n(ple4))
head(fdf)
summary(fdf)

sdf <- as.data.frame(ple4)
head(sdf)
}
\keyword{methods}
