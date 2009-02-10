\name{is.FL}
\docType{methods}
% FLBiol, FLQuant, FLStock
\alias{is.FLBiol}
\alias{is.FLQuant}
\alias{is.FLStock}
% FLQuants
\alias{is.FLQuants}
\alias{is.FLQuants-methods}
\alias{is.FLQuants,ANY-method}
% FLBiols
\alias{is.FLBiols}
\alias{is.FLBiols-methods}
\alias{is.FLBiols,ANY-method}
% FLCatches
\alias{is.FLCatches}
\alias{is.FLCatches-methods}
\alias{is.FLCatches,ANY-method}
% FLFleets
\alias{is.FLFleets}
\alias{is.FLFleets-methods}
\alias{is.FLFleets,ANY-method}
% FLIndices
\alias{is.FLIndices}
\alias{is.FLIndices-methods}
\alias{is.FLIndices,ANY-method}
% FLmetiers
\alias{is.FLMetiers}
\alias{is.FLMetiers-methods}
\alias{is.FLMetiers,ANY-method}
% FLStocks
\alias{is.FLStocks}
\alias{is.FLStocks-methods}
\alias{is.FLStocks,ANY-method}
\title{Methods to determine the class of a given object}
\description{
  These methods return \code{TRUE} if the given object is of the corresponding class, and
  \code{FALSE} otherwise.

  These methods should be substituted by calls to \code{\link[methods]{is}} and will very
  likely be deprecated in future releases.
}
\section{Generic function}{
 is.FLQuants(object) 

 is.FLBiols(object) 

 is.FLCatches(object) 

 is.FLFleets(object) 

 is.FLIndices(object) 

 is.FLMetiers(object) 

 is.FLStocks(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Describe method}
}}
\author{The FLR Team}
\seealso{\link[methods]{is}}
\examples{
# This call ...
is.FLQuant(FLQuant())
# ... should be substituted by
is(FLQuant(), 'FLQuant')
}
\keyword{methods}
