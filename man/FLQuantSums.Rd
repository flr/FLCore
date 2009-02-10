%$Id$
\name{FLQuantSums}
\docType{methods}
% quant
\alias{quantSums}
\alias{quantSums-methods}
\alias{quantSums,FLQuant-method}
\alias{quantMeans}
\alias{quantMeans-methods}
\alias{quantMeans,FLQuant-method}
\alias{quantVars}
\alias{quantVars-methods}
\alias{quantVars,FLQuant-method}
% year
\alias{yearSums}
\alias{yearSums-methods}
\alias{yearSums,FLQuant-method}
\alias{yearMeans}
\alias{yearMeans-methods}
\alias{yearMeans,FLQuant-method}
\alias{yearVars}
\alias{yearVars-methods}
\alias{yearVars,FLQuant-method}
% unit
\alias{unitSums}
\alias{unitSums-methods}
\alias{unitSums,FLQuant-method}
\alias{unitMeans}
\alias{unitMeans-methods}
\alias{unitMeans,FLQuant-method}
\alias{unitVars}
\alias{unitVars-methods}
\alias{unitVars,FLQuant-method}
% season
\alias{seasonSums}
\alias{seasonSums-methods}
\alias{seasonSums,FLQuant-method}
\alias{seasonMeans}
\alias{seasonMeans-methods}
\alias{seasonMeans,FLQuant-method}
\alias{seasonVars}
\alias{seasonVars-methods}
\alias{seasonVars,FLQuant-method}
% area
\alias{areaSums}
\alias{areaSums-methods}
\alias{areaSums,FLQuant-method}
\alias{areaMeans}
\alias{areaMeans-methods}
\alias{areaMeans,FLQuant-method}
\alias{areaVars}
\alias{areaVars-methods}
\alias{areaVars,FLQuant-method}
% iter
\alias{iterMeans}
\alias{iterMeans-methods}
\alias{iterMeans,FLQuant-method}
\alias{iterVars}
\alias{iterVars-methods}
\alias{iterVars,FLQuant-method}
% dim
\alias{dimSums}
\alias{dimSums-methods}
\alias{dimSums,FLQuant-method}
\alias{dimMeans}
\alias{dimMeans-methods}
\alias{dimMeans,FLQuant-method}
\alias{dimVars}
\alias{dimVars-methods}
\alias{dimVars,FLQuant-method}


\title{Methods to compute sums, means and vars of FLQuant objects}
\description{
	This set of methods computes three different summaries (sum, mean and variance) of an
	\code{FLQuant} object along each of the six dimensions (quant, year, unit, season, area,
	or iter). Three methods (\code{dimSums}, \code{dimMeans} and \code{dimVars}) operate
	by default over the second to fifth dimensions (\code{unit}, \code{season} and 
	\code{area}).

	These methods simply encapsulate a call to \code{\link[base]{apply}} with the 
  corresponding dimension and function.

	Sums are not calculated for the \code{iter} dimension, as it is used to store multiple 
	replicates of a given array of values.

	Methods to operate over the first dimension refer to it as the \code{quant} dimension,
	regardless of the actual name used in the object.

	The output object will have length=1 on the selected dimension.
}
\section{Generic function}{
 quantSums(x), quantMeans(x), quantVars(x) 

 yearSums(x), yearMeans(x), yearVars(x) 

 unitSums(x), unitMeans(x), unitVars(x) 

 seasonSums(x), seasonMeans(x), seasonVars(x) 

 areaSums(x), areaMeans(x), areaVars(x) 

 iterMeans(x), iterVars(x) 

 dimSums(x), dimMeans(x), dimVars(x) 
}
\section{Method arguments}{\describe{
\item{\code{x} :}{an object of a class for which this method has been defined.}
\item{\code{na.rm} :}{a logical indicating whether \code{NA}s should be deleted from the
	calculations. Defaults to \code{TRUE}.}
\item{\code{dim} :}{numeric, the dimensions over which \code{dimSums}, \code{dimMeans} or 
	\code{dimVars} should operate. Defaults to \code{c(1:2,6)}.}
}}
\section{Methods}{\describe{
\item{ signature(x=FLQuant) :}{Computes a given summary statistic over a certain dimension 
	of an FLQuant.}
}}
\author{The FLR Team}
\seealso{ \link{FLQuant}, \link[base]{sum}, \link[base]{mean}, \link[stats]{var}}
\examples{
 flq <- FLQuant(rnorm(4000), dim=c(5,10,2,2,2,10), quant='age')
 quantSums(flq)
 quantMeans(flq)
 yearSums(flq)
 iterMeans(flq)
 dim(quantSums(flq))}
\keyword{methods}
