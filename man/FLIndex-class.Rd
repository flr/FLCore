\name{FLIndex}
\docType{class}
\alias{FLIndex-class}
\alias{FLIndex}
\alias{FLIndex-methods}
\alias{FLIndex,missing-method}
\alias{FLIndex,FLQuant-method}

\title{Class FLIndex}
\description{
 A class designed to hold data and parameters related to abundance indices 
}
\section{Slots}{\describe{
\item{type}{(\code{character}) Type of index}
\item{distribution}{(\code{character}): Statistical distribution of the index values}
\item{index}{(\code{FLQuant}): Index values}
\item{index.var}{ (\code{FLQuant}): Variance of the index}
\item{catch.n}{ (\code{FLQuant}): Catch numbers used to create the index}
\item{catch.wt}{ (\code{FLQuant}): Catch weight of the index}
\item{effort}{ (\code{FLQuant}): Effort used to create the index}
\item{sel.pattern}{ (\code{FLQuant}): Selection pattern for the index}
\item{index.q}{ (\code{FLQuant}): Catchability of the index}
\item{name}{ (\code{character}): Name of the stock inherited from FLComp-class}
\item{desc}{ (\code{character}): General description of the object inherited from FLComp-class}
\item{range}{ (\code{numeric}):}{Range of the object inherited from FLComp-class}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLIndex(object, ...)} constructor method allows simple creation of new FLIndex with the methods described below.
}
\item{ signature(object=missing) :}{Describe method}
\item{ signature(object=FLQuant) :}{Describe method}

}}

\section{Methods}{\describe{
\item{\link{catch.n}(FLCore) :}{Accessor for the catch.n slot in the object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n<-}(FLCore) :}{Replacement function for the catch.n slot in the object}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link{catch.wt}(FLCore) :}{Accessor for the catch.n slot in the object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt<-}(FLCore) :}{replacement function for the catch.wt slot in the object}
\item{}{\code{ signature(object=FLIndex,value=FLQuant) }}
\item{\link[base]{coerce}(methods) :}{Describe method}
\item{}{\code{ signature(from=NULL,to=FLIndex,strict=missing) }}
\item{\link{computeCatch}(FLCore) :}{Compute the catch in the FLIndex method, as catch.n and catch.wt}
\item{}{\code{ signature(object=missing) }}
\item{\link{dims}(FLCore) :}{returns the dims of the objects}
\item{}{\code{ signature(obj=missing) }}
\item{\link{iter}(FLCore) :}{To extract or modify a subset of the iterations contained in the FLIndex object, the 
  \code{iter} and \code{iter<-} methods can be used}
\item{}{\code{ signature(object=missing) }}
\item{\link[graphics]{plot}(graphics) :}{Plot the FLIndex-class, takes an additional argument type, which defines the type of plot}
\item{}{\code{ signature(x=FLIndex,y=missing) }}
\item{\link{propagate}(FLCore) :}{\code{FLIndex} objects with a single iteration (length of 1 in the sixth dimension) can be extended using the \code{propagate} method. The \code{type} argument
  selects whether the new iterations are filled with the content of the first iteration (\code{type='all'}) or left empty (\code{type='first'}) }
\item{}{\code{ signature(object=missing) }}
\item{\link{summary}(base) :}{summary is a generic function used to produce result summaries of the \code{FLIndex}}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{transform}(base) :}{Transform allows to change a number of slots of class FLQuant to be changed at once}
\item{}{\code{ signature(_data=missing) }}
\item{\link{trim}(FLCore) :}{Subsetting of FLIdex object can be carried out using the dimension names by using 
  \code{trim}. A number of dimension names and selected dimensions are passed to the method and those are used to subset the input object.}
\item{}{\code{ signature(object=missing) }}
\item{\link[stats]{window}(stats) :}{This method extracts a section of or extends the FLInedex object along the \code{year} dimension. If a frequency is specified, the new object contains data at
  only those year steps.}
\item{}{\code{ signature(x=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{catch.n}, \link{catch.n<-}, \link{catch.wt}, \link{catch.wt<-}, \link{coerce}, \link{computeCatch}, \link{dims}, \link{iter}, \link[graphics]{plot}, \link{propagate}, \link[base]{summary}, \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp} }
\examples{}
\keyword{classes}
