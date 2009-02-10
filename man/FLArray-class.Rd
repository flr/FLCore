\name{FLArray}
\docType{class}
\alias{FLArray}
\alias{FLArray-class}
\title{Class FLArray}
\description{
  A basic class, extended by \code{\link{FLQuant-class}} and \code{\link{FLCohort-class}}.
  No objects of this class are created in FLCore, as it is used only for method
  inheritance.
}
\section{Slots}{\describe{
\item{.Data (\code{array}):}{Internal S4 data representation.}
\item{units (\code{character}):}{The data units in some understandable metric.}
}}
\section{Extends}{\describe{
array
matrix
vector
}}
\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Subset method}
\item{}{\code{ signature(x=FLArray,i=ANY,j=ANY,drop=missing) }}
\item{\link[base]{[<-}(base) :}{Substitute elements}
\item{}{\code{ signature(x=FLArray,i=ANY,j=ANY,value=missing) }}
\item{\link{units}(base) :}{Extract the information about data units.}
\item{}{\code{ signature(x=FLArray) }}
\item{\link{units<-}(base) :}{Replace data units information.}
\item{}{\code{ signature(x=FLArray,value=character) }}
\item{\link{quant}(FLCore) :}{Extract the name of the first dimname.}
\item{}{\code{ signature(object=FLArray) }}
\item{\link{quant<-}(FLCore) :}{Modify the name of the first dimname.}
\item{}{\code{ signature(object=FLArray,value=character) }}
\item{\link[base]{names}(base) :}{Returns a vector with the names of the elements in the dimnames list (the dimnames\' names).}
\item{}{\code{ signature(x=FLArray) }}
\item{\link{iter}(FLCore) :}{Returns a single \emph{iter}, subsetting along the sixth dimension }
\item{}{\code{ signature(object=FLArray, iter=numeric) }}
\item{\link[base]{summary}(base) :}{Outputs a summary of the object contents}
\item{}{\code{ signature(object=FLArray) }}
\item{\link[methods]{show}(methods) :}{Standard display of an object contents}
\item{}{\code{ signature(object=FLArray) }}
\item{\link{trim}(FLCore) :}{Trim an object along any dimension}
\item{}{\code{ signature(x=FLArray) }}
\item{\link{expand}(FLCore) :}{Extend an object along any dimension}
\item{}{\code{ signature(x=FLArray) }}
\item{\link[methods]{Arith}(base) :}{Arithmetic operations on FLArray and numeric objects}
\item{}{\code{ signature(e1=FLQuant, e2=FLQuant) }}
\item{\link[methods]{as.data.frame}(base) :}{Convert to a data.frame with 7 columns}
\item{}{\code{ signature(x=FLArray,row.names=missing,optional=missing) }}
}}
\author{The FLR Team}
\seealso{
\code{\link{FLQuant-class}}, \code{\link{FLCohort-class}}
}
\keyword{classes}
