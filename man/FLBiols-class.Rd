\name{FLBiols}
\docType{class}
\alias{FLBiols}
\alias{FLBiols-class}
\alias{FLBiols}
\alias{FLBiols-methods}
\alias{FLBiols,ANY-method}
\alias{FLBiols,missing-method}
\alias{FLBiols,list-method}

\title{Class FLBiols}
\description{
 A list of \code{FLBiol} objects.
}
\section{Slots}{\describe{
\item{.Data}{Internal S4 data representation, of class \code{list}.}
\item{desc}{As textual description of the object contents}
\item{lock}{Can the object be extended/trimmed? \code{TRUE} or \code{FALSE}.}
\item{names}{A character vector for the element names}
}}
\section{Extends}{\describe{
FLlst
list
vector
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLBiols(object, ...)} constructor method allows simple creation of new FLBiols objects with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an FLBiols object}
\item{ signature(object=missing) :}{Returns an empty FLBiols object}
\item{ signature(object=list) :}{Returns an FLBiols object}
}}
\section{Methods}{\describe{
All methods are inherited.
}}
\author{The FLR Team}
\seealso{ \link{FLlst}, \link[base]{list}, \link[base]{vector} }
\keyword{classes}
