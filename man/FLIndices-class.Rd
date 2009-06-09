\name{FLIndices}
\docType{class}
\alias{FLIndices-class}
\alias{FLIndices}
\alias{FLIndices-methods}
\alias{FLIndices,ANY-method}
 \alias{FLIndices,missing-method}
 \alias{FLIndices,list-method}

\title{Class FLIndices}
\description{
\code{FLIndices} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLIndices} must all be of class \code{FLIndex}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
}
\section{Slots}{\describe{
\item{.Data}{The data. \code{list}.}
\item{names}{Names of the list elements. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}.}
}}
\section{Extends}{\describe{
FLlst
list
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLIndices(object, ...)} constructor method allows simple creation of new FLIndices with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an FLIndices object}
\item{ signature(object=missing) :}{Returns an empty FLIndices object}
\item{ signature(object=list) :}{Returns an FLIndices object}
}}

\section{Methods}{\describe{
All methods are inherited.
}}
\author{The FLR Team}
\seealso{ \link{FLlst-class}, \link[base]{list}}
\examples{}
\keyword{classes}
