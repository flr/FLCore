\name{FLIndices}
\docType{class}
\alias{FLIndices-class}
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
\section{Methods}{\describe{
All methods are inherited.
}}
\author{The FLR Team}
\seealso{ \link{FLlst-class}, \link[base]{list}}
\examples{}
\keyword{classes}
