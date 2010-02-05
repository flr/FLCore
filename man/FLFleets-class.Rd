\name{FLFleets}
\docType{class}
\alias{FLFleets-class}
\alias{FLFleets}
\alias{FLFleets-methods}
\alias{FLFleets,ANY-method}
 \alias{FLFleets,missing-method}
 \alias{FLFleets,list-method}

\title{Class FLFleets}
\description{
\code{FLFleets} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLFleets} must all be of class \code{FLFleet}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
\item{}{The \code{FLFleets(object)} constructor method allows simple creation of new FLFleets with the methods described below.
}
\item{ signature(object=ANY) :}{create a FLFleets object from a set of FLFleet objects}
\item{ signature(object=missing) :}{create a FLFleets object as a void list for future use}
\item{ signature(object=list) :}{create a FLFleets object from a list of FLFleet objects}

}}


\section{Methods}{\describe{
All methods are inherited.
}}
\author{The FLR Team}
\seealso{ \link{FLFleets} \link{FLlst}, \link[base]{list}}
\examples{
data(bt4)
is(bt4)
summary(FLFleets(Fleet1=bt4, Fleet2=bt4))
}
\keyword{classes}
