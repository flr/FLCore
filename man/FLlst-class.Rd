\name{FLlst}
\docType{class}
\alias{FLlst-class}
\alias{FLlst}
\alias{FLlst-methods}
\alias{FLlst,ANY-method}
\alias{FLlst,missing-method}
\alias{FLlst,list-method}

\title{Class FLlst}
\description{
\code{FLlst} is a class that extends \code{list} but implements a set of features that give a little bit more structure to list objects. First the elements of \code{FLlst} must all be of the same class. Second it implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. Both features are needed to propoerly implement \code{FLFleet} class. 
}
\section{Slots}{\describe{
\item{.Data}{The data. \code{list}.}
\item{names}{Names of the list elements. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}.}
}}
\section{Extends}{\describe{
list
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLlst(object)} constructor method allows simple creation of new FLlst with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an \code{FLlst} object with the provided elements if they are all o the same class.}
\item{ signature(object=missing) :}{Returns an empty \code{FLlst} object}
\item{ signature(object=list) :}{Returns and \code{FLlst} object with the input list at its core}

}}

\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Select method.}
\item{}{\code{ signature(x=FLlst,i=ANY,j=missing,drop=missing) }}
\item{\link[base]{[<-}(base) :}{Replacement method for elements.}
\item{}{\code{ signature(x=FLlst,i=ANY,j=missing,value=ANY) }}
\item{\link[base]{[[<-}(base) :}{Replacement method within elements.}
\item{}{\code{ signature(x=FLlst,i=ANY,j=missing,value=missing) }}
\item{\link[base]{$<-}(base) :}{Replacement method for elements.}
\item{}{\code{ signature(x=FLlst,name=character,value=missing) }}
\item{\link[methods]{coerce}(methods) :}{Coerce method.}
\item{}{\code{ signature(from=FLlst,to=list,strict=missing) }}
\item{\link[base]{lapply}(base) :}{\code{lapply} implemented for \code{FLlst} objects.}
\item{}{\code{ signature(X=missing,FUN=missing) }}
\item{\link[stats]{window}(stats) :}{Selects a set of years from all elements at once.}
\item{}{\code{ signature(x=missing) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{[}, \link[base]{[<-}, \link[base]{[[<-}, \link[base]{$<-}, \link[methods]{coerce}, \link[base]{lapply}, \link[stats]{window}, \link[base]{list}}
\examples{
fll01 <- new("FLlst", list(a=1:10, b=10:20))
fll02 <- new("FLlst", list(1:10, 10:20), names=c("a","b"))
fll03 <- FLlst(a=1:10, b=10:20)
fll04 <- FLlst(list(a=1:10, b=10:20))
fll05 <- FLlst(c(1:10), c(10:20))
names(fll05) <- names(fll01)
names(fll01)
}
\keyword{classes}
