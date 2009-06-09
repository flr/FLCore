\name{FLQuants}
\docType{class}
\alias{FLQuants-class}
\alias{FLQuants}
\alias{FLQuants-methods}
\alias{FLQuants,ANY-method}
 \alias{FLQuants,missing-method}
 \alias{FLQuants,list-method}
 \alias{FLQuants,FLQuants-method}
\title{Class FLQuants}
\description{
\code{FLQuants} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLQuants} must all be of class \code{FLQuant}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
\item{}{The \code{FLQuants(object, ...)} constructor method allows simple creation of new FLQuants with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an FLQuants object with the given named elements}
\item{ signature(object=missing) :}{Returns an FLQuants object}
\item{ signature(object=list) :}{Returns an FLQuants object with the provided list as its core}
}}
\section{Methods}{\describe{
\item{\link[base]{*}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLQuants,e2=FLQuants) }}
\item{\link[base]{Arith}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLQuants,e2=FLQuants) }}
\item{\link[base]{as.data.frame}(base) :}{Describe method}
\item{}{\code{ signature(x=FLQuants,row.names=missing,optional=missing) }}
\item{\link{bubbles}(FLCore) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuants) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLQuants) }}
\item{\link{iter}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[stats]{model.frame}(stats) :}{Describe method}
\item{}{\code{ signature(formula=missing) }}
\item{\link[methods]{show}(methods) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{summary}(base) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[lattice]{xyplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLQuants) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{*}, \link[base]{Arith}, \link[base]{as.data.frame}, \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame}, \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot}, \link{FLlst}, \link[base]{list}}
\keyword{classes}
