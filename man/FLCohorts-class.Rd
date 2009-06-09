\name{FLCohorts}
\docType{class}
\alias{FLCohorts-class}
\alias{FLCohorts}
\alias{FLCohorts-methods}
\alias{FLCohorts,ANY-method}
\alias{FLCohorts,missing-method}
\alias{FLCohorts,list-method}
\alias{FLCohorts,FLCohorts-method}

\title{Class FLCohorts}
\description{
\code{FLCohorts} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLCohorts} must all be of class \code{FLCohort}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
}
\section{Slots}{\describe{
\item{.Data}{The data. \code{list}}
\item{names}{Names of the list elements. \code{character}}
\item{desc}{Description of the object. \code{character}}
\item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}}
}}
\section{Extends}{\describe{
FLlst
list
vector
}}
\section{Constructor}{
\describe{
\item{}{The \code{FLCohorts(object, ...)} constructor method allows simple creation of new FLCatch with the methods described below.
}
\item{ signature(object=ANY) :}{Returns an FLCohorts object}
\item{ signature(object=missing) :}{Returns an empty FLCohorts object}
\item{ signature(object=list) :}{Returns an FLCohorts object}
}}

\section{Methods}{\describe{
\item{\link[base]{*}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLCohorts,e2=FLCohorts) }}
\item{\link[base]{Arith}(base) :}{Describe method}
\item{}{\code{ signature(e1=FLCohorts,e2=FLCohorts) }}
\item{\link[base]{as.data.frame}(base) :}{Describe method}
\item{}{\code{ signature(x=FLCohorts,row.names=missing,optional=missing) }}
\item{\link{bubbles}(FLCore) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLCohorts) }}
\item{\link{catch<-}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=FLStock,value=FLCohorts) }}
\item{\link{iter}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[stats]{model.frame}(stats) :}{Describe method}
\item{}{\code{ signature(formula=missing) }}
\item{\link[methods]{show}(methods) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[base]{summary}(base) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link[lattice]{xyplot}(lattice) :}{Describe method}
\item{}{\code{ signature(x=formula,data=FLCohorts) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{*}, \link[base]{Arith}, \link[base]{as.data.frame}, \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame}, \link[base]{show}, \link[base]{summary}, \link[lattice]{xyplot}, \link{FLlst}, \link[base]{list}}
\keyword{classes}
