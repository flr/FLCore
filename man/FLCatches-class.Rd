\name{FLCatches}
\docType{class}
\alias{FLCatches-class}
\title{Class FLCatches}
\alias{FLCatches}
\alias{FLCatches-methods}
\alias{FLCatches,ANY-method}
\alias{FLCatches,missing-method}
\alias{FLCatches,list-method}
\description{
\code{FLCatches} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLCatches} must all be of class \code{FLCatch}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
\item{}{The \code{FLCatches(object)} constructor method allows simple creation of new FLCatches with the methods described below.
}
\item{ signature(object=ANY) :}{create a FLMetiers object from a set of FLMetier objects}
\item{ signature(object=missing) :}{create a FLMetiers object as a void list for future use}
\item{ signature(object=list) :}{create a FLMetiers object from a list of FLMetier objects}
}}

\section{Methods}{\describe{
\item{\link{catch}(FLCore) :}{Accessor method for the \emph{catch} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{catch.n}(FLCore) :}{Accessor method for the \emph{catch.n} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{catch.wt}(FLCore) :}{Accessor method for the \emph{catch.wt} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{discards}(FLCore) :}{Accessor method for the \emph{discards} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{discards.n}(FLCore) :}{Accessor method for the \emph{discards.n} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{discards.wt}(FLCore) :}{Accessor method for the \emph{discards.wt} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{landings}(FLCore) :}{Accessor method for the \emph{landings} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{landings.n}(FLCore) :}{Accessor method for the \emph{landings.n} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{landings.wt}(FLCore) :}{Accessor method for the \emph{landings.wt} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{catch.q}(FLCore) :}{Accessor method for the \emph{catch.q} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{price}(FLCore) :}{Accessor method for the \emph{price} slots. Returns an object of class \code{\link{FLQuants-class}}.}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{FLFleet}(FLCore) :}{Conversion method of a FLFleet object from a FLCatches object}
\item{}{\code{ signature(object=FLCatches) }}
\item{\link{FLMetier}(FLCore) :}{Conversion method of a FLMetier object from a FLCatches object}
\item{}{\code{ signature(catches=FLCatches) }}
}}
\author{The FLR Team}
\seealso{ \link{catch}, \link{catch.n}, \link{catch.wt}, \link{discards}, \link{discards.n}, \link{discards.wt}, \link{FLFleet}, \link{FLMetier}, \link{landings}, \link{landings.n}, \link{landings.wt}, \link{price}, \link{FLlst}, \link[base]{list}}
\examples{

# creation
catch1 <- FLCatch(name='catch1')
catch2 <- FLCatch(name='catch2')
lst.catches <- FLCatches(catch1,catch2)

# getters
catch(lst.catches) # get a FLQuants list over the species
}
\keyword{classes}
