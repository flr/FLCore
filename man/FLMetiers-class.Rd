\name{FLMetiers}
\docType{class}
\alias{FLMetiers-class}
\title{Class FLMetiers}
\description{
\code{FLMetiers} is a class that extends \code{list} through \code{FLlst} but implements a set of features that give a little bit more structure to list objects. The elements of \code{FLMetiers} must all be of class \code{FLMetier}. It implements a lock mechanism that, when turned on, does not allow the user to increase or decrease the object length. 
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
vector
}}
\section{Methods}{\describe{
\item{\link{catch}(FLCore) :}{Accessor method getting the FLCatch catch slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n}(FLCore) :}{Accessor method getting the FLCatch catch.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt}(FLCore) :}{Accessor method getting the FLCatch catch.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards}(FLCore) :}{Accessor method getting the FLCatch discards slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n}(FLCore) :}{Accessor method getting the FLCatch discards.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt}(FLCore) :}{Accessor method getting the FLCatch discards.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings}(FLCore) :}{Accessor method getting the FLCatch landings slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n}(FLCore) :}{Accessor method getting the FLCatch landings.n slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt}(FLCore) :}{Accessor method getting the FLCatch landings.wt slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.q}(FLCore) :}{Accessor method getting the FLCatch catch.q slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
\item{\link{price}(FLCore) :}{Accessor method getting the FLCatch price slots. Returns a FLQuants}
\item{}{\code{ signature(object=missing) }}
}}
\author{The FLR Team}
\seealso{ \link{catch}, \link{catch.n}, \link{catch.wt}, \link{discards}, \link{discards.n}, \link{discards.wt}, \link{FLFleet}, \link{landings}, \link{landings.n}, \link{landings.wt}, \link{price}, \link{FLlst-class}, \link[base]{list}}
\examples{

# creation
met1 <- FLMetier(name='met1',catches=FLCatches(FLCatch(name="spp1",
  landings.n=FLQuant(rnorm(100), dim=c(10,20)))))
met2 <- FLMetier(name='met2',catches=FLCatches(FLCatch(name="spp1",
  landings.n=FLQuant(rnorm(100), dim=c(10,20))), FLCatch(name="spp2",
  landings.n=FLQuant(rnorm(100), dim=c(10,20)))))
lst.metiers <- FLMetiers(met1=met1,met2=met2)

# getter
landings.n(met1,catch="spp1") # get a FLQuant
landings.n(lst.metiers,catch="spp1") # get a FLQuants across metiers
landings.n(lst.metiers,metier="met1") # get a FLQuants across species
landings.n(lst.metiers,metier="met1",catch="spp1") # get a FLQuant
}
\keyword{classes}
