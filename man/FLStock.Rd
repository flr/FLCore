\docType{class}
\name{FLStock}
\alias{FLStock}
\alias{FLStock,FLQuant-method}
\alias{FLStock,missing-method}
\alias{FLStock-class}
\alias{FLStock-methods}
\alias{catch,FLStock-method}
\alias{catch.n,FLStock-method}
\alias{catch.n<-,FLStock,FLQuant-method}
\alias{catch.wt,FLStock-method}
\alias{catch.wt<-,FLStock,FLQuant-method}
\alias{catch<-,FLStock,FLQuant-method}
\alias{desc,FLStock-method}
\alias{desc<-,FLStock,character-method}
\alias{discards,FLStock-method}
\alias{discards.n,FLStock-method}
\alias{discards.n<-,FLStock,FLQuant-method}
\alias{discards.wt,FLStock-method}
\alias{discards.wt<-,FLStock,FLQuant-method}
\alias{discards<-,FLStock,FLQuant-method}
\alias{harvest,FLStock-method}
\alias{harvest.spwn,FLStock-method}
\alias{harvest.spwn<-,FLStock,FLQuant-method}
\alias{harvest<-,FLStock,FLQuant-method}
\alias{landings,FLStock-method}
\alias{landings.n,FLStock-method}
\alias{landings.n<-,FLStock,FLQuant-method}
\alias{landings.wt,FLStock-method}
\alias{landings.wt<-,FLStock,FLQuant-method}
\alias{landings<-,FLStock,FLQuant-method}
\alias{m,FLStock-method}
\alias{m.spwn,FLStock-method}
\alias{m.spwn<-,FLStock,FLQuant-method}
\alias{m<-,FLStock,FLQuant-method}
\alias{mat,FLStock-method}
\alias{mat<-,FLStock,FLQuant-method}
\alias{name,FLStock-method}
\alias{name<-,FLStock,character-method}
\alias{range,FLStock-method}
\alias{range<-,FLStock,numeric-method}
\alias{stock,FLStock-method}
\alias{stock.n,FLStock-method}
\alias{stock.n<-,FLStock,FLQuant-method}
\alias{stock.wt,FLStock-method}
\alias{stock.wt<-,FLStock,FLQuant-method}
\alias{stock<-,FLStock,FLQuant-method}
\title{Class FLStock}
\description{
A class for modelling a fish stock.
}
\details{
The \code{FLStock} object contains a representation of a
fish stock This includes information on removals (i.e.
catches, landings and discards), maturity, natural
mortality and the results of an analytical assessment (i.e.
estimates of abundance and removal rates).
}
\section{Slots}{
  \describe{ \item{catch}{ (\code{FLQuant}) Total catch
  weight.} \item{catch.n}{ (\code{FLQuant}) Catch numbers.}
  \item{catch.wt}{ (\code{FLQuant}) Mean catch weights.}
  \item{discards}{ (\code{FLQuant}) Total discards weight.}
  \item{discards.n}{ (\code{FLQuant}) Discard numbers.}
  \item{discards.wt}{ (\code{FLQuant}) Mean discard
  weights.} \item{landings}{ (\code{FLQuant}) Total
  landings weight.} \item{landings.n}{ (\code{FLQuant})
  Landing numbers.} \item{landings.wt}{ (\code{FLQuant})
  Landing weights.} \item{stock}{ (\code{FLQuant}) Total
  stock weight.} \item{stock.n}{(\code{FLQuant}) Stock
  numbers.} \item{stock.wt}{(\code{FLQuant}) Mean stock
  weights.} \item{m}{ (\code{FLQuant}) Natural mortality.}
  \item{mat}{ (\code{FLQuant}) Proportion mature.}
  \item{harvest}{ (\code{FLQuant}) Harvest rate or fishing
  mortality. The units of the FLQuant should be set to
  'harvest' or 'f' accordingly.} \item{harvest.spwn}{
  (\code{FLQuant}) Proportion of harvest/fishing mortality
  before spawning.} \item{m.spwn}{ (\code{FLQuant})
  Proportion of natural mortality before spawning.}
  \item{name}{ (\code{character}) Name of the stock.}
  \item{desc}{ (\code{character}) Description of stock.}
  \item{range}{ (\code{numeric}) Named numeric vector
  containing the quant and year ranges, the plusgroup and
  the quant range that the average fishing mortality is
  calculated over.} }
}

\section{Validity}{
  \describe{ \item{Dimensions}{All FLQuant slots must have
  iters equal to 1 or 'n'.} \item{Iters}{The dimname for
  iter[1] should be '1'.} \item{Dimnames}{The name of the
  quant dimension must be the same for all FLQuant slots.}
  \item{Totals}{The length of the quant dimension for the
  totals slots (catch, landings and discards) must be equal
  to 1.} }
}

\section{Accesors}{
  All slots in the class have accessor and replacement
  methods defined that allow retrieving and substituting
  individual slots.

  The values passed for replacement need to be of the class
  of that slot. A numeric vector can also be used when
  replacing FLQuant slots, and the vector will be used to
  substitute the values in the slot, but not its other
  attributes.
}

\section{Constructor}{
  A construction method exists for this class that can take
  named arguments for any of its slots. All slots are then
  created to match the requirements of the class validity.
  If an unnamed \code{FLQuant} object is provided, this is
  used for sizing but not stored in any slot.
}
\examples{
data(ple4)

landings(ple4) #get the landings slot
landings(ple4) <- apply(landings.n(ple4)*landings.wt(ple4),2,sum)   # assign values to the landings slot

discards(ple4) <- computeDiscards(ple4)

harvest(ple4) <- 'f' # set the units of the harvest slot of an FLStock object

catch(ple4) <- computeCatch(ple4)
catch(ple4) <- computeCatch(ple4, slot="all")

ple4[,1] # subset the FLStock
trim(ple4, age=2:6, year=1980:1990) #trim the FLStock

ssb(ple4) # calculate SSB
ssbpurec(ple4) # calculate SSB per recruit

biol <- as(ple4, "FLBiol")  # coerce an FLStock to an FLBiol
flsr <- as.FLSR(ple4)       # initialise an FLSR object from an FLStock
}
\author{
The FLR Team
}
\seealso{
\link[base]{[}, \link[base]{[<-}, \link{as.FLBiol},
\link{as.FLSR}, \link{catch}, \link{catch<-},
\link{catch.n}, \link{catch.n<-}, \link{catch.wt},
\link{catch.wt<-}, \link[methods]{coerce},
\link{computeCatch}, \link{computeDiscards},
\link{computeLandings}, \link{discards}, \link{discards<-},
\link{discards.n}, \link{discards.n<-}, \link{discards.wt},
\link{discards.wt<-}, \link{harvest}, \link{harvest<-},
\link{harvest.spwn}, \link{landings}, \link{landings<-},
\link{landings.n}, \link{landings.n<-}, \link{landings.wt},
\link{landings.wt<-}, \link{m}, \link{m<-}, \link{mat},
\link{m.spwn}, \link[graphics]{plot}, \link{ssb},
\link{ssbpurec}, \link{stock}, \link{stock.n},
\link{stock.wt}, \link{trim}, \link{FLComp}
}
\keyword{classes}

