\docType{class}
\name{FLStockLen}
\alias{FLStockLen}
\alias{FLStockLen,FLQuant-method}
\alias{FLStockLen,missing-method}
\alias{FLStockLen-class}
\alias{FLStockLen-methods}
\title{Class FLStockLen}
\description{
A class for modelling a length structured fish stock.
}
\details{
The \code{FLStockLen} object contains a length based
representation of a fish stock This includes information on
removals (i.e. catches, landings and discards), maturity,
natural mortality and the results of an analytical
assessment (i.e. estimates of abundance and removal rates).
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
\author{
The FLR Team
}
\seealso{
\link[base]{[}, \link[base]{[<-}, \link{as.FLBiol},
\link{as.FLSR}, \link{computeCatch},
\link{computeDiscards}, \link{computeLandings},
\link[graphics]{plot}, \link{ssb}, \link{ssbpurec},
\link{trim}, \link{FLComp}
}
\keyword{classes}

