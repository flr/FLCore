\name{FLStock-class}
\docType{class}
\alias{FLStock-class}
\alias{FLStock}
\alias{FLStock-methods}
\alias{FLStock,missing-method}
\alias{FLStock,FLQuant-method}

\title{Class FLStock}
\description{

 Data representation for a stock object. The \code{FLStock} object contains the available information
 for a fish stock, usually derived from a national or international sampling program, that will be used
 for conducting an assessment. Typically this will include information on total catches. In addition
 information on catch numbers, catch weights, maturity, natural mortality etc. may be provided.
 
 The \code{FLStock} object also contains the slots stock, stock.n and harvest to hold information
 resulting from an analytical assessment.
 
}
\section{Slots}{\describe{
\item{catch}{ (\code{FLQuant}) Total catch weight by year}
\item{catch.n}{ (\code{FLQuant}) Catch numbers by quant and year}
\item{catch.wt}{ (\code{FLQuant}) Catch weights by quant and year}
\item{discards}{ (\code{FLQuant}) Total discards weight by year}
\item{discards.n}{ (\code{FLQuant}) Discard numbers by quant and year}
\item{discards.wt}{ (\code{FLQuant}) Discard weights by quant and year}
\item{landings}{ (\code{FLQuant}) Total landings weight by year}
\item{landings.n}{ (\code{FLQuant}) Landing numbers by quant and year}
\item{landings.wt}{ (\code{FLQuant}) Landing weights by quant and year}
\item{stock}{ (\code{FLQuant}) Total stock weight by year}
\item{stock.n}{ (\code{FLQuant}) Stock numbers by quant and year}
\item{stock.wt}{ (\code{FLQuant}) Stock weights by quant and year}
\item{m}{ (\code{FLQuant}) Natural mortality by quant and year}
\item{mat}{ (\code{FLQuant}) Proportion mature by quant and year}
\item{harvest}{ (\code{FLQuant}) Harvest rate or fishing mortality by quant and year. The units of the FLQuant should be set to 'harvest' or 'f' accordingly}
\item{harvest.spwn}{ (\code{FLQuant}) Proportion of harvest/fishing mortality before spawning by quant and year}
\item{m.spwn}{ (\code{FLQuant}) Proportion of natural mortality before spawning by quant and year}
\item{name}{ (\code{character}) Name of the stock}
\item{desc}{ (\code{character}) Description of stock}
\item{range}{ (\code{numeric}) Named vector containing the dimensions of the stock object}
}}
\section{Extends}{\describe{
FLComp
}}

\section{Constructor}{
\describe{
\item{}{The \code{FLStock(object, ...)} constructor method allows simple creation of new FLStock with the methods described below.
}
\item{ signature(object=missing) :}{Returns an \code{FLStock} using the first provided argument of class \code{FLQuant} for object sizing, or \code{FLQuant()} if none given.}
\item{ signature(object=FLQuant) :}{Returns and \code{FLStock} that uses \emph{object} only for sizing.}
}}
\section{Constructor arguments}{
  \describe{
  \item{object}{An initial FLQuant only used for sizing.}
  \item{plusgroup}{The age/length to be considered as plusgroup.}
  \item{...}{Extra objects to be allocated to given slots.}
  }
}
\section{Methods}{\describe{
\item{\link[base]{[}(base) :}{Subsets an FLStock by subsetting each FLQuant slot in the object}
\item{}{\code{ signature(x=FLStock,i=ANY,j=ANY,drop=missing) }}
\item{\link[base]{[<-}(base) :}{Assign to a subset of an FLStock an FLStock of equivalent dimension}
\item{}{\code{ signature(x=FLStock,i=ANY,j=ANY,value=missing) }}
\item{\link{as.FLBiol}(FLCore) :}{Convert an FLStock object to an FLBiol object}
\item{}{\code{ signature(object=missing) }}
\item{\link{as.FLSR}(FLCore) :}{Initialise an FLSR object from an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch}(FLCore) :}{Get the catch slot from an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch<-}(FLCore) :}{Assign values to the catch slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch<-}(FLCore) :}{Assign values to the catch, catch.n and catch.wt slots of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuants) }}
\item{\link{catch.n}(FLCore) :}{Get the catch.n slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.n<-}(FLCore) :}{Assign values to the catch.n slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{catch.wt}(FLCore) :}{Get the catch.wt slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{catch.wt<-}(FLCore) :}{Assign values to the catch.wt slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link[methods]{coerce}(methods) :}{Force an object of class FLStock to another class using as(from, to)}
\item{}{\code{ signature(from=NULL,to=FLStock,strict=missing) }}
\item{\link[methods]{coerce}(methods) :}{Force an object of class FLBiol to an object of class FLStock. Uses as(from, to)}
\item{}{\code{ signature(from=FLBiol,to=FLStock,strict=missing) }}
\item{\link[methods]{coerce}(methods) :}{Force an object of class FLStock be an object of class FLBiol. Uses as(from, to)}
\item{}{\code{ signature(from=FLStock,to=FLBiol,strict=missing) }}
\item{\link{computeCatch}(FLCore) :}{Calculate the total catch weight by year for an FLStock}
\item{}{\code{ signature(object=missing) }}
\item{\link{computeDiscards}(FLCore) :}{Calculate the total discards weight by year for an FLStock}
\item{}{\code{ signature(object=missing) }}
\item{\link{computeLandings}(FLCore) :}{Calculate the total landings weight by year for an FLStock}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards}(FLCore) :}{Get the discards slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards<-}(FLCore) :}{Assign values to the discards slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.n}(FLCore) :}{Get the discards.n slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.n<-}(FLCore) :}{Assign values to the discards.n slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{discards.wt}(FLCore) :}{Get the discards.wt slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{discards.wt<-}(FLCore) :}{Assign values to the discards.wt slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{harvest}(FLCore) :}{Get the harvest slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{harvest<-}(FLCore) :}{Assign values to the units of the harvest slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=character) }}
\item{\link{harvest<-}(FLCore) :}{Assign values to the harvest slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{harvest.spwn}(FLCore) :}{Get the harvest.spwn slot from an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings}(FLCore) :}{Get the landings slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings<-}(FLCore) :}{Assign values to the landings slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.n}(FLCore) :}{Get the landings.n slot from an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.n<-}(FLCore) :}{Assign values to the landings.n slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{landings.wt}(FLCore) :}{Get the landings.wt slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{landings.wt<-}(FLCore) :}{Assign values to the landings.wt slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{m}(FLCore) :}{Get the m slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{m<-}(FLCore) :}{Assign values to the m slot of an FLStock object}
\item{}{\code{ signature(object=FLStock,value=FLQuant) }}
\item{\link{mat}(FLCore) :}{Get the mat slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{m.spwn}(FLCore) :}{Get the m.spwn slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{plot}(graphics) :}{A summary plot of an FLStock object}
\item{}{\code{ signature(x=FLStock,y=missing) }}
\item{\link{ssb}(FLCore) :}{Calculate SSB by year for an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{ssbpurec}(FLCore) :}{Calculate SSB per recruit for an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{stock}(FLCore) :}{Get the stock slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{stock.n}(FLCore) :}{Get the stock.n slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{stock.wt}(FLCore) :}{Get the stock.wt slot of an FLStock object}
\item{}{\code{ signature(object=missing) }}
\item{\link{trim}(FLCore) :}{Trim an FLStock object to a reduced range over the year or quant dimension}
\item{}{\code{ signature(object=missing) }}
}}
\author{The FLR Team}
\seealso{ \link[base]{[}, \link[base]{[<-}, \link{as.FLBiol}, \link{as.FLSR}, \link{catch}, \link{catch<-}, \link{catch.n}, \link{catch.n<-}, \link{catch.wt}, \link{catch.wt<-}, \link[methods]{coerce}, \link{computeCatch}, \link{computeDiscards}, \link{computeLandings}, \link{discards}, \link{discards<-}, \link{discards.n}, \link{discards.n<-}, \link{discards.wt}, \link{discards.wt<-}, \link{harvest}, \link{harvest<-}, \link{harvest.spwn}, \link{landings}, \link{landings<-}, \link{landings.n}, \link{landings.n<-}, \link{landings.wt}, \link{landings.wt<-}, \link{m}, \link{m<-}, \link{mat}, \link{m.spwn}, \link[graphics]{plot}, \link{ssb}, \link{ssbpurec}, \link{stock}, \link{stock.n}, \link{stock.wt}, \link{trim}, \link{FLComp} }
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
\keyword{classes}
