#' Class FLStock
#' 
#' A class for modelling a fish stock.
#' 
#' The \code{FLStock} object contains a representation of a fish stock. This
#' includes information on removals (i.e. catches, landings and discards),
#' maturity, natural mortality and the results of an analytical assessment
#' (i.e. estimates of abundance and removal rates).
#' 
#' @name FLStock
#' @aliases FLStock FLStock,FLQuant-method FLStock,missing-method FLStock-class
#' FLStock-methods catch,FLStock-method catch.n,FLStock-method
#' catch.n<-,FLStock,FLQuant-method catch.wt,FLStock-method
#' catch.wt<-,FLStock,FLQuant-method catch<-,FLStock,FLQuant-method
#' desc,FLStock-method desc<-,FLStock,character-method discards,FLStock-method
#' discards.n,FLStock-method discards.n<-,FLStock,FLQuant-method
#' discards.wt,FLStock-method discards.wt<-,FLStock,FLQuant-method
#' discards<-,FLStock,FLQuant-method harvest,FLStock-method
#' harvest.spwn,FLStock-method harvest.spwn<-,FLStock,FLQuant-method
#' harvest<-,FLStock,FLQuant-method landings,FLStock-method
#' landings.n,FLStock-method landings.n<-,FLStock,FLQuant-method
#' landings.wt,FLStock-method landings.wt<-,FLStock,FLQuant-method
#' landings<-,FLStock,FLQuant-method m,FLStock-method m.spwn,FLStock-method
#' m.spwn<-,FLStock,FLQuant-method m<-,FLStock,FLQuant-method
#' mat,FLStock-method mat<-,FLStock,FLQuant-method name,FLStock-method
#' name<-,FLStock,character-method range,FLStock-method
#' range<-,FLStock,numeric-method stock,FLStock-method stock.n,FLStock-method
#' stock.n<-,FLStock,FLQuant-method stock.wt,FLStock-method
#' stock.wt<-,FLStock,FLQuant-method stock<-,FLStock,FLQuant-method
#' @docType class
#' @section Slots:
#' 
#' \describe{ \item{catch}{Total catch weight (\code{FLQuant}).}
#' \item{catch.n}{Catch numbers (\code{FLQuant}).} \item{catch.wt}{Mean catch
#' weights (\code{FLQuant}).} \item{discards}{Total discards weight
#' (\code{FLQuant}).} \item{discards.n}{Discard numbers (\code{FLQuant}).}
#' \item{discards.wt}{Mean discard weights (\code{FLQuant}).}
#' \item{landings}{Total landings weight (\code{FLQuant}).}
#' \item{landings.n}{Landing numbers (\code{FLQuant}).}
#' \item{landings.wt}{Landing weights (\code{FLQuant}).} \item{stock}{Total
#' stock weight (\code{FLQuant}).} \item{stock.n}{Stock numbers
#' (\code{FLQuant}).} \item{stock.wt}{Mean stock weights (\code{FLQuant}).}
#' \item{m}{Natural mortality (\code{FLQuant}).} \item{mat}{Proportion mature
#' (\code{FLQuant}).} \item{harvest}{Harvest rate or fishing mortality. The
#' units of the FLQuant should be set to 'harvest' or 'f' accordingly
#' (\code{FLQuant}).} \item{harvest.spwn}{Proportion of harvest/fishing
#' mortality before spawning (\code{FLQuant}).} \item{m.spwn}{Proportion of
#' natural mortality before spawning (\code{FLQuant}).} \item{name}{Name of the
#' stock (\code{character}).} \item{desc}{Description of stock
#' (\code{character}).} \item{range}{Named numeric vector containing the quant
#' and year ranges, the plusgroup and the quant range that the average fishing
#' mortality is calculated over (\code{numeric}).} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link{as.FLBiol}, \link{as.FLSR},
#' \link{catch}, \link{catch<-}, \link{catch.n}, \link{catch.n<-},
#' \link{catch.wt}, \link{catch.wt<-}, \link[methods]{coerce},
#' \link{computeCatch}, \link{computeDiscards}, \link{computeLandings},
#' \link{discards}, \link{discards<-}, \link{discards.n}, \link{discards.n<-},
#' \link{discards.wt}, \link{discards.wt<-}, \link{harvest}, \link{harvest<-},
#' \link{harvest.spwn}, \link{landings}, \link{landings<-}, \link{landings.n},
#' \link{landings.n<-}, \link{landings.wt}, \link{landings.wt<-}, \link{m},
#' \link{m<-}, \link{mat}, \link{m.spwn}, \link[graphics]{plot}, \link{ssb},
#' \link{ssbpurec}, \link{stock}, \link{stock.n}, \link{stock.wt}, \link{trim},
#' \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' summary(ple4)
#'
#' landings(ple4)  # get the landings slot
#' landings(ple4) <- apply(landings.n(ple4)*landings.wt(ple4),2,sum)  # assign values to the landings slot
#' 
#' discards(ple4) <- computeDiscards(ple4)  # perform same calculation as preceding apply function
#' 
#' harvest(ple4) <- 'f'  # set the units of the harvest slot of an FLStock object
#' 
#' catch(ple4) <- computeCatch(ple4)
#' catch(ple4) <- computeCatch(ple4, slot="all")
#' 
#' ple4[,1]  # subset the FLStock
#' trim(ple4, age=2:6, year=1980:1990)  # trim the FLStock
#' 
#' ssb(ple4)  # calculate SSB
#' ssbpurec(ple4)  # calculate SSB per recruit at zero fishing mortality
#' 
#' biol <- as(ple4, "FLBiol")  # coerce an FLStock to an FLBiol
#' flsr <- as.FLSR(ple4)  # initialise an FLSR object from an FLStock
#' 