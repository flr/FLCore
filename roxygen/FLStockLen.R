#' Class FLStockLen
#' 
#' A class for modelling a length structured fish stock.
#' 
#' The \code{FLStockLen} object contains a length based representation of a
#' fish stock This includes information on removals (i.e. catches, landings and
#' discards), maturity, natural mortality and the results of an analytical
#' assessment (i.e. estimates of abundance and removal rates).
#' 
#' @name FLStockLen
#' @aliases FLStockLen FLStockLen,FLQuant-method FLStockLen,missing-method
#' FLStockLen-class FLStockLen-methods catch,FLStockLen-method
#' catch.n,FLStockLen-method catch.n<-,FLStockLen,FLQuant-method
#' catch.wt,FLStockLen-method catch.wt<-,FLStockLen,FLQuant-method
#' catch<-,FLStockLen,FLQuant-method desc,FLStockLen-method
#' desc<-,FLStockLen,character-method discards,FLStockLen-method
#' discards.n,FLStockLen-method discards.n<-,FLStockLen,FLQuant-method
#' discards.wt,FLStockLen-method discards.wt<-,FLStockLen,FLQuant-method
#' discards<-,FLStockLen,FLQuant-method halfwidth,FLStockLen-method
#' halfwidth<-,FLStockLen,numeric-method harvest,FLStockLen-method
#' harvest.spwn,FLStockLen-method harvest.spwn<-,FLStockLen,FLQuant-method
#' harvest<-,FLStockLen,FLQuant-method landings,FLStockLen-method
#' landings.n,FLStockLen-method landings.n<-,FLStockLen,FLQuant-method
#' landings.wt,FLStockLen-method landings.wt<-,FLStockLen,FLQuant-method
#' landings<-,FLStockLen,FLQuant-method m,FLStockLen-method
#' m.spwn,FLStockLen-method m.spwn<-,FLStockLen,FLQuant-method
#' m<-,FLStockLen,FLQuant-method mat,FLStockLen-method
#' mat<-,FLStockLen,FLQuant-method name,FLStockLen-method
#' name<-,FLStockLen,character-method range,FLStockLen-method
#' range<-,FLStockLen,numeric-method stock,FLStockLen-method
#' stock.n,FLStockLen-method stock.n<-,FLStockLen,FLQuant-method
#' stock.wt,FLStockLen-method stock.wt<-,FLStockLen,FLQuant-method
#' stock<-,FLStockLen,FLQuant-method
#' @docType class
#' @section Slots:
#' 
#' \describe{ \item{halfwidth}{The middle of the length bins (\code{numeric}).}
#' \item{catch}{Total catch weight (\code{FLQuant}).} \item{catch.n}{Catch
#' numbers (\code{FLQuant}).} \item{catch.wt}{Mean catch weights
#' (\code{FLQuant}).} \item{discards}{Total discards weight (\code{FLQuant}).}
#' \item{discards.n}{Discard numbers (\code{FLQuant}).} \item{discards.wt}{Mean
#' discard weights (\code{FLQuant}).} \item{landings}{Total landings weight
#' (\code{FLQuant}).} \item{landings.n}{Landing numbers (\code{FLQuant}).}
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
#' \link{computeCatch}, \link{computeDiscards}, \link{computeLandings},
#' \link[graphics]{plot}, \link{ssb}, \link{ssbpurec}, \link{trim},
#' \link{FLComp}
#' @keywords classes
#'