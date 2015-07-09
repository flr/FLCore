#' Class FLIndex
#' 
#' A class for modelling abundance indices.
#' 
#' The \code{FLIndex} object holds data and parameters related to abundance
#' indices.
#'
#' @name FLIndex
#' @aliases FLIndex FLIndex,FLQuant-method FLIndex,missing-method
#' FLIndex-methods catch.n,FLIndex-method
#' catch.n<-,FLIndex,FLQuant-method catch.wt,FLIndex-method
#' catch.wt<-,FLIndex,FLQuant-method desc,FLIndex-method
#' desc<-,FLIndex,character-method distribution,FLIndex-method
#' distribution<-,FLIndex,character-method effort,FLIndex-method
#' effort<-,FLIndex,FLQuant-method index,FLIndex-method index.q,FLIndex-method
#' index.q<-,FLIndex,FLQuant-method index.var,FLIndex-method
#' index.var<-,FLIndex,FLQuant-method index<-,FLIndex,FLQuant-method missing
#' name,FLIndex-method name<-,FLIndex,character-method range,FLIndex-method
#' range<-,FLIndex,numeric-method sel.pattern,FLIndex-method
#' sel.pattern<-,FLIndex,FLQuant-method type,FLIndex-method
#' type<-,FLIndex,character-method
#' @docType class
#' @section Slots: \describe{ \item{type}{Type of index (\code{character}).}
#' \item{distribution}{Statistical distribution of the index values
#' (\code{character}).} \item{index}{Index values (\code{FLQuant}).}
#' \item{index.var}{Variance of the index (\code{FLQuant}).}
#' \item{catch.n}{Catch numbers used to create the index (\code{FLQuant}).}
#' \item{catch.wt}{Catch weight of the index (\code{FLQuant}).}
#' \item{effort}{Effort used to create the index (\code{FLQuant}).}
#' \item{sel.pattern}{Selection pattern for the index (\code{FLQuant}).}
#' \item{index.q}{Catchability of the index (\code{FLQuant}).} \item{name}{Name
#' of the stock (\code{character}).} \item{desc}{General description of the
#' object (\code{character}).} \item{range}{Range of the object
#' (\code{numeric})} }
#' @author The FLR Team
#' @seealso \link{computeCatch}, \link{dims}, \link{iter},
#' \link[graphics]{plot}, \link{propagate}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' # Create an FLIndex object.
#'   fli <- FLIndex(index=FLQuant(rnorm(8), dim=c(1,8)), name="myTestFLindex")
#'   summary(fli)
#'   index(fli)
#'
#' # Creat an FLIndex object using an existing FLQuant object.
#'   data(ple4)
#'   # Create a perfect index of abundance from abundance at age
#'     fli2 <- FLIndex(index=stock.n(ple4))
#'   # Add some noise around the signal
#'     index(fli2) <- index(fli2)*exp(rnorm(1, index(fli2)-index(fli2), 0.1))
#'