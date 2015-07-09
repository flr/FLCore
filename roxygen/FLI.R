#' Class FLI
#' 
#' A VIRTUAL class that holds data and parameters related to abundance indices.
#' 
#' @name FLI
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