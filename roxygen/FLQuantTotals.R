#' Method quantTotals
#' 
#' 
#' These methods return an object of same dimensions as the input but with the
#' sums along the first (\code{yearTotals}) or second dimension
#' (\code{quantTotals}). Although the names might appear contradictory, it must
#' be noted that what each method really returns are the totals over the
#' selected dimension.
#' 
#' 
#' @name FLQuantTotals
#' @aliases quantTotals quantTotals-methods quantTotals,FLQuant-method
#' yearTotals yearTotals-methods yearTotals,FLQuant-method
#' @docType methods
#' @section Generic function: quantTotals(x)
#' 
#' yearTotals(x)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#'  flq <- FLQuant(rlnorm(100), dim=c(10,10))
#'  quantTotals(flq)
#'  # See how the values obtained by yearSums are being replicated
#'  yearSums(flq)
#'  # Get the proportions by quant
#'  flq / quantTotals(flq)
#'  # or year
#'  flq / yearTotals(flq)
#'  