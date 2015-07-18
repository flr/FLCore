#' Method mergeFL
#'
#' Merging FLStock objects
#' 
#' Two FLStock objects with the same dimensions can be merged using this method
#'  or a plus sign. Catch slots are added, and weight slots are averaged,
#' weighted by the relative catch numbers. No meaningful calculation is
#' currently done for harvest, harvest.spwn, m and m.spwn; currently, NAs are
#' inserted into the harvest slot, and unweighted averages used for the others.
#'
#' @name mergeFL
#' @aliases mergeFLStock +,FLStock,FLStock-method
#' @docType methods
#' @section Methods: \describe{ \item{ signature(e1=FLStock, e2=FLStock)
#' :}{Adds two FLStock objects} }
#' @author The FLR Team
#' @seealso \link{FLStock}
#' @keywords methods
#'