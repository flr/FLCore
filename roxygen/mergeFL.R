#' Merging FLStock objects
#' 
#' Two FLStock object can be \emph{merged} using this method or a plus sign.
#' Catch slots are added, and weight slots are averaged, weighted by the
#' relative catches. No meaningful calculation is currently done for harvest,
#' harvest.spwn, m, and m.spwn.
#' 
#' 
#' @name mergeFL
#' @aliases mergeFLStock +,FLStock,FLStock-method
#' @docType methods
#' @section Methods: \describe{ \item{ signature(e1=FLStock, e2=FLStock)
#' :}{Adds two FLStock objects} }
#' @author The FLR Team
#' @seealso \link{FLStock}
#' @keywords methods