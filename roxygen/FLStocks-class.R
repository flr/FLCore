#' Class FLStocks
#' 
#' \code{FLStocks} is a class that extends \code{list} through \code{FLlst} but
#' implements a set of features that give a little bit more structure to list
#' objects. The elements of \code{FLStocks} must all be of class
#' \code{FLStock}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#' 
#' 
#' @name FLStocks
#' @aliases FLStocks-class FLStocks FLStocks-methods FLStocks,ANY-method
#' FLStocks,missing-method FLStocks,list-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @author The FLR Team
#' @seealso \link[graphics]{plot}, \link{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' fls <- FLStocks(sa=ple4, sb=window(ple4, end=1980))
#' summary(fls)
#' 