#' Class FLlst
#' 
#' \code{FLlst} is a class that extends \code{list} but implements a set of
#' features that give a little more structure to list objects. First the
#' elements of \code{FLlst} must all be of the same class. Second it implements
#' a lock mechanism that, when turned on, does not allow the user to increase
#' or decrease the object length.
#' 
#' 
#' @name FLlst
#' @aliases FLlst-class FLlst FLlst-methods FLlst,ANY-method
#' FLlst,missing-method FLlst,list-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{[[<-},
#' \link[base]{$<-}, \link[methods]{coerce}, \link[base]{lapply},
#' \link[stats]{window}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' fll01 <- new("FLlst", list(a=1:10, b=10:20))
#' fll02 <- new("FLlst", list(1:10, 10:20), names=c("a","b"))
#' fll03 <- FLlst(a=1:10, b=10:20)
#' fll04 <- FLlst(list(a=1:10, b=10:20))
#' fll05 <- FLlst(c(1:10), c(10:20))
#' names(fll05) <- names(fll01)
#' names(fll01)
#' 