#' Class FLIndices
#' 
#' \code{FLIndices} is a class that extends \code{list} through \code{FLlst}
#' but implements a set of features that give a little more structure to
#' list objects. The elements of \code{FLIndices} must all be of class
#' \code{FLIndex}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#'
#' @name FLIndices
#' @aliases FLIndices-class FLIndices FLIndices-methods FLIndices,ANY-method
#' FLIndices,missing-method FLIndices,list-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @author The FLR Team
#' @seealso \linkS4class{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' data(ple4.index)
#' flis <- FLIndices(INDa=ple4.index, INDb=window(ple4.index, end=2000))
#' 