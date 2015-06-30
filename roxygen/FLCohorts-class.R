#' Class FLCohorts
#' 
#' \code{FLCohorts} is a class that extends \code{list} through \code{FLlst}
#' but implements a set of features that give a little more structure to
#' list objects. The elements of \code{FLCohorts} must all be of class
#' \code{FLCohort}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#' 
#' 
#' @name FLCohorts
#' @aliases FLCohorts-class FLCohorts FLCohorts-methods FLCohorts,ANY-method
#' FLCohorts,missing-method FLCohorts,list-method FLCohorts,FLCohorts-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}}
#' \item{names}{Names of the list elements. \code{character}}
#' \item{desc}{Description of the object. \code{character}} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}} }
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes