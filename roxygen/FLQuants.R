#' Class FLQuants
#' 
#' \code{FLQuants} is a \code{list} of \code{FLQuant} objects. It is very
#' similar to the standard \code{list} class. It implements a lock mechanism
#' that, when turned on, does not allow the user to increase or decrease the
#' object length. The elements of \code{FLQuants} must all be of class
#' \code{FLQuant}.
#' 
#' 
#' @name FLQuants
#' @aliases FLQuants FLQuants,ANY-method FLQuants,FLQuants-method
#' FLQuants,list-method FLQuants,missing-method FLQuants-class FLQuants-methods
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#'
#' # Compute various FLStock indicators
#'   data(ple4)
#'   fqs <- FLQuants(ssb=ssb(ple4), catch=catch(ple4), rec=rec(ple4), f=fbar(ple4))
#'   summary(fqs)
#'   xyplot(data~year|qname, fqs, type='b', scales=list(relation='free'))

