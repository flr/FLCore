#' Methods upper and lower
#' 
#' Accesor and replacement methods for the \code{lower} and \code{upper}
#' attributes of objects of class \code{\linkS4class{FLModel}}. These are
#' stored as part of the structure inside the \code{initial} slot. This slot
#' contains a function to be used to provide initial values to any of the
#' fitting method (e.g. \code{\link{fmle}}).
#' 
#' The values in \code{lower} and \code{upper} are only used if the method
#' selected for \code{\link[stats]{optim}} is able to make use of them, like
#' for example "L-BFGS-B", which is the default for \code{\link{fmle}}.
#' 
#' The exact location of this information could be changed (i.e. a separate
#' slot might be created), so code accessing it is encouraged to use these
#' accesor methods.
#' 
#' 
#' @name limits
#' @aliases upper upper,FLModel-method upper<- upper<-,FLModel,numeric-method
#' lower lower,FLModel-method lower<- lower<-,FLModel,numeric-method
#' @docType methods
#' @section Generic function: upper(object) upper<-(object, value)
#' lower(object) lower<-(object, value)
#' @author The FLR Team
#' @seealso \linkS4class{FLModel}
#' @keywords methods
#' @examples
#' 
#' data(nsher)
#' lower(nsher)
#' upper(nsher)
#' 