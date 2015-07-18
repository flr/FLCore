#' Method Extract
#'
#' Extract or replace parts of an FLR Object
#' 
#' Operators acting on FLQuant, FLCohort, FLPar, FLComp, and derived classes to
#' extract or replace sections of an object.
#' 
#' Please note the differences between referencing sections of an object by
#' position using values of class \code{numeric}, or by using dimnames of class
#' \code{character}. See examples below.
#' 
#' All classes that are derived from \code{FLComp} (for example, \code{FLStock}
#' and \code{FLBiol}) can be subset along the six dimensions of their
#' \code{FLQuant} slots.
#' 
#' Classes that are derived from \code{FLlst} (for example, \code{FLStocks} and
#' \code{FLBiols}) can be subset in a similar way to ordinary list objects.
#'
#' @name Extract
#' @aliases Extract-FLCore [,FLArray,ANY,ANY-method [,FLArray,array,ANY-method
#' [,FLComp,ANY,ANY-method [,FLlst,ANY,missing-method [,FLPar,ANY,ANY-method
#' [,FLStock,ANY,ANY-method [,FLCohort,ANY,ANY-method [,FLIndex,ANY,ANY-method
#' [<-,FLArray,ANY,ANY,ANY-method [<-,FLComp,ANY,ANY,ANY-method
#' [<-,FLlst,ANY,missing,ANY-method [<-,FLPar,ANY,ANY,ANY-method
#' [<-,FLStock,ANY,ANY,FLStock-method [[<-,FLlst,ANY,missing-method
#' $<-,FLlst,character-method
#' @docType methods
#' @section Generic function: \describe{ \item{}{[x,i,j,drop]}
#' \item{}{[<-(x,i,j,value)} \item{}{[[<-(x,i,j,value)}
#' \item{}{\$<-(x,name,value)} }
#' @author The FLR Team
#' @seealso \link[base]{Extract}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(200), dimnames=list(age=0:4, year=1991:2000,
#'   season=1:4))
#'
#' # Extracting by position...
#'   flq[1,]
#'   flq[,1:5]
#'   flq[1:2,,,c(1,3)]
#'
#' # ...by dimnames
#'   flq['0',]
#'   flq[,'1991']
#'   flq[,as.character(1991:1995),,'1']
#'
#' # Replacing part of the object
#'   flq['0',,,1]<-0
#'