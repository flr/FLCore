#' Select or modify iterations of an FLR object
#' 
#' To extract or modify a subset of the iterations contained in an FLR object,
#' the \code{iter} and \code{iter<-} methods can be used.
#' 
#' In complex with various \code{FLQuant} slots, the \code{iter} method checks
#' whether individual slots contain more than one iteration, i.e.
#' \code{dims(object)[6] > 1}. If a particular slot contains a single
#' iteration, that is returned, otherwise the chosen one is selected. This is
#' in contrast with the subset operator \code{[}, which does not carry out this
#' check.
#' 
#' For objects of class \code{\link{FLModel}}, iters are extracted for slots of
#' classes \code{FLQuant}, \code{FLCohort} and \code{FLPar}.
#' 
#' 
#' @name iter
#' @aliases iter iter-methods iter,FLQuant,ANY-method iter,FLArray-method
#' iter,vector-method iter,FLComp-method iter,FLModel-method
#' iter,FLQuants-method iter,FLPar-method iter,logLik-method iter<-
#' iter<--methods iter<-,FLQuant,FLQuant-method iter<-,FLCohort,FLCohort-method
#' iter<-,FLComp,FLComp-method iter<-,FLPar,FLPar-method
#' iter<-,FLPar,numeric-method
#' @docType methods
#' @section Generic function: iter(object) iter<-(object,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLComp}, \linkS4class{FLQuant}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(800), dim=c(4,10,2), iter=10)
#' iter(flq, 2)
#' 
#' fls <- FLStock(catch.n=flq, m=FLQuant(0.2, dim=c(4,10,2)))
#' fls2 <- iter(fls, 2)
#' summary(fls2)
#' 