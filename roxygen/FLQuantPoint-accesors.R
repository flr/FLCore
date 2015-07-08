#' Method lowq
#' 
#' These are the accesor and replacement methods for the various elements
#' stored in an \code{\linkS4class{FLQuantPoint}} object along the sixth
#' dimension.
#' 
#' @name FLQuantPoint-accesors
#' @aliases FLQuantPoint-accesors lowq lowq-methods lowq,FLQuantPoint-method
#' lowq<- lowq<--methods lowq<-,FLQuantPoint-method mean,FLQuantPoint-method
#' mean<- mean<--methods mean<-,FLQuantPoint-method
#' median,FLQuantPoint,missing-method median,FLQuantPoint-method median<-
#' median<--methods median<-,FLQuantPoint-method uppq uppq-methods
#' uppq,FLQuantPoint-method uppq<- uppq<--methods uppq<-,FLQuantPoint-method
#' var,FLQuantPoint-method var,FLQuantPoint,missing,missing,missing-method
#' var<- var<--methods var<-,FLQuantPoint-method
#' @docType methods
#' @section Generic function: lowq(x) lowq<-(x,value) mean(x) mean<-(x,value)
#' median(x,na.rm) median<-(x,value) uppq(x) uppq<-(x,value) var(x,y,na.rm,use)
#' var<-(x,value)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(40000), dim=c(10,20,1,1,1,200))
#' flqp <- FLQuantPoint(flq)
#' mean(flqp)
#' # Replace the mean by something else
#'   mean(flqp) <- FLQuant(rnorm(200, 10, 3), dim=c(10,20))
#' 