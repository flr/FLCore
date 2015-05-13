#' Method range
#' 
#' Extraction and modification of the \emph{range} slot from objects of any
#' class inheriting from \code{\linkS4class{FLComp}}.
#' 
#' 
#' @name range
#' @aliases range-methods range,FLComp,missing-method range,FLComp-method
#' range,FLlst-method range<- range<--methods range<-,FLComp-method
#' @docType methods
#' @section Generic function: range(x, i) range<-(x, i, value)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # example FLStock
#' data(ple4)
#' 
#' range(ple4)
#' 
#' range(ple4, 'plusgroup')
#' 
#' range(ple4, 'plusgroup') <- 14
#' 
#' 