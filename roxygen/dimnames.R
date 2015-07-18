#' Method dimnames
#'
#' Modify dimnames of an FLQuant
#' 
#' The \code{dimnames<-} method for objects of class \code{\link{FLQuant}}
#' modifies the \code{dimnames} attribute. In contrast with the method for
#' class \code{array}, an incomplete named list of dimension names can be
#' provided. Only the relevant dimensions will be modify.
#' 
#' It is posible to modify the name of the first dimension (by default
#' \code{quant}) using this method.
#'
#' @name dimnames<-
#' @aliases dimnames<-,ANY,missing-method dimnames<-,FLQuant,list-method
#' dimnames<-,FLStock,list-method
#' @docType methods
#' @section Generic function: dimnames<-(x,value)
#' @author The FLR Team
#' @seealso \code{\link[base]{dimnames}}, \code{\link{FLQuant}},
#' \code{\link[base]{array}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(80), dim=c(4,10,2))
#' dimnames(flq) <- list(unit=c('male', 'female'))
#'
#' # This modifies both dimnames and dimnames name
#'   dimnames(flq) <- list(age=0:3)
#'   dimnames(flq)
#' 