#' Method rnorm
#' 
#' Generates random numbers following a normal distribution. \emph{mean} and
#' \emph{sd} can be specified as objects of class \code{\linkS4class{FLQuant}},
#' of the same dimensions, but any of the two could be given as a numeric, in
#' which case the value will be reused accordingly. If either is of class
#' \code{FLQuant} then the resultant object will be of class \code{FLQuant}.
#'
#' @name rnorm
#' @aliases rnorm,numeric,FLQuant,FLQuant-method
#' rnorm,numeric,FLQuant,missing-method rnorm,numeric,FLQuant,numeric-method
#' rnorm,numeric,numeric,FLQuant-method rnorm,numeric,missing,FLQuant-method
#' rnorm,numeric,FLQuantPoint,missing-method
#' @docType methods
#' @section Generic function: rnorm(n, mean, sd)
#' @author The FLR Team
#' @seealso \link[stats]{rnorm}, \linkS4class{FLQuant},
#' \linkS4class{FLQuantPoint}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' rnorm(10,mean=harvest(ple4)[,"2001"], sd=harvest(ple4)[,"2001"])
#' 