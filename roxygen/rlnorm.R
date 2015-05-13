#' Method rlnorm
#' 
#' Random generation for the log normal distribution whose logarithm has mean
#' equal to \emph{meanlog} and standard deviation equal to \emph{sdlog}.
#' \emph{meanlog} and \emph{sdlog} can be given as FLQuant objects.  If both
#' are given as FLQuant objects their dimensions must be the same.  If either
#' of these arguments are FLQuant objects, rlnorm returns an FLQuant.
#' 
#' 
#' @name rlnorm
#' @aliases rlnorm,numeric,FLQuant,FLQuant-method
#' rlnorm,numeric,FLQuant,missing-method rlnorm,numeric,FLQuant,numeric-method
#' rlnorm,numeric,numeric,FLQuant-method rlnorm,numeric,missing,FLQuant-method
#' rlnorm,numeric,FLQuantPoint,missing-method
#' @docType methods
#' @section Generic function: rlnorm(n,meanlog,sdlog)
#' @author The FLR Team
#' @seealso \link[stats]{rlnorm}, \linkS4class{FLQuant},
#' \linkS4class{FLQuantPoint}
#' @keywords methods
#' @examples
#' 
#' out <- rlnorm(1000, meanlog=FLQuant(c(5,5,5,5,5)),sdlog=FLQuant(c(0,1,2,3,4)))
#' apply(log(out),2,sd)
#' apply(log(out),2,mean)
#' 