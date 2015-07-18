#' Method survprob
#'
#' Calculating survival probabilties given mortality in the \code{FLBiol} object
#' 
#' For an \code{FLBiol} object with the mortality-at-quant present, this method
#' calculates the associated survival probability-at-quant. This can be used
#' later by the \code{r()} method. The calculation can be carried out either by
#' year or by cohort.
#' 
#' Individual survival probability (i.e. from one year to the next) is simply
#' exp(-M) and the survival probaiblity for a given quant is the cumulative
#' product along the quant dimension of the individual survival probabilities.
#' 
#' @aliases survprob survprob-methods survprob,FLBiol-method
#' @param object An object of type \code{\linkS4class{FLBiol}}.
#' @param \dots Extra arguments accepted by each implementation.
#' @return An object of class \code{\linkS4class{FLQuant}}.
#' @author FLR Team
#' @seealso \code{\linkS4class{FLBiol}}
#' @keywords methods
#' @examples
#'
#' data(ple4.biol)
#' survprob(ple4.biol,by='year')
#'