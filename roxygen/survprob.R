#' Calculating survival probabilties given mortality in the FLBiol object
#' 
#' For an \code{FLBiol} object with the mortality-at-quant this method
#' calculates the associated survival probability-at-quant. This can be later
#' used by the \code{r()} method. The calculation can be carried out either by
#' year or by cohort.
#' 
#' Calculates the survival probability-at-quant given the mortality information
#' in an \code{\linkS4class{FLBiol}} object - survival probability from one
#' year to the next is simply exp(-M) and the survival probaiblity to a given
#' quant is merely the product along the quant dimension of the individual
#' survival probabilities.
#' 
#' @aliases survprob survprob-methods survprob,FLBiol-method
#' @param object An object of type \code{\linkS4class{FLBiol}}.
#' @param \dots Extra arguments accepted by each implementation.
#' @return An object of class \code{\linkS4class{FLQuant}}.
#' @author FLR Team
#' @seealso \code{\linkS4class{FLBiol}}
#' @keywords methods
#' @examples
#' \dontrun{
#' data(nsher.biol)
#' nsh.ps <- survprob(nsh.biol,by='year')
#' }
#' 