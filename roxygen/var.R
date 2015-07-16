#' Method var
#'
#' Variance of an FLPar
#' 
#' \code{var} computes the variance of an \code{\link{FLPar}} object along the
#' first dimension (\code{iter}) returning a value for each column
#' (\code{param})
#' 
#' By default, arguments \code{na.rm} and \code{use} have values of
#' \code{FALSE} and \code{'all.obs'} respectively. See the
#' \code{\link[stats]{var}} help page for more information on possible argument
#' values.
#'
#' @name var
#' @aliases var,FLPar,missing,missing,missing-method var,FLPar-method
#' @docType methods
#' @section Generic function: var(x, y, na.rm, use)
#' @author The FLR Team
#' @seealso \code{\link[stats]{var}}, \code{\linkS4class{FLPar}}
#' @keywords methods
#' @examples
#' 
#' flp <- FLPar(rnorm(200), params=c('a', 'b'))
#' var(flp)
#'