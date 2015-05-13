#' Population variability
#' 
#' The \code{pv} method computes the population variability (\emph{pv}) of an
#' \code{FLQuant} object.
#' 
#' 
#' @name pv
#' @aliases pv pv-methods pv,FLQuant-method
#' @docType methods
#' @section Generic function: pv(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @references Heath, J.P. 2006. Quantifying temporal variability in population
#' abundances. \emph{Oikos} \bold{115 (3)}: 573--581.
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(40), dim=c(1,40))
#' pv(flq)
#' 
#' data(ple4)
#' pv(stock(ple4))
#' 