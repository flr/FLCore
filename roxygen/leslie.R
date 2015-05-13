#' Method for calculating Leslie matrix dynamics of an FLBiol object
#' 
#' For an \code{FLBiol} object with the natural mortality-at-age, fecundity and
#' spwn data present in the object.
#' 
#' Usual Leslie matrix type dynamics for a \code{FLBiol} object.
#' 
#' @aliases leslie leslie-methods leslie,FLBiol-method
#' @param object An object of type \code{\linkS4class{FLBiol}}.
#' @param \dots Extra arguments accepted by each implementation.
#' @return An object of class \code{\linkS4class{FLBiol}}.
#' @author FLR Team
#' @seealso \code{\linkS4class{FLBiol}}
#' @references Leslie, P.H. (1945) The use of matrices in certain population
#' mathematics. Biometrika,33(3), 183-212.
#' 
#' Leslie, P.H. (1948) Some further notes on the use of matrices in population
#' mathematics. Biometrika, 35(3-4), 213-245.
#' @keywords methods
#' @examples
#' 
#' data(ple4.biol)
#' ple4.l <- leslie(ple4.biol,plusgroup=FALSE)
#' 