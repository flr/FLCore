#' Method mean.lifespan
#'
#' Method for calculating mean lifespan, given the natural mortality
#' 
#' Used for an \code{FLBiol} object with natural mortality-at-age available
#' in the object.
#' 
#' Using actuarial definitions for the expected life-span of a given species
#' and survival rate-at-age (natural mortality), we can compute the expected
#' life-span, \eqn{\ell_x}{logL(x)}, of a species, from a given reference age
#' \eqn{x}, using the following equation:
#' 
#' \deqn{\ell_x = \sum\limits_{t=1}^{\infty}
#' \exp\left(-\sum\limits_{i=x}^{x+t}M_i\right)}{logL(x) = SUM_1_inf
#' exp(SUM_i=x_x+t m_i)}.
#' 
#' The method accepts objects of class \code{FLBiol} of any particular
#' dimension. If the object has a seasonal structure to the population
#' dynamics, then we sum over all seasons to get the yearly survival rate.
#' 
#' @aliases mean.lifespan mean.lifespan-methods mean.lifespan,FLBiol-method
#' @param x An object of type \code{\linkS4class{FLBiol}}.
#' @param \dots Extra arguments accepted by each implementation.
#' @return An object of class \code{\linkS4class{FLQuant}} whose first and
#' second dimension is of length one.
#' @author FLR Team
#' @seealso \code{\linkS4class{FLBiol}}
#' @keywords methods
#' @examples
#'
#' data(ple4.biol)
#' lfs.ple4 <- mean.lifespan(ple4.biol,ref.age=1)
#'