#' Method ssn
#' 
#' Returns the Spawning Stock Numbers of \code{\linkS4class{FLBiol}} objects.
#'
#' \code{ssn} is calculated as follows:
#'
#'  	\eqn{SSB = sum(N * mat * exp(-M*propM))}
#'
#' @name ssn
#' @aliases ssn ssn-methods ssn,FLBiol-method
#' @docType methods
#' @section Generic function: ssn(object)
#' @author The FLR Team
#' @seealso \link{FLComp} \link{FLStock}
#' @keywords methods
#' @examples
#'
#'  data(ple4.biol)
#' # Change spwn slot to make the calculation more interesting
#'   spwn(ple4.biol)<-0.5
#'
#' # Calculate ssn and check
#'  ssn(ple4.biol)
#'  ssnp <- quantSums(n(ple4.biol) * fec(ple4.biol) *
#'    exp(-m(ple4.biol)*spwn(ple4.biol)))
#'  ssn(ple4.biol)-ssnp
#'