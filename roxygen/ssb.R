#' Method ssb
#' 
#' Returns the spawning stock biomass of \code{FLStock} and \code{FLBiol}
#' objects.
#' 
#' If spawning occurs at the beginning of the year, the calculated SSB is the
#' same regardless of the units of the harvest slot. If spawning occurs at any
#' other time during the year such that the stock is subject to fishing
#' mortality prior to spawning, then the calculated SSB will depend on the units
#' of the harvest slot (either 'f' or 'hr').
#'
#' For an \code{FLStock} with harvest units 'f', SSB is calculated as
#'
#'    \eqn(SSB = sum(N*exp(-F*propF-M*propM) * wt * mat)}
#'
#' For an \code{FLStock} with harvest units 'hr', SSB is calculated as
#'
#'		\eqn{SSB = sum(N*(1-harvest*propF)*exp(-M*propM) * wt * mat)}
#'
#' The units of the harvest slot in the \code{FLStock} object must be specified
#' as either 'f' for an instantaneous fishing mortality or else as 'hr' for a
#' harvest rate.
#'
#' For an \code{FLBiol} the spawning biomass is calculated as
#'
#'  	\eqn{SSB = sum(N * wt * mat * exp(-M*propM))}
#'
#' @name ssb
#' @aliases ssb ssb-methods ssb,FLStock-method ssb,FLBiol-method
#' @docType methods
#' @section Generic function: ssb(object)
#' @author The FLR Team
#' @seealso \linkS4class{FLBiol} \linkS4class{FLStock}
#' @keywords methods
#' @examples
#' 
#' # For an FLStock
#'   data(ple4)
#' # Change spwn slots to make the calculation more interesting
#'   harvest.spwn(ple4)<-0.5
#'   m.spwn(ple4)<-0.5
#'
#' # check the units of the harvest slot
#'   units(harvest(ple4))
#'
#' # ssb with F
#'   ssb(ple4)
#'
#'   # Recalculate ssb with F and check
#'     ssbF <- quantSums(stock.n(ple4) * stock.wt(ple4) * mat(ple4) *
#'       exp(-harvest(ple4) * harvest.spwn(ple4) - m(ple4) * m.spwn(ple4)))
#'     ssb(ple4)-ssbF
#'
#' # ssb with hr
#'   harvest(ple4) <- hr
#'   ssb(ple4)
#'
#'   # Recalculate ssb with hr and check
#'     hr <- catch.n(ple4) / stock.n(ple4)
#'     units(hr) <- 'hr'
#'     ssbHR <- quantSums(stock.n(ple4) * stock.wt(ple4) * mat(ple4) *
#'       (1 - hr * harvest.spwn(ple4)) * exp(-m(ple4) * m.spwn(ple4)))
#'     ssb(ple4)-ssbHR
#'
#' # For an FLBiol
#'   data(ple4.biol)
#' # Change spwn slot to make the calculation more interesting
#'   spwn(ple4.biol)<-0.5
#'
#' # Calculate ssb and check
#'  ssb(ple4.biol)
#'  ssbp <- quantSums(n(ple4.biol) * fec(ple4.biol) * wt(ple4.biol) *
#'    exp(-m(ple4.biol)*spwn(ple4.biol)))
#'  ssb(ple4.biol)-ssbp
#'