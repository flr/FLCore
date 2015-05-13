#' Class FLCohort
#' 
#' This class represents cohorts in columns. It simply shifts the typical
#' matrix representation where cohorts are found on the diagonals, into a
#' matrix where cohorts are found in columns. It is very usefull for all
#' analysis that want to make use of cohorts instead of years.
#' 
#' 
#' @name FLCohort
#' @aliases FLCohort-class FLCohort FLCohort-methods FLCohort,FLQuant-method
#' FLCohort,FLCohort-method FLCohort,array-method FLCohort,missing-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{Internal S4 data representation.
#' \code{array}.} \item{units}{The data units in some understandable metric.
#' \code{character}} }
#' @author The FLR Team
#' @seealso \link{[}, \link{as.data.frame}, \link{bubbles}, \link{ccplot},
#' \link{FLCohort,FLQuant-method}, \link{flc2flq}, \link[graphics]{plot},
#' \link{quant}, \link{trim}, \link{units},
#' \link{units<-,FLCohort,character-method}, \link[lattice]{xyplot},
#' \link[base]{array}
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' flq <- catch.n(ple4)
#' flc <- FLCohort(flq)
#' plot(trim(flc, cohort=1960:2000))
#' 