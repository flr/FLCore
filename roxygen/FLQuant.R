#' FLQuant class for numerical data
#' 
#' The \code{FLQuant} class is a six-dimensional \code{\link[base]{array}}
#' designed to store most quantitative data used in fisheries and population
#' modelling.
#' 
#' The six dimensions are named. The name of the first dimension can be altered
#' by the user from its default, \code{quant}. This could typically be
#' \code{age} or \code{length} for data related to natural populations. The
#' only name not accepted is 'cohort', as data structured along cohort should
#' be stored using the \code{\link{FLCohort}} class instead. Other dimensions
#' are always named as follows: \code{year}, for the calendar year of the
#' datapoint; \code{unit}, for any kind of division of the population, e.g. by
#' sex; \code{season}, for any temporal strata shorter than year; \code{area},
#' for any kind of spatial stratification; and \code{iter}, for replicates
#' obtained through bootstrap, simulation or Bayesian analysis.
#' 
#' In addition, \code{FLQuant} objects contain a \code{units} attribute, of
#' class \code{\link[base]{character}}, intended to contain the units of
#' measurement relevant to the data.
#' 
#' @name FLQuant
#' @aliases FLQuant FLQuant,FLQuant-method FLQuant,array-method
#' FLQuant,matrix-method FLQuant,missing-method FLQuant,vector-method
#' FLQuant-class FLQuant-methods
#' @docType class
#' @section Slots: \describe{ \item{.Data}{A 6-D array for numeric data.
#' \code{\link[base]{array}}.} \item{units}{Units of measurement.
#' \code{\link[base]{character}}.} }
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}}
#' @keywords classes
#' @examples
#' 
#' # creating a new FLQuant
#'   flq <- FLQuant()
#'   flq <- FLQuant(1:10, dim=c(2,5))
#'   summary(flq)
#' 
#' # Vectors are used column first...
#'   dim(FLQuant(1:10))
#' # ...while matrices go row first.
#'   dim(FLQuant(matrix(1:10)))
#' 
#' FLQuant(matrix(rnorm(100), ncol=20))
#' 
#' FLQuant(array(rnorm(100), dim=c(5,2,1,1,1,10)))
#' FLQuant(array(rnorm(10), dim=c(5,2)), iter=10)
#' 
#' # working with FLQuant objects
#'   flq <- FLQuant(rnorm(45), dimnames=list(age=1:5, year=2000:2008), units='diff')
#'   summary(flq)
#' 
#' flq[1,]
#' flq[,1]
#' flq[1,1] <- 0
#' 
#' units(flq)
#' quant(flq)
#' 
#' plot(flq)
#' 