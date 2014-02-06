# classesArr.R - 
# FLCore/R/classesArr.R - 

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# FLArray {{{

#' Class FLArray
#' 
#' A basic 6D array class. No objects of this class are created in
#' FLCore, as it is used only for method inheritance.
#' 
#' @name FLArray
#' @aliases FLArray FLArray-class
#' @docType class
#' @section Slots: \describe{
#'   \item{.Data}{Internal S4 data representation, of class \code{array}.}
#' }
#' @section Validity: \describe{
#' \item{Dimensions:}{Array must have 6 dimensions}
#' \item{Content:}{Array must be of class \code{numeric}}
#' }
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}}, \code{\linkS4class{FLCohort}}
#' @keywords classes

setClass("FLArray",	representation("array"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1))),
	validity=function(object){
	# Make sure there are at least 6 dimensions in the array
	Dim  <-  dim(object)
	if (length(Dim) != 6) 
		return("the array must have 6 dimensions")

	if (!is.numeric(object) && !is.na(object)) 
		return("array is not numeric")

	# Everything is fine
	return(TRUE)
}
) # }}}

# FLQuant {{{

# FLQuant class
#' FLQuant class for numerical data
#' 
#' The \code{FLQuant} class is a six-dimensional \code{\link[base]{array}}
#' designed to store most quantitative data used in fisheries and population
#' modelling.
#' 
#' The six dimensions are named. The name of the first dimension can be
#' altered by the user from its default, \code{quant}. This could typically be
#' \code{age} or \code{length} for data related to natural populations. The
#' only name not accepted is 'cohort', as data structured along cohort should
#' be stored using the \code{\link{FLCohort}} class instead. Other dimensions
#' are always names as follows: \code{year}, for the calendar year of the
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
#' @aliases FLQuant-class FLQuant FLQuant-methods FLQuant,missing-method
#' FLQuant,vector-method FLQuant,array-method FLQuant,matrix-method
#' FLQuant,FLQuant-method
#' @family FLCoreClasses
#' @docType class
#'
#' @section Slots: \describe{
#'   \item{.Data}{A 6-D array for numeric data. \code{\link[base]{array}}.}
#'  \item{units}{Units of measurement. \code{\link[base]{character}}.} }
#' @section Validity: \describe{
#'  \item{Dimensions:}{Array must have 6 dimensions}
#'  \item{Content:}{Array must be of class \code{numeric}}
#'  \item{Dimnames:}{Dimensions 2 to 6 must be named "year", "unit", "season", "area" and "iter"}
#' }
#'
#' @section Constructor:
#'  The \code{FLQuant} method provides a flexible constructor for objects of the class.
#'  Inputs can be of class:
#'  \describe{
#'    \item{\code{vector}:}{A numeric vector will be placed along the year dimension by default.}
#'    \item{\code{matrix}:}{A matrix will be placed along dimensions 1 and 2, unless otherwise specified by 'dim'. The matrix dimnames will be used unless overriden by 'dimnames'.}
#'    \item{\link[base]{array}:}{As above}
#'    \item{\link[base]{missing}:}{If no input is given, an empty \code{FLQuant}  (NA) is returned, but dimensions and dimnames can still be specified.} }
#' 
#'  Additional arguments to the constructor: 
#'  \describe{
#'     \item{units:}{The units of measurement, a \code{\link[base]{character}} string.}
#'     \item{dim:}{The dimensions of the object, a \code{\link[base]{numeric}} vector of length 6.}
#'     \item{dimnames:}{A \code{\link[base]{list}} object providing the dimnames of the array. Only those different from the default ones need to be specified.}
#'     \item{quant:}{The name of the first dimension, if different from 'quant', as a \code{\link[base]{character}} string.} }
#'
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}}
#' @keywords classes
#' @examples
#' 
#' # creating a new FLQuant
#' flq <- FLQuant()
#' flq <- FLQuant(1:10, dim=c(2,5))
#' summary(flq)
#' 
#' # Vectors are used column first...
#' dim(FLQuant(1:10))
#' # ...while matrices go row first.
#' dim(FLQuant(matrix(1:10)))
#' 
#' FLQuant(matrix(rnorm(100), ncol=20))
#' 
#' FLQuant(array(rnorm(100), dim=c(5,2,1,1,1,10)))
#' FLQuant(array(rnorm(100), dim=c(5,2)), iter=10)
#' 
#' # working with FLQuant objects
#' flq <- FLQuant(rnorm(200), dimnames=list(age=1:5, year=2000:2008), units='diff')
#' summary(flq)
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

setClass("FLQuant",
	representation("FLArray", units="character"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter="1")), units="NA"),
# VALIDITY
	validity=function(object){
	
	DimNames  <-  names(dimnames(object))
	# First dimension cannot be called cohort
	if(DimNames[1] == "cohort")
		return("first dimension cannot be named 'cohort'")
	# Make sure there are at least 6 dimensions in the array named
  if (length(DimNames) != 6)
    return("the array must have 6 dimensions")
	# *, "year", "unit", "season", "area" and "iter"
  if (!all(DimNames[2:6] == c("year", "unit", "season", "area", "iter")))
    return("dimension names of the array are incorrect")
	# array is numeric
	if (!is.numeric(object) && !is.na(object))
		return("array is not numeric")

	# Everything is fine
	return(TRUE)
}) # }}}

# FLQuantPoint    {{{

#' Class FLQuantPoint
#' 
#' The \code{FLQuantPoint} class summarizes the contents of an \code{FLQuant}
#' object with multiple iterations along its sixth dimension using a number of
#' descriptive statistics.
#' 
#' An object of this class has a set structure along its sixth dimension
#' (\emph{iter}), which will always be of length 5, and with dimnames
#' \emph{mean}, \emph{median}, \emph{var}, \emph{uppq} and \emph{lowq}. They
#' refer, respectively, to the sample mean, sample median, variance, and lower
#' (0.25) and upper (0.75) quantiles.
#' 
#' Objects of this class wil be typically created from an \code{FLQuant}. The
#' various statistics are calculated along the \emph{iter} dimension of the
#' original \code{FLQuant} using \code{\link[base]{apply}}.
#' 
#' @name FLQuantPoint
#' @aliases FLQuantPoint-class FLQuantPoint FLQuantPoint-methods
#' FLQuantPoint,FLQuant-method
#' @docType class
#' @section Slots: \describe{
#'  \item{.Data}{The main array holding the computed statistics. \code{array}.}
#'  \item{units}{Units of measurement. \code{character}.}
#' }
#' @section Accesors: \describe{
#'  \item{mean,mean<-:}{'mean' element on 6th dimension, arithmetic mean.}
#'  \item{median,median<-:}{'median' element on 6th dimension, median.}
#'  \item{var,var<-:}{'var' element on 6th dimension, variance.}
#'  \item{lowq,lowq<-:}{'lowq' element on 6th dimension, lower quantile (0.25 by default).}
#'  \item{uppq,uppq<-:}{'uppq' element on 6th dimension, upper quantile (0.75 by default).}
#' }
#' @section Constructor:
#'  Inputs can be of class:
#'  \describe{
#'    \item{\code{FLQuant}:}{An FLQuant object with iters (i.e. dim[6] > 1)}
#' }
#' @section Validity: \describe{
#'  \item{iter:}{iter dimension is of length 5.}
#'  \item{Dimnames:}{iter dimnames are 'mean', 'median', 'var', 'uppq' and'lowq'}
#' }
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords classes
#' @examples
#' 
#' flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
#' flqp <- FLQuantPoint(flq)
#' summary(flqp)
#' mean(flqp)
#' var(flqp)
#' rnorm(200, flqp)
#' 

setClass("FLQuantPoint",
    representation("FLQuant"),
	prototype(new('FLQuant', array(as.numeric(NA), dim=c(1,1,1,1,1,5),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter=c('mean', 'median', 'var', 'uppq', 'lowq'))), units="NA")),
	# VALIDITY
    validity=function(object) {
    
    # iter dimensions is of length 5 and with names:
    if(dim(object)[6] != 5)
        return("dims of object do not match those of the FLQuantPoint class")

    # dimnames are 'mean', 'median', 'var', 'uppq', 'lowq'
    if(any(dimnames(object)$iter != c('mean', 'median', 'var', 'uppq', 'lowq')))
        return("dimnames of object do not match those of the FLQuantPoint class")
    
	# Everything is fine
    return(TRUE)
	}
) # }}}

# FLQuantDistr    {{{
validFLQuantDistr <- function(object) {

	# .Data & var have same dims
	if(!all(dim(object@.Data) == dim(object@var)))
		return("object slots' dimensions must match")

	# .Data & var have same dimnames
	if(!all.equal(dimnames(object@.Data), dimnames(object@var)))
		return("object slots' dimnames must match")

	# Everything is fine
    return(TRUE)
}
setClass("FLQuantDistr",
    representation("FLQuant", var="FLArray", distr="character"),
	prototype(new("FLQuant"), var=new("FLArray"), distr="lnorm"))

setValidity("FLQuantDistr", validFLQuantDistr)
remove(validFLQuantDistr)   # }}}

# FLCohort {{{
validFLCohort <-  function(object) {
  # names
  if(!all.equal(names(dimnames(object)), 
      c("age", "cohort", "unit", "season", "area", "iter")))
    return("names of FLCohort object are not correct")

	# Everything is fine
  return(TRUE)
}

setClass("FLCohort",
	representation("FLArray"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(age="1", cohort="1", unit="unique", season="all", area="unique",
		iter="none")), units="NA"),
  validity=validFLCohort
) # }}}

# FLPar {{{
validFLPar <- function(object) {

	# Last dimension is called 'iter' ...
  if(names(dimnames(object))[length(dim(object))] != "iter")
    return("last dimension must be named 'iter'")
  # ... and the first 'params'

	return(TRUE)
}

setClass('FLPar', representation('array', units='character'),
	prototype=prototype(array(as.numeric(NA), dim=c(1,1),
	dimnames=list(param="", iter=1)), units='NA'), validity=validFLPar)
remove(validFLPar)
# }}}
