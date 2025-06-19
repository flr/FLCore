# classesArr.R - Array-based classes
# FLCore/R/classesArr.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

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
#' @aliases FLQuant-class FLQuant FLQuant-methods
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
#' @param object Input numeric object
#' @param dim Vector of dimension lengths
#' @param dimnames List of dimension names
#' @param quant Character vector for name of first dimension
#' @param units Character vctor of units of measurement, see \link[FLCore]{uom}
#' @param iter Number of iterations, i.e. length of the 6th dimension
#' @param fill.iter Should iterations be filled with the same content as the first?
#' @param ... Additional arguments
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
	prototype(new('FLArray', array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter="1"))), units="NA"),
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
#' mean,FLQuantPoint-method mean<-,FLQuantPoint,FLQuant-method
#' median,FLQuantPoint-method median<-,FLQuantPoint,FLQuant-method
#' var,FLQuantPoint-method var<-,FLQuantPoint,FLQuant-method
#' lowq,FLQuantPoint-method lowq<-,FLQuantPoint,FLQuant-method
#' uppq,FLQuantPoint-method uppq<-,FLQuantPoint,FLQuant-method
#' quantile,FLQuantPoint-method
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
#'  \item{quantile:}{returns the 'lowq' or 'uppq' iter, depending on the value of 'probs' (0.25 or 0.75).}
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
#' @param object Input numeric object
#' @param ... Additonal arguments
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords classes
#' @examples
#' 
#' flq <- FLQuant(rlnorm(2000), dim=c(10,20,1,1,1,200), units="kg")
#' flqp <- FLQuantPoint(flq)
#' flqp <- FLQuantPoint(flq, probs=c(0.05, 0.95))
#' summary(flqp)
#' mean(flqp)
#' var(flqp)
#' rnorm(200, flqp)

setClass("FLQuantPoint",
    representation("FLQuant", n="numeric"),
	prototype(new('FLQuant', array(as.numeric(NA), dim=c(1,1,1,1,1,5),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter=c('mean', 'median', 'var', 'uppq', 'lowq'))), units="NA"), n=1),
	# VALIDITY
    validity=function(object) {
    
    # iter dimensions is of length 5 and with names:
    if(dim(object)[6] != 5)
        return("dims of object do not match those of the FLQuantPoint class")

    # dimnames are 'mean', 'median', 'var', 'uppq', 'lowq'
    if(any(dimnames(object)$iter != c('mean', 'median', 'var', 'uppq', 'lowq')))
        return("dimnames of object do not match those of the FLQuantPoint class")

    #
    if(length(n) != 1)
      return("'n' must be of length 1")
    
	# Everything is fine
    return(TRUE)
	}
) # }}}

# FLQuantDistr    {{{

#' A class for samples of a probability distribution
#'
#' This extended FLQuant class holds both a measure of central tendendy (mean,
#' median) and of dispersion (tipically variance), to be later used to generate,
#' for example, random numbers with those mean and variances.
#'
#' @name FLQuantDistr
#' @docType class
#' @rdname FLQuantDistr
#' @aliases FLQuantDistr-class
#' @section Slots:
#'     \describe{
#'     \item{.Data}{Unnamed slot for storing the mean (or other measure of
#'       expectation) (\code{FLQuant}).}
#'     \item{var}{Variance, or other measure of dispersion, (\code{FLQuant}).}
#'     \item{distr}{Name of the probability distribution, see Details
#'       (\code{character}).}
#' }
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{slot dims}{.Data and var slots must have the same dimensions.}
#'     \item{slot dimnames}{.Data and var slots must have the same dimnames.}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLQuantDistr'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' The contents of the unnamed slot (.Data) can be accessed through the
#' \code{e()} method, see Example below.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity. If an unnamed \code{FLQuant} object is provided, this is used
#' for the .Data slot.
#' @param object Input numeric object
#' @param ... Additonal arguments
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{Arith}{.}
#' }
#' @section Arithmetic:
#' The methods under the \emph{Arith} group have been defined for objects of
#' this class, both for operations between two \code{FLQuantDistr} objects and
#' with objects of class \code{FLQuant} (\code{FLArray}) as follows:
#'
#' \describe{
#'     \item{`+`, FLQuantDistr,FLArray}{.}
#'     \item{`-`, FLQuantDistr,FLArray}{.}
#'     \item{`*`, FLQuantDistr,FLArray}{.}
#'     \item{`/`, FLQuantDistr,FLArray}{.}
#'     \item{`+`, FLQuantDistr,FLQuantDistr}{.}
#'     \item{`-`, FLQuantDistr,FLQuantDistr}{.}
#'     \item{`*`, FLQuantDistr,FLQuantDistr}{.}
#' }
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#' fqd <- FLQuantDistr(catch.n(ple4), var=catch.n(ple4) * 10, distr='norm')
#'
setClass("FLQuantDistr",
    representation("FLQuant", var="FLQuant", distr="character"),
	prototype(new("FLQuant"), var=new("FLQuant"), distr="norm"),
  
  # VALIDITY
	validity=function(object) {

	# .Data & var have same dims
	if(!all(dim(object@.Data) == dim(object@var)))
		return("object slots' dimensions must match")

	# .Data & var have same dimnames
	if(!all.equal(dimnames(object@.Data), dimnames(object@var)))
		return("object slots' dimnames must match")

	# Everything is fine
    return(TRUE)
}) # }}}

# FLQuantJK     {{{

#' A class for jackknifing fisheries data
#'
#' This extended FLQuant class holds both a jackknifed FLQuant, one in which each
#' iter is missing one element, and the original object, as a separate
#' \code{FLQuant} in the \code{orig} slot.
#'
#' @name FLQuantJK
#' @docType class
#' @aliases FLQuantJK-class
#'
#' @section Slots:
#'     \describe{
#'     \item{.Data}{Unnamed slot containing the jackknifed object(\linkS4class{FLQuant}).}
#'     \item{orig}{Original object, (\linkS4class{FLQuant}).}
#' }
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{slot dims}{.Data and orig slots must have the same dimensions 1-5.}
#'     \item{slot dimnames}{.Data and var slots must have the same dimnames 1-5.}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLQuantJK'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' Objects of this class must be constructed from an \linkS4class{FLQuant} that
#' is to be jackknifed, through the \code{\link{jackknife}} method.
#' @param object Input numeric object
#' @param ... Additonal arguments
#'
#' @section Methods:
#' All methods defined for the \linkS4class{FLQuant} class are available, but
#' they will operate only on the jackknifed (\code{.Data}) slot. Please use
#' \code{orig()} to apply them to the original object stored in the class.
#'
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#' fjk <- jackknife(stock(ple4))
#' # New object has as many iters as length of jackknifed dimension (defaults to 'year')
#' dim(fjk)
#'

setClass("FLQuantJK",
	representation("FLQuant", orig="FLQuant"),
  prototype(new("FLQuant"), orig=new("FLQuant")),
  validity=function(object) {

#    if(!all.equal(dimnames(object@.Data)[1:5], dimnames(object@orig)[1:5]))
#     return("Original and jackknifed object must share dimnames[1-5]")

    return(TRUE)
  }
) # }}}

# FLCohort {{{

#' Class FLCohort
#'
#' A class for modelling cohorts.
#'
#' This class represents cohorts in columns. It simply shifts the typical
#' matrix representation where cohorts are found on the diagonals, into a
#' matrix where cohorts are found in columns. It is very usefull for all
#' analysis that want to make use of cohorts instead of years.
#' 
#' @name FLCohort
#' @aliases FLCohort FLCohort-class FLCohort-methods
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
setClass("FLCohort",
	representation("FLArray", units="character"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(age="1", cohort="1", unit="unique", season="all", area="unique",
		iter="none")), units="NA"),
  validity=function(object) {
  # names
  if(!all.equal(names(dimnames(object)), 
      c("age", "cohort", "unit", "season", "area", "iter")))
    return("names of FLCohort object are not correct")

	# Everything is fine
  return(TRUE)
}

) # }}}

# FLPar {{{

#' Class FLPar
#' 
#' A class for storing parameters of a model.
#'
#' The \code{FLPar} class is based on the array class which can store
#' Monte Carlo samples and the names of the relevant parameter vectors.
#' 
#' Methods for this class include subsetting and replacement as for the
#' \code{\linkS4class{FLQuant}} class. There are methods for extracting
#' statistics of the sample (mean, median etc.) and for plotting the parameter
#' samples.
#'
#' @name FLPar
#' @aliases FLPar-class FLPar FLPar-methods FLPar,array-method
#' FLPar,missing-method FLPar,vector-method FLPar,FLPar-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{Describe slot. \code{array}.}
#' \item{units}{Units of measurement. \code{character}.} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame},
#' \link[lattice]{densityplot}, \link[lattice]{histogram}, \link{iter},
#' \link{iter<-}, \link[base]{mean}, \link[stats]{median},
#' \link[graphics]{plot}, \link[lattice]{splom}, \link[base]{summary},
#' \link{units,FLPar-method}, \link{units<-,FLPar,character-method},
#' \link[stats]{var}
#' @keywords classes
#' @examples
#' 
#' FLPar(rnorm(4), params=c('a','b','c','sigma2'))
#'
#' FLPar(rnorm(20), dimnames=list(params=c('a','b'), year=1990:1999, iter=1),
#'   units='NA')
#'
#' # with iters
#'   FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
#'

setClass('FLPar', representation('array', units='character'),
	prototype=prototype(array(as.numeric(NA), dim=c(1,1),
	dimnames=list(params=character(1), iter=1)), units='NA'),
	validity=function(object) {
		# object must be numeric
		if(!is.numeric(object))
			return('object must be numeric')
		# Last dimension is called 'iter' ...
	  if(names(dimnames(object))[length(dim(object))] != "iter")
  	  return("last dimension must be named 'iter'")
  	# ... and the first 'param'
	  # if(names(dimnames(object))[1] != "params")
  	#   return("first dimension must be named 'params'")

		return(TRUE)
	}
) # }}}

# FLParJK {{{

#' Class FLParJK
#' 
#' A class for storing parameters of a jackknifed model fit.
#'
#' @name FLParJK
#' @aliases FLParJK-class FLParJK
#' @docType class
#' @md
#' @slot .Data Jackknifed object, `FLPar`.
#' @slot units units of measurement, `character`.
#' @slot orig original object being jackknifed, `FLPar`.
#' @section Validity:
#' You can inspect the class validity function by using
#' `getValidity(getClassDef('FLParJK'))`
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#' @section Constructor:
#' Objects of this class are commonly created by calling the [jackknife()] method
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity.
#' @author The FLR Team
#' @seealso [`FLPar`]
#' @keywords classes

setClass("FLParJK",
	representation("FLPar", orig="FLPar"),
  prototype(new("FLPar"), orig=new("FLPar")),
	validity=function(object) {
    # dimnames of .Data and origin, but 'iter', must mjatch
    if(!all.equal(dimnames(object@.Data)[ - length(dim(object))],
      dimnames(object@orig)[ - length(dim(object))]))
      return("dimnames of .Data and origin must match")
		return(TRUE)
	}) # }}}
