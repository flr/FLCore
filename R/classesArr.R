# classesArr.R - 
# FLCore/R/classesArr.R - 

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# FLArray {{{
validFLArray  <-  function(object){
	# Make sure there are at least 6 dimensions in the array
	Dim  <-  dim(object)
	if (length(Dim) != 6) 
		return("the array must have 6 dimensions")

	if (!is.numeric(object) && !is.na(object)) 
		return("array is not numeric")

	# Everything is fine
	return(TRUE)
}

setClass("FLArray",	representation("array"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1))),
	validity=validFLArray
) # }}}

# FLQuant     {{{
validFLQuant  <-  function(object){
	
	# Make sure there are at least 6 dimensions in the array named
	# *, "year", "unit", "season", "area" and "iter"
	DimNames  <-  names(dimnames(object))
  if (length(DimNames) != 6)
    return("the array must have 6 dimensions")
  if (!all(DimNames[2:6] == c("year", "unit", "season", "area", "iter")))
    return("dimension names of the array are incorrect")
	if (!is.numeric(object) && !is.na(object))
		return("array is not numeric")

	# Everything is fine
	return(TRUE)
}

# 2 {{{
#' FLQuant class for numerical data
#' 
#' The \code{FLQuant} class is a six-dimensional \code{\link[base]{array}}
#' designed to store most quantitative data used in fisheries and population
#' modelling.
#' 
#' The sixth dimensions are named. The name of the first dimension can be
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
#' 
#' @name FLQuant
#' @aliases FLQuant-class FLQuant FLQuant-methods FLQuant,missing-method
#' FLQuant,vector-method FLQuant,array-method FLQuant,matrix-method
#' FLQuant,FLQuant-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{A 6-D array for numeric data.
#' \code{array}.} \item{units}{Units of measurement. \code{character}.} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{apply},
#' \link[methods]{Arith}, \link[base]{as.data.frame}, \link{as.FLQuant},
#' \link[lattice]{barchart}, \link{bubbles}, \link[lattice]{bwplot},
#' \link{catch<-}, \link{catch.n<-}, \link{catch.wt<-}, \link[methods]{coerce},
#' \link{cv}, \link{dimnames<-}, \link{dims}, \link{discards<-},
#' \link{discards.n<-}, \link{discards.wt<-}, \link[lattice]{dotplot},
#' \link{E}, \link{fec<-}, \link{FLCatch}, \link{FLCohort}, \link{FLMetier},
#' \link{FLQuant}, \link{FLQuantPoint}, \link{FLSR}, \link{harvest<-},
#' \link[lattice]{histogram}, \link{iter}, \link{iter<-}, \link{iters},
#' \link{landings<-}, \link{landings.n<-}, \link{landings.wt<-}, \link{m<-},
#' \link{n<-}, \link[base]{names}, \link[graphics]{plot}, \link{price<-},
#' \link[base]{print}, \link{propagate}, \link{quant}, \link{quant<-},
#' \link{quantile}, \link[stats]{rlnorm}, \link[stats]{rnorm},
#' \link[methods]{show}, \link{spwn<-}, \link[lattice]{stripplot},
#' \link[base]{summary}, \link{trim}, \link[base]{units}, \link[base]{units<-},
#' \link[stats]{window}, \link{wt<-}, \link[lattice]{xyplot},
#' \link[base]{array}
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
#' xyplot(data ~ year, data=flq, type='b', main='FLQ Test Plot', groups=age,
#'   ylab='diff', xlab='', pch=19, auto.key=TRUE)
#' 
# 2 }}}

setClass("FLQuant",
	representation("FLArray", units="character"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter="1")), units="NA"),
	validity=validFLQuant
)

remove(validFLQuant)    # }}}

# FLQuantPoint    {{{
validFLQuantPoint <- function(object) {
    
    # iter dimensions is of length 5 and with names:
    if(dim(object)[6] != 5)
        return("dims of object do not match those of the FLQuantPoint class")

    # dimnames are 'mean', 'median', 'var', 'uppq', 'lowq'
    if(any(dimnames(object)$iter != c('mean', 'median', 'var', 'uppq', 'lowq')))
        return("dimnames of object do not match those of the FLQuantPoint class")
    
	# Everything is fine
    return(TRUE)
}
setClass("FLQuantPoint",
    representation("FLQuant"),
	prototype(new('FLQuant', array(as.numeric(NA), dim=c(1,1,1,1,1,5),
		dimnames=list(quant="all", year="1", unit="unique", season="all",
		area="unique", iter=c('mean', 'median', 'var', 'uppq', 'lowq'))), units="NA")),
    validity=validFLQuantPoint
)

setValidity("FLQuantPoint", validFLQuantPoint)
remove(validFLQuantPoint)   # }}}

# FLQuantVar    {{{
validFLQuantVar <- function(object) {

	# Everything is fine
    return(TRUE)
}
setClass("FLQuantVar",
    representation("FLQuant", var="FLArray", distr="character"),
	prototype(new("FLQuant"), var=new("FLArray"), distr="lnorm"))

#setValidity("FLQuantVar", validFLQuantVar)
remove(validFLQuantVar)   # }}}

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

