# .R - 
# /R/.R

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

	# check "units" slot
	if(!is.character(object@units))
		return("units must be a string")
	# Everything is fine
	return(TRUE)
}

setClass("FLArray",	representation("array", units="character"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1)), units="NA"),
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

	# check "units" slot
	if(!is.character(object@units))
		return("units must be a string")

	# Everything is fine
	return(TRUE)
}

setClass("FLQuant",
	representation("FLArray"),
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

