#=====================================================================
#
#! Class: FLlst & FL*s
#! Date: 13/04/2007
#! Version: 0.1-0
#
# \name{ myclass-class }
# \docType{class}
# \alias{ myclass-class }
# \title{  - class}
# \description{
#	Represents this using that. 
# }
# \section{Slots}{
#	\describe{
#		\item{\code{slot1}:}{Object of class \code{"???"} ...}
#		\item{\code{slot2}:}{Object of class \code{"???"} ...}
# 	}
# }
# \section{Methods}{
# Type \code{showMethods(classes="myclass")} at the R prompt for a complete list of methods which are available for this class.
# Useful methods include:
#	\describe{
#		\item{\code{mymeth1}:}{ blahblahblah }
#		\item{\code{mymeth2}:}{ blahblahblah }
#	}
# }
# \seealso{
#	\code{\link{ myclass2-class}}
# }
# \section{Creating Objects from the Class}{
#	new("myclass")
# }
# \note{
#	If relevant ...	
# }
# \author{
#	me 
#	\email{me@myself.org}
#	\url{www.myself.org}
# }
# \keyword{classes}
# \keyword{classes}
#
#! ToDo:
#
#! References (bibtex):
#
#! Notes:
#
#=====================================================================

#! FLlst

# validity
vFLl <- function(object){

	# Make sure the list contains all items of the same class
	cls <- unlist(lapply(object, class))
  if(any(cls != cls[1]))
	  stop("Components must be of the same class!")	

  # All elements in the list are validObjects themselves
  if(!all(unlist(lapply(object, validObject))))
	  stop("Components must be valid objects themselves (validObject == TRUE)")	

	# Everything is fine
	return(TRUE)
}

# class
setClass("FLlst",contains="list",representation(
	names="character",
	desc="character",
	lock="logical"),
	prototype(lock=FALSE),
	validity=vFLl
)

# getPlural
getPlural <- function(object)
{
  switch(class(object),
    'FLQuant'='FLQuants',
    'FLCohort'='FLCohorts',
    'FLCatch'='FLCatches',
    'FLMetier'='FLMetiers',
    'FLStock'='FLStocks',
    'FLIndex'='FLIndices',
    'FLBiol'='FLBiols',
    'FLFleet'='FLFleets',
    'list'
    )
}

# constructor
setMethod("FLlst", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLlst", lst)
})

setMethod("FLlst", signature(object="missing"), function(...){
	if(missing(...)){
		new("FLlst")
	} else { 
		lst <- list(...)
		new("FLlst", lst)
	}
})

setMethod("FLlst", "list", function(object){
	new("FLlst", object)
})

# coerce to list
setAs("FLlst", "list", function(from){
	lst <- from@.Data
	names(lst) <- from@names
	attr(lst, "desc") <- from@desc # check when it's empty insert something
	lst
})

#! FLStocks

# validity
vFLSs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLStock")) stop("Components must be FLStock")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLStocks", contains="FLlst",
	validity=vFLSs
)

# constructor
setMethod("FLStocks", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLStocks", lst)
})

setMethod("FLStocks", "missing", function(...){
	if(missing(...)){
		new("FLStocks")
	} else { 
		lst <- list(...)
		new("FLStocks", lst)
	}
})

setMethod("FLStocks", "list", function(object){
	new("FLStocks", object)
})

# is
setGeneric("is.FLStocks", function(object)
	standardGeneric("is.FLStocks"))

setMethod("is.FLStocks", "ANY", function(object){
	identical(is(object)[1],"FLStocks")
})

#! FLIndices

# validity
vFLIs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLIndex")) stop("Components must be FLIndex")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLIndices", contains="FLlst",
	validity=vFLIs
)

# constructor
setGeneric("FLIndices", function(object, ...){
	standardGeneric("FLIndices")
	}
)

setMethod("FLIndices", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLIndices", lst)
})

setMethod("FLIndices", "missing", function(...){
	if(missing(...)){
		new("FLIndices")
	} else { 
		lst <- list(...)
		new("FLIndices", lst)
	}
})

setMethod("FLIndices", "list", function(object){
	new("FLIndices", object)
})

# is
setGeneric("is.FLIndices", function(object, ...){
	standardGeneric("is.FLIndices")
	}
)

setMethod("is.FLIndices", "ANY", function(object, ...){
	identical(is(object)[1],"FLIndices")
})

#! FLBiols

# validity
vFLBs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLBiol")) stop("Components must be FLBiol")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLBiols", contains="FLlst",
	validity=vFLBs
)

# constructor
setGeneric("FLBiols", function(object, ...){
	standardGeneric("FLBiols")
	}
)

setMethod("FLBiols", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLBiols", lst)
})

setMethod("FLBiols", "missing", function(...){
	if(missing(...)){
		new("FLBiols")
	} else { 
		lst <- list(...)
		new("FLBiols", lst)
	}
})

setMethod("FLBiols", "list", function(object){
	new("FLBiols", object)
})

# is
setGeneric("is.FLBiols", function(object, ...){
	standardGeneric("is.FLBiols")
	}
)

setMethod("is.FLBiols", "ANY", function(object, ...){
	identical(is(object)[1],"FLBiols")
})

#! FLCatches

# validity
vFLCs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCatch")) stop("Components must be FLCatch")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLCatches", contains="FLlst", 
	validity=vFLCs
)

# constructor
setGeneric("FLCatches", function(object, ...){
	standardGeneric("FLCatches")
	}
)

setMethod("FLCatches", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)

	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1

	# IM 20.08.07 get names
	names <- c(object@name, unlist(lapply(lst1, function(x) x@name)))
	attr(lst, "names") <- names
	attr(lst, "lock") <- TRUE
	new("FLCatches", lst)
})

setMethod("FLCatches", "missing", function(...){
	if(missing(...)){
		new("FLCatches")
	} else { 
		lst <- list(...)
		new("FLCatches", lst)
	}
})

setMethod("FLCatches", "list", function(object){
	new("FLCatches", object)
})

# is
setGeneric("is.FLCatches", function(object, ...){
	standardGeneric("is.FLCatches")
	}
)

setMethod("is.FLCatches", "ANY", function(object, ...){
	identical(is(object)[1],"FLCatches")
})

#! FLMetiers

# validity
vFLMs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLMetier")) stop("Components must be FLMetier")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLMetiers", contains="FLlst", 
	validity=vFLMs
)

# constructor
setGeneric("FLMetiers", function(object, ...){
	standardGeneric("FLMetiers")
	}
)

setMethod("FLMetiers", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1

  if(is.null(names(lst1)))
    names(lst1) <- rep("", nlst)
  names(lst) <- c(object@gear, names(lst1))
  if(any(names(lst) == ""))
  names(lst)[names(lst)==""] <- unlist(lapply(lst[names(lst)==""], function(x) x@gear))

	attr(lst, "lock") <- TRUE
	new("FLMetiers", lst)
})

setMethod("FLMetiers", "missing", function(...){
	if(missing(...)){
		new("FLMetiers")
	} else { 
		lst <- list(...)
    if(any(names(lst) == ""))
      names(lst)[names(lst)==""] <- unlist(lapply(lst[names(lst)==""], function(x) x@gear))
		new("FLMetiers", lst)
	}
})

setMethod("FLMetiers", "list", function(object){
	new("FLMetiers", object)
})

# is
setGeneric("is.FLMetiers", function(object, ...){
	standardGeneric("is.FLMetiers")
	}
)

setMethod("is.FLMetiers", "ANY", function(object, ...){
	identical(is(object)[1],"FLMetiers")
})

#! FLFleets

# validity
vFLFs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLFleet")) stop("Components must be FLFleet")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLFleets", contains="FLlst",
	validity=vFLFs
)

# constructor
setGeneric("FLFleets", function(object, ...){
	standardGeneric("FLFleets")
	}
)

setMethod("FLFleets", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLFleets", lst)
})

setMethod("FLFleets", "missing", function(...){
	if(missing(...)){
		new("FLFleets")
	} else { 
		lst <- list(...)
		new("FLFleets", lst)
	}
})

setMethod("FLFleets", "list", function(object){
	new("FLFleets", object)
})

# is
setGeneric("is.FLFleets", function(object, ...){
	standardGeneric("is.FLFleets")
	}
)

setMethod("is.FLFleets", "ANY", function(object, ...){
	identical(is(object)[1],"FLFleets")
})

#! FLQuants

# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLQuant")) stop("Components must be FLQuant")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLQuants", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLQuants", function(object, ...){
	standardGeneric("FLQuants")
	}
)

setMethod("FLQuants", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLQuants", lst)
})

setMethod("FLQuants", "missing", function(...){
	if(missing(...)){
		new("FLQuants")
	} else { 
		lst <- list(...)
		new("FLQuants", lst)
	}
})

setMethod("FLQuants", "list", function(object){
	new("FLQuants", object)
})

setMethod("FLQuants", "FLQuants", function(object){
	return(object)
})

# is
setGeneric("is.FLQuants", function(object, ...){
	standardGeneric("is.FLQuants")
	}
)

setMethod("is.FLQuants", "ANY", function(object, ...){
	identical(is(object)[1],"FLQuants")
})

#! FLCohorts

# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCohort")) stop("Components must be FLCohort")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLCohorts", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLCohorts", function(object, ...){
	standardGeneric("FLCohorts")
	}
)

setMethod("FLCohorts", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLCohorts", lst)
})

setMethod("FLCohorts", "missing", function(...){
	if(missing(...)){
		new("FLCohorts")
	} else { 
		lst <- list(...)
		new("FLCohorts", lst)
	}
})

setMethod("FLCohorts", "list", function(object){
	new("FLCohorts", object)
})

setMethod("FLCohorts", "FLCohorts", function(object){
	return(object)
})
