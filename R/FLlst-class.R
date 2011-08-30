# FLlst-class.R - 
# FLCore/R/FLlst-class.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$

# getPlural {{{
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
    'FLSR'='FLSRs',
    'list'
    )
} # }}}

# FLlst() {{{
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
}) # }}}

# coerce {{{
setAs("FLlst", "list", function(from){
	lst <- from@.Data
	names(lst) <- from@names
	attr(lst, "desc") <- from@desc # check when it's empty insert something
	lst
}) # }}}
