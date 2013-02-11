# FLAccesors - «Short one line description»
# FLCore/R/FLAccesors.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$

## createFLAccesors		{{{
createFLAccesors <- function(class, exclude=character(1), include=missing) {
  
  object <- class

  if(!missing(include))
  	slots <- getSlots(class)[include]
  else
  	slots <- getSlots(class)[!names(getSlots(class))%in%exclude]

	defined <- list()

	for (x in names(slots)) {
		# check method is defined already and signatures match
  eval(
		substitute(if(!is.null(getGeneric(x)) && names(formals(x)) != "object") {
      warning(paste("Accesor method for", x, "conflicts with a differently defined 
      generic. Type", x, "for more information")); break}, list(x=x))
	  )
    # accessor
		eval(
		substitute(setMethod(x, signature(object=y),
      function(object) return(slot(object, x))),
      list(x=x, y=class))
		)
    # replacer
		eval(
		substitute(setReplaceMethod(x, signature(object=y, value=v),
      function(object, value)
			{slot(object, s) <- value; if(validObject(object)) object else
        stop("Object not valid")}),
      list(x=x, y=class, s=x, v=unname(slots[x])))
		)
    if(any(unname(slots[x]) %in% c('FLArray', 'FLQuant', 'FLCohort')))
    eval(
		substitute(setReplaceMethod(x, signature(object=y, value="numeric"),
      function(object, value)
			{slot(object, s)[] <- value; object}), list(x=x, y=object, s=x))
		)
    xr <- paste(x, "<-", sep="")
		defined[[x]] <- c(x, xr, paste('alias{',x,',', class,'-method}', sep=''),
			paste('\alias{',xr,',', class,',',unname(slots[x]), '-method}', sep=''),
			paste('\alias{',x,'-methods}', sep=''),
			paste('\alias{"',xr, '"-methods}', sep='')
		)
	}
	return(defined)
}	# }}}

## createFLeetAccesors  {{{
createFleetAccesors <- function(slot, fun, level=c(1:5), assigment=TRUE, class='FLQuant')
  {
	# replacement function
  if(assigment == TRUE)
  {
	# FLCatch
  if(1 %in% level) {
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatch', value=class),
		function(object, value) {
			slot(object, SLOT) <- value
			return(object)}), list(SLOT=slot)))
  eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatch', value='numeric'),
		function(object, value) {
			slot(object, SLOT)[] <- value
			return(object)}), list(SLOT=slot)))
  }
	# FLCatches
  if(2 %in% level) {
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatches', value=class),
		function(object, catch, value) {
			slot(object[[catch]], SLOT) <- value
			return(object)
		}),list(SLOT=slot)))
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLCatches', value='numeric'),
		function(object, catch, value) {
			slot(object[[catch]], SLOT)[] <- value
			return(object)
		}),list(SLOT=slot)))
  }
	# FLMetier
  if(3 %in% level) {
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetier', value=class),
		function(object, catch, value) {
			slot(object@catches[[catch]], SLOT) <- value
			return(object)
		}),list(SLOT=slot)))
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetier', value='numeric'),
		function(object, catch, value) {
			slot(object@catches[[catch]], SLOT)[] <- value
			return(object)
		}),list(SLOT=slot)))
  }
	# FLMetiers
  if(4 %in% level) {
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetiers', value=class),
		function(object, metier, catch, value) {
			slot(object[[metier]]@catches[[catch]], SLOT) <- value
			return(object)
		}), list(SLOT=slot)))
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLMetiers', value='numeric'),
		function(object, metier, catch, value) {
			slot(object[[metier]]@catches[[catch]], SLOT)[] <- value
			return(object)
		}), list(SLOT=slot)))
  }
	# FLFleet
  if(5 %in% level) {
	eval(substitute(setReplaceMethod(SLOT, signature(object='FLFleet', value=class),
		function(object, metier, catch, value) {
			slot(object@metiers[[metier]]@catches[[catch]], SLOT) <- value
			return(object)
		}), list(SLOT=slot)))
  eval(substitute(setReplaceMethod(SLOT, signature(object='FLFleet', value='numeric'),
		function(object, metier, catch, value) {
			slot(object@metiers[[metier]]@catches[[catch]], SLOT)[] <- value
			return(object)
		}), list(SLOT=slot)))
  }
  }

	# accesor functions
	# FLCatch
  if(1 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLCatch'),
		function(object)
			return(slot(object, SLOT))), list(SLOT=slot)))
	# FLCatches
  if(2 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLCatches'),
		function(object, catch='missing') {
			if(missing(catch))
				return(lapply(object, SLOT))
			else
				return(FUN(object[[catch]]))}),list(SLOT=slot, FUN=fun)))
	# FLMetier
  if(3 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLMetier'),
		function(object, ...)
				return(FUN(object@catches, ...))), list(SLOT=slot, FUN=fun)))
	# FLMetiers
  if(4 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLMetiers'),
		function(object, metier='missing', catch='missing', ...) {
      # nothing
			if (missing(metier) && missing(catch))
				stop('Either metier or catch must be specified')
      # metier
			else if(!missing(metier) && missing(catch))
				return(FUN(object[[metier]], ...))
      # catch
			else if(missing(metier) && !missing(catch))
      {
				res <- FLQuants()
				for(i in names(object))
        {
          if (catch %in% names(object[[i]]@catches))
  					res[[i]] <- FUN(object[[i]], catch=catch, ...)
        }
				return(res)
      # both
			} else
				return(FUN(object[[metier]], catch=catch, ...))}), list(SLOT=slot, FUN=fun)))
	# FLFleet
  if(5 %in% level)
	eval(substitute(setMethod(SLOT, signature(object='FLFleet'),
		function(object, ...)
				return(FUN(object@metiers, ...))), list(SLOT=slot, FUN=fun)))
}   # }}}

# getSlotNamesClass {{{
getSlotNamesClass <- function(object, class)
{
    slots <- names(getClass(class(object))@slots)
    contains <- as.list(rep(FALSE, length(slots)))
    names(contains) <- slots
    for(what in slots)
      if(is(slot(object, what), class))
        contains[[what]] <- TRUE
    return(names(contains[contains == TRUE]))
} # }}}
