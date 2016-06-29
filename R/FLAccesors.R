# FLAccesors - Accesor-generating functions
# FLCore/R/FLAccesors.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

# createFLAccesors {{{
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
}  # }}}

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
