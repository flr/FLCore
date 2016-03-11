# FLIndex.R - FLIndex class and methods
# FLCore/R/FLIndex.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Richard Hillary, Imperial College London
# $Id: FLIndex.R 1778 2012-11-23 08:43:57Z imosqueira $

# FLIndex()   {{{
setMethod('FLIndex', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
		units(object) <- 'NA'
    qobject <- quantSums(object)

    dims <- dims(object)

    res <- new("FLIndex",
      index=object, index.var=object, catch.n=object, catch.wt=object,
      effort=qobject, sel.pattern=object, index.q=object,
      range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear, startf=as.numeric(NA),
      endf=as.numeric(NA))))
      

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
  }
)

setMethod('FLIndex', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant()
    else
    {
      qslots <- slots[!slots %in% c('effort')]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots]]
    }
    return(FLIndex(object, ...))
  }
) # }}}

# is.FLIndex	{{{
is.FLIndex <- function(x)
    return(inherits(x, "FLIndex"))
# }}}
