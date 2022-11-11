# FLIndexBiomass.R - DESC
# FLIndexBiomass.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03
# Soundtrack:
# Notes:

## Accesors {{{
invisible(createFLAccesors("FLIndexBiomass", exclude=c('name', 'desc', 'range', 'effort'))) # }}}

# FLIndexBiomass()   {{{

#' @rdname FLIndexBiomass
#' @aliases FLIndexBiomass,FLQuant-method
setMethod('FLIndexBiomass', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
    qobject <- quantSums(object)

    dims <- dims(object)

    res <- new("FLIndexBiomass",
      index=qobject, index.var=qobject, catch.n=object, catch.wt=object,
      effort=qobject, sel.pattern=object, index.q=qobject,
      range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear, startf=as.numeric(NA),
      endf=as.numeric(NA))))
      

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
  }
)

#' @rdname FLIndexBiomass
#' @aliases FLIndexBiomass,missing-method
setMethod('FLIndexBiomass', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant(dimnames=list(age='all'))
    else
    {
      qslots <- slots[!slots %in% c('index', 'index.q', 'index.var', 'effort')]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots]]
    }
    return(FLIndexBiomass(object, ...))
  }
) # }}}

# biomass {{{
setMethod("biomass", signature(x="FLIndexBiomass"),
  function(x) {
    quantSums(index(x))
  }
)
# }}}
