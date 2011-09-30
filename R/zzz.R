# zzz.R
# FLCore/R/zzz.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$

.onLoad <- function(lib,pkg) {
	require(methods)
  cat("FLCore 2.4 \"The Duke of Prawns\"\n")
  cat("------------------------------------\n")
}

## convert6d  {{{
convert6d<-function(obj)
    {
    if (is(obj, "FLQuant"))
        return(FLQuant(obj@.Data, dimnames = dimnames(obj@.Data),
            units = units(obj)))

    if (is(obj, "FLQuants"))
        return(lapply(obj, convert6d))

    slots <- getSlots(class(obj))
    slots <- names(slots[slots == "FLQuant"])
    for (i in slots){
       print(i)
       slot(obj, i) <- FLQuant(slot(obj, i)@.Data, dimnames = dimnames(slot(obj, i)@.Data), units = attributes(slot(obj,i))$units)
       }

    if (is(obj, "FLFleet"))
        for (i in names(obj@metiers)) for (j in names(obj@metiers[[i]]@catches)) obj@metiers[[i]]@catches[[j]] <- qapply(obj@metiers[[i]]@catches[[j]],
            convert6d)
    return(obj)
    }
# }}}

# convertFLPar{{{
	setGeneric("convertFLPar", function(object, ...)
		standardGeneric("convertFLPar"))
setMethod('convertFLPar', signature(object='FLModel'),
  function(object)
  {
    params(object) <- convertFLPar(params(object))
    return(object)
  }
)
setMethod('convertFLPar', signature(object='FLPar'),
  function(object)
  {
    dimn <- dimnames(object)
    if(all(names(dimn) == c('iter', 'params')))
      object@.Data <- aperm(object@.Data, c(2,1))
    else if (length(dimn) > 2)
    {
      iter <- grep('iter', names(dimn))
      params <- grep('params', names(dimn))
      idx <- c(params, 3:length(dimn), iter)
      object@.Data <- aperm(object@.Data, idx)
    }
    if(validObject(object))
      return(object)
    else
      stop('FLPar object is still invalid')
  }
)
# }}}

# ac
ac <- function(x, ...)
  as.character(x, ...)

# convertFLModel{{{
	setGeneric("convertFLModel", function(object, ...)
		standardGeneric("convertFLModel"))
setMethod('convertFLModel', signature(object='FLModel'),
  function(object) {

    res <- new(class(object))

    idx <- slotNames(object)
    idx <- idx[idx != "distribution"]
    
    for (i in idx) {
      slot(res, i) <- slot(object, i)
    }

    if(validObject(res))
      return(res)
    else
      stop('FLModel object is still invalid')
  }
)
# }}}

