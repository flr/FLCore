# FLS.R - Parent FLStock* class methods
# FLS.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Notes:

# computeLandings	{{{
setMethod("computeLandings", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res        <- quantSums(landings.n(object) * landings.wt(object), na.rm=na.rm)
        units(res) <- paste(units(landings.n(object)), "*",  units(landings.wt(object)))
        return(res)
 	} 
)	# }}}

# computeDiscards	{{{
setMethod("computeDiscards", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        units(res) <- paste(units(discards.n(object)), units(discards.wt(object)))
        return(res)
 	} 
)	# }}}

# computeCatch	{{{
setMethod("computeCatch", signature(object="FLS"),
  function(object, slot="catch", na.rm=TRUE) {
    if(slot == "n"){
		# NA to 0
      res <- landings.n(object) + discards.n(object)
      if (units(discards.n(object)) == units(landings.n(object)))
			  units(res) <- units(discards.n(object))
    }
    else if(slot == "wt") {
      res <- (landings.wt(object) * landings.n(object) +
        discards.wt(object) * discards.n(object)) /
        (landings.n(object) + discards.n(object))
		  if (units(discards.wt(object)) == units(landings.wt(object)))
		    units(res) <- units(discards.wt(object))
    }
		else if (slot == "all") {
      ctch.n     <-computeCatch(object, slot="n")
      ctch.wt    <-computeCatch(object, slot="wt")
			ctch       <-quantSums(ctch.n*ctch.wt, na.rm=na.rm)
      units(ctch)<-paste(units(ctch.n), units(ctch.wt))

      res <- FLQuants(catch.wt=ctch.wt,
			  catch.n =ctch.n,
        catch   =ctch)
		}
    else {
		  res <- quantSums(catch.n(object) * catch.wt(object), na.rm=na.rm)
        units(res) <- paste(units(catch.n(object)), units(catch.wt(object)))
    }
		return(res)
    }
)	# }}}

# computeStock	{{{
setMethod("computeStock", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res <- quantSums(stock.n(object) * stock.wt(object), na.rm=na.rm)
        units(res) <- paste(units(stock.n(object)), "*",  units(stock.wt(object)))
        return(res)
 	} 
)	# }}}

# harvest		{{{
setMethod("harvest", signature(object="FLS", catch="missing"),
	function(object, index="f") {
		if (!missing(index) && units(slot(object, "harvest")) != index)
			stop("The units of harvest in the object do not match the specified index")
		return(slot(object, "harvest"))
	}
)

# harvest<-
setMethod("harvest<-", signature(object="FLS", value="character"),
	function(object, value) {
		units(slot(object, "harvest")) <- value
		return(object)
	}
)
setMethod("harvest<-", signature(object="FLS", value="FLQuant"),
	function(object, value) {
		slot(object, "harvest") <- value
    if(validObject(object))
      return(object)
    else
      stop("Object not valid")
	}
)
setMethod("harvest<-", signature(object="FLS", value="numeric"),
	function(object, value) {
		slot(object, "harvest")[] <- value
		return(object)
	}
) # }}}

# z {{{
setMethod("z", "FLS", function(object, ...) {
  f <- harvest(object)
  if(units(f) != 'f') {
    stop("Your exploitation rate is not defined as F, cannot be added to M")
  } else { 
    return(m(object) + f)
  }
}) # }}}

# trim {{{

#' @rdname trim
#' @aliases trim,FLS-method
#' @aliases trim,FLStock-method
#' @aliases trim,FLStockLen-method

setMethod("trim", signature(x="FLS"), function(x, ...){

	args <- list(...)

  rng<-range(x)
  
  c1 <- args[[quant(landings.n(x))]]
	c2 <- args[["year"]]
	c3 <- args[["unit"]]
	c4 <- args[["season"]]
	c5 <- args[["area"]]
	c6 <- args[["iter"]]

  # FLQuants with quant
	names <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])

    for (name in names) {
        if(name %in% c('stock', 'catch', 'landings', 'discards'))
            slot(x,name) <- trim(slot(x,name), year=c2, unit=c3, season=c4,
                area=c5, iter=c6)
        else
            slot(x,name) <- trim(slot(x,name), ...)
    }
            
  if (length(c1) > 0) {
    x@range["min"] <- c1[1]
		x@range["minfbar"] <- max(rng["minfbar"], c1[1])
    x@range["max"] <- c1[length(c1)]
		x@range["maxfbar"] <- min(rng["maxfbar"], c1[length(c1)])
    if (rng["max"] != x@range["max"])
        x@range["plusgroup"] <- NA
  }
  if (length(c2)>0 ) {
    x@range["minyear"] <- c2[1]
    x@range["maxyear"] <- c2[length(c2)]
  }

	return(x)}) # }}}
