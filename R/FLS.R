# FLS.R - DESC
# FLS.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

## computeLandings	{{{
setMethod("computeLandings", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res        <- quantSums(landings.n(object) * landings.wt(object), na.rm=na.rm)
        units(res) <- paste(units(landings.n(object)), "*",  units(landings.wt(object)))
        return(res)
 	} 
)	# }}}

## computeDiscards	{{{
setMethod("computeDiscards", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        units(res) <- paste(units(discards.n(object)), units(discards.wt(object)))
        return(res)
 	} 
)	# }}}

## computeCatch	{{{
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

## computeStock	{{{
setMethod("computeStock", signature(object="FLS"),
	function(object, na.rm=TRUE) {
        res <- quantSums(stock.n(object) * stock.wt(object), na.rm=na.rm)
        units(res) <- paste(units(stock.n(object)), "*",  units(stock.wt(object)))
        return(res)
 	} 
)	# }}}

