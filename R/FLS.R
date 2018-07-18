# FLS.R - Parent FLStock* class methods
# FLS.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Notes:

# computeLandings  {{{
#' @rdname compute
#' @aliases computeLandings,FLS-method computeLandings,FLStock-method
#' computeLandings,FLStockLen-method
setMethod("computeLandings", signature(object="FLS"),
  function(object, na.rm=TRUE) {
        res        <- quantSums(landings.n(object) * landings.wt(object), na.rm=na.rm)
        return(res)
   } 
)  # }}}

# computeDiscards  {{{
#' @rdname compute
#' @aliases computeDiscards,FLS-method computeDiscards,FLStock-method
setMethod("computeDiscards", signature(object="FLS"),
  function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        return(res)
   } 
)  # }}}

# computeCatch  {{{
#' @rdname compute
#' @aliases computeCatch,FLS-method computeCatch,FLStock-method
#' computeCatch,FLStockLen-method
setMethod("computeCatch", signature(object="FLS"),
  function(object, slot="catch", na.rm=TRUE) {
    if(slot == "n"){
    # NA to 0
      res <- landings.n(object) + discards.n(object)
    }
    else if(slot == "wt") {
    # 0 to 1e-16
      res <- (landings.wt(object) * (landings.n(object) + 1e-16) +
        discards.wt(object) * (discards.n(object) + 1e-16)) /
        ((landings.n(object) + discards.n(object)) + 1e-16)
    }
    else if (slot == "all") {
      ctch.n     <-computeCatch(object, slot="n")
      ctch.wt    <-computeCatch(object, slot="wt")
      ctch       <-quantSums(ctch.n * ctch.wt, na.rm=na.rm)

      res <- FLQuants(catch.wt=ctch.wt,
        catch.n =ctch.n,
        catch   =ctch)
    }
    else {
      res <- quantSums(catch.n(object) * catch.wt(object), na.rm=na.rm)
    }
    return(res)
    }
)  # }}}

# computeStock  {{{
#' @rdname compute
#' @aliases computeStock,FLS-method computeStock,FLStock-method
#' computeStock,FLStockLen-method
setMethod("computeStock", signature(object="FLS"),
  function(object, na.rm=TRUE) {
        res <- quantSums(stock.n(object) * stock.wt(object), na.rm=na.rm)
        return(res)
   } 
)  # }}}

# harvest    {{{
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

#' @rdname z-methods
#' @aliases z,FLS-method
#' @examples
#' data(ple4)
#'
#' z(ple4)

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
#' @examples
#' 
#' # Now on an FLStock
#' data(ple4)
#' summary(trim(ple4, year=1990:1995))
#' 
#' # If 'age' is trimmed in ple4, catch, landings and discards need to be
#' # recalculated
#'   shpl4 <- trim(ple4, age=1:4)
#'   landings(shpl4) <- computeLandings(shpl4)
#'   discards(shpl4) <- computeDiscards(shpl4)
#'   catch(shpl4) <- computeCatch(shpl4)
#'   summary(shpl4)
#'
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
    x@range["min"] <- as.numeric(c1[1])
    x@range["minfbar"] <- max(rng["minfbar"], as.numeric(c1[1]))
    x@range["max"] <- as.numeric(c1[length(c1)])
    x@range["maxfbar"] <- min(rng["maxfbar"], as.numeric(c1[length(c1)]))
    if (rng["max"] != x@range["max"])
        x@range["plusgroup"] <- NA
  }
  if (length(c2)>0 ) {
    x@range["minyear"] <- as.numeric(c2[1])
    x@range["maxyear"] <- as.numeric(c2[length(c2)])
  }

  return(x)}) # }}}

# metrics {{{

#' @rdname metrics

setMethod("metrics", signature(object="FLS", metrics="missing"),
  function(object, ...) {
    
    dots <- list(...)
    # HACK for some method dispatch problem
    foo <- selectMethod("metrics", c(object="FLS", metrics="list"))

    if(length(dots) > 0) {
      return(foo(object=object, metrics=dots))
    }
    else {
      if(tolower(units(harvest(object))) == "f") {
        return(foo(object=object,
          metrics=list(Rec=rec, SSB=ssb, Catch=catch, F=fbar)))
      } else if(tolower(units(harvest(object))) == "hr") {
        return(foo(object=object,
          metrics=list(Rec=rec, SSB=ssb, Catch=catch, HR=fbar)))
      } else {
        return(foo(object=object,
          metrics=list(Rec=rec, SSB=ssb, Catch=catch)))
      }
    }
  }
) # }}}

# catch<- FLQuants		{{{

#' @rdname accessors
#' @aliases catch<-,FLS,FLQuants-method
#' @md
#' @details An object of class FLQuants, containing three elements named
#' *catch*, *catch.n* and *catch.wt*, as returned by [`computeCatch`], can be
#' assigned directly to an object using *catch<-*.
#' @examples
#' # Assign the 3 catch slots
#' catch(ple4) <- computeCatch(ple4, slot="all")
setMethod("catch<-", signature(object="FLS", value="FLQuants"),
	function(object, value) {
		catch(object)    <- value[['catch']]
		catch.n(object)  <- value[['catch.n']]
		catch.wt(object) <- value[['catch.wt']]
		return(object)
	}
) # }}}
