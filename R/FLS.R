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
    res <- quantSums(landings.n(object) * landings.wt(object), na.rm=na.rm)
    return(res)
   } 
)  # }}}

# computeDiscards  {{{
#' @rdname compute
#' @aliases computeDiscards,FLS-method computeDiscards,FLStock-method
setMethod("computeDiscards", signature(object="FLS"),
  function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object) * discards.wt(object), na.rm=na.rm)
        return(res)
   } 
)  # }}}

# computeCatch  {{{
#' @rdname compute
#' @aliases computeCatch,FLS-method computeCatch,FLStock-method
#' computeCatch,FLStockLen-method
setMethod("computeCatch", signature(object="FLS"),
  function(object, slot="catch", na.rm=TRUE) {

    # PROPAGATE in case iters differ among slots
    object <- propagate(object, dims(object)$iter)

    if(slot == "n"){
    # NA to 0
      if(na.rm)
        landings.n(object)[is.na(landings.n(object))] <- 0
        discards.n(object)[is.na(discards.n(object))] <- 0
      res <- landings.n(object) + discards.n(object)
    }
    else if(slot == "wt") {
    # 0 to 1e-16
      if(na.rm)
        landings.wt(object)[is.na(landings.n(object))] <- 0
        landings.n(object)[is.na(landings.n(object))] <- 1
        discards.wt(object)[is.na(discards.n(object))] <- 0
        discards.n(object)[is.na(discards.n(object))] <- 1

        res <- ((landings.wt(object) * (landings.n(object) + 1e-16)) +
          (discards.wt(object) * (discards.n(object) + 1e-16))) / 
          (landings.n(object) + discards.n(object) + 1e-16)
    }
    else if (slot == "all") {
      land <- computeLandings(object)
      disc <- computeDiscards(object)
      ctch.n     <-computeCatch(object, slot="n")
      ctch.wt    <-computeCatch(object, slot="wt")
      ctch       <-quantSums(ctch.n * ctch.wt, na.rm=na.rm)

      res <- FLQuants(catch.wt=ctch.wt, catch.n =ctch.n, catch=ctch,
        landings=land, discards=disc)
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

    for(i in names(value))
      slot(object, i) <- value[[i]]

		return(object)
	}
) # }}}

# standardUnits {{{

#' @rdname standardUnits-methods
#' @details For objects derived from class *FLS*, which currently includes
#' *FLStock* and *FLStockLen*, the adopted standard includes: 'kg' for individual
#' weights, '1000' for number of individuals, 't' for biomass, 'f' for harvest,
#' 'm' for natural mortality, and an empty string for proportions (spwn, mat).
#' @examples
#' stk <- FLStock(catch=FLQuant(runif(20, 2, 120)))
#' # FLStock object has no units
#' summary(stk)
#' # Obtain standard units for the class as a list
#' standardUnits(stk)
#' # which can then be assigned to the object
#' units(stk) <- standardUnits(stk)
#' summary(stk)
#' # units<- methjod also accepts a function to be called to provide units
#' units(stk) <- standardUnits

setMethod("standardUnits", signature(object="FLS"),
  function(object, ...) {

    standard <- list(biomass="t", numbers="1000", weights="kg",
      proportions="", m="m", harvest="f")

    args <- list(...)
    standard[names(args)] <- args

    units <- c(

    # weights
    setNames(rep(standard$weight, 4),
      c("catch.wt", "landings.wt", "discards.wt", "stock.wt")),

    # biomass
    setNames(rep(standard$biomass, 4),
      c("catch", "landings", "discards", "stock")),

    # numbers
    setNames(rep(standard$numbers, 4),
      c("catch.n", "landings.n", "discards.n", "stock.n")),

    # proportions
    setNames(rep(standard$proportions, 3),
      c("mat", "harvest.spwn", "m.spwn")),

    # proportions
    setNames(rep(standard$m, 1),
      c("m")),
    
    # proportions
    setNames(rep(standard$harvest, 1),
      c("harvest"))
    )

    return(as.list(units))

    }
) # }}}

# dbind {{{

setMethod("dbind", signature(x="FLS", y="FLS"),
  function(x, y, ..., dim=3, names=NULL) {

    # ASSEMBLE list of input FLStock(s)
    args <- c(x, y, list(...))

    # EXTRACT FLQuant slots
    fqs <- lapply(args, as, 'FLQuants')

    # CALL dbind on dim across FLQuant list
    res <- lapply(setNames(nm=names(fqs[[1]])), function(i)
      do.call(dbind, c(lapply(fqs, '[[', i), dim=dim)))

    # BUILD new object
    out <- do.call(class(x), res)

    # ASSIGN names
    if(!is.null(names))
      dimnames(out) <- setNames(nm=names(dimnames(out))[dim], list(1:4))

    # TAKE name and desc from x
    name(out) <- name(x)
    desc(out) <- desc(x)

    return(out)
  }
)
# }}}
