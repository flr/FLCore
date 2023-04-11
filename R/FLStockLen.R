# FLStockLen.R - FLStockLen and methods
# FLCore/R/FLStockLen.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLStockLen   {{{

#' @rdname FLStockLen
#' @aliases FLStockLen,FLQuant-method
#' @examples
#' stkl <- FLStockLen(m=FLQuant(0.2, dimnames=list(len=seq(5, 50, by=2), year=2015:2020)))
#' summary(stkl)

setMethod("FLStockLen", signature(object="FLQuant"),
  function(object, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
    units(object) <- "len"
    qobject <- quantSums(object)

    dims <- dims(object)

		# midpoint
		lens <- suppressWarnings(as.numeric(dimnames(object)[[1]]))
		if(all(is.na(lens)))
			halfwidth <- as.numeric(NA)
		else {
			hwd <-  (lens[-1] - lens[-length(lens)]) / 2
			if(all(hwd[1] == unique(hwd)))
			halfwidth <- hwd[1]
		else
			halfwidth <- hwd
		}

    res <- new("FLStockLen", halfwidth=halfwidth,
    catch=qobject, catch.n=object, catch.wt=object,
    landings=qobject, landings.n=object, landings.wt=object,
    discards=qobject, discards.n=object, discards.wt=object,
    stock=qobject, stock.n=object, stock.wt=object,
    harvest=object, harvest.spwn=object, m=object, m.spwn=object, mat=object, 
    range = unlist(list(min=dims$min, max=dims$max, 
			minyear=dims$minyear, maxyear=dims$maxyear, minfbar=dims$min, maxfbar=dims$max)))

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

		if(validObject(res))
			return(res)
		else
			stop("Invalid object created, check input FLQuant(s)")
  }
)

#' @rdname FLStockLen
#' @aliases FLStockLen,missing-method
#' @examples
#' # Unnamed FLQuant used for sizing
#' stkl <- FLStockLen(FLQuant(0.2, dimnames=list(len=seq(5, 50, by=2), year=2015:2020)))
#' summary(stkl)
#' m(stkl)

setMethod("FLStockLen", signature(object="missing"),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    argNms<- lapply(args, class)
    slots <- names(argNms)[argNms == "FLQuant"]

    flqs  <- names(argNms)[argNms == "FLQuants"]
    if(length(flqs) != 0)
       for (i in args[[flqs]])
          for (j in names(i))
             object <- i[[j]]

    if(length(slots) == 0)
      object <- FLQuant(dimnames=list(len=as.numeric(NA)))
    else{
      qslots <- slots[!slots %in% c("catch","stock","landings","discards")]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots[1]]]}

    return(FLStockLen(object, ...))})	# }}}

# halfwidth {{{
setMethod("halfwidth", signature(object="FLStockLen"),
	function(object, ...) {
		return(object@halfwidth)
	})

#' @examples
#' halfwidth(stkl)

setMethod("halfwidth<-", signature(object="FLStockLen", value="numeric"),
	function(object, ..., value) {

		len <- length(value)
		dims <- dims(object)

		# len is 1 or equal to dims$len
		if(len > 1)
			if(len != dims$len)
				stop("halfwidth must be of length 1 or equal to the number of length intervals")
		# halfwidth values match bounds
		if(!identical((rightbound(object) - leftbound(object)) / 2,
			rep(value, length=dims$len)))
			stop("halfwidth does not match existing interval bounds")

		object@halfwidth <- value

		return(object)
	}) # }}}

# breaks et al {{{
# breaks

#' @examples
#' breaks(stkl)
setMethod("breaks", signature(object="FLStockLen"),
	function(object, ...) {
		hwd <- halfwidth(object)
		brk <- as.numeric(dimnames(object@stock.n)[['len']])
		return(c(brk, brk[length(brk)] + hwd * 2))
	})


# leftbound

#' @examples
#' leftbound(stkl)
setMethod("leftbound", signature(object="FLStockLen"),
	function(object, ...) {
		return(as.numeric(dimnames(object@stock.n)[['len']]))
	})

# rightbound

#' @examples
#' rightbound(stkl)
setMethod("rightbound", signature(object="FLStockLen"),
	function(object, ...) {
		return(as.numeric(dimnames(object@stock.n)[['len']]) + 2 * halfwidth(object))
	})

# mids

#' @examples
#' mids(stkl)
setMethod("mids", signature(object="FLStockLen"),
	function(object, ...) {
		return(as.numeric(dimnames(object@stock.n)[['len']]) + halfwidth(object))
	}) # }}}
