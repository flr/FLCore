# FLStockLenBMS.R - DESC
# /FLStockLenBMS.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLStockLenBMS      {{{
setClass("FLStockLenBMS",
  representation(
  "FLStockLen",
  bms.n = "FLQuant",
  bms.wt = "FLQuant"
  ),
  prototype=prototype(
    range  = unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
    halfwidth = as.numeric(NA),
    catch  = FLQuant(dimnames=list(len=as.numeric(NA))),
    catch.n  = FLQuant(dimnames=list(len=as.numeric(NA))),
    catch.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    discards= FLQuant(dimnames=list(len=as.numeric(NA))),
    discards.n = FLQuant(dimnames=list(len=as.numeric(NA))),
    discards.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    landings   = FLQuant(dimnames=list(len=as.numeric(NA))),
    landings.n = FLQuant(dimnames=list(len=as.numeric(NA))),
    landings.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    bms.n = FLQuant(dimnames=list(len=as.numeric(NA))),
    bms.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    stock     = FLQuant(dimnames=list(len=as.numeric(NA))),
    stock.n   = FLQuant(dimnames=list(len=as.numeric(NA))),
    stock.wt = FLQuant(dimnames=list(len=as.numeric(NA))),
    m     = FLQuant(dimnames=list(len=as.numeric(NA))),
    mat     = FLQuant(dimnames=list(len=as.numeric(NA))),
    harvest   = FLQuant(dimnames=list(len=as.numeric(NA))),
    harvest.spwn = FLQuant(dimnames=list(len=as.numeric(NA))),
    m.spwn   = FLQuant(dimnames=list(len=as.numeric(NA)))
  ),
  validity=function(object) {

  return(TRUE)}
) # }}}

# FLStockLenBMS
setGeneric("FLStockLenBMS", function(object, ...)
  standardGeneric("FLStockLenBMS"))


setMethod("FLStockLenBMS", signature(object="missing"),
  function(...) {

    args <- list(...)
    bms <- args[names(args) %in% c("bms.n", "bms.wt")]
    args <- args[!names(args) %in% c("bms.n", "bms.wt")]

    res <- do.call("FLStockLen", args)
    res <- new("FLStockLenBMS", res)
    res@bms.n <- res@landings.n
    res@bms.n[] <- NA
    res@bms.wt <- res@landings.wt
    res@bms.wt[] <- NA

    if(length(bms) > 0)
      for(i in names(bms))
        slot(res, i) <- bms[[i]]

    return(res)
  }
)




# bms
setGeneric("bms", function(object, ...)
  standardGeneric("bms"))

setMethod("bms", signature(object="FLStockLenBMS"),
  function(object) {
    return(quantSums(object@bms.n * object@bms.wt))
  }
)

# bms.n
setGeneric("bms.n", function(object, ...)
  standardGeneric("bms.n"))

setMethod("bms.n", signature(object="FLStockLenBMS"),
  function(object) {
    return(object@bms.n)
  }
)

setGeneric("bms.n<-", function(object, ..., value)
  standardGeneric("bms.n<-"))

setMethod("bms.n<-", signature(object="FLStockLenBMS", value="FLQuant"),
  function(object, value) {
    object@bms.n <- value
    return(object)
  }
)

# bms.wt
setGeneric("bms.wt", function(object, ...)
  standardGeneric("bms.wt"))

setMethod("bms.wt", signature(object="FLStockLenBMS"),
  function(object) {
    return(object@bms.wt)
  }
)

setGeneric("bms.wt<-", function(object, ..., value)
  standardGeneric("bms.wt<-"))

setMethod("bms.wt<-", signature(object="FLStockLenBMS", value="FLQuant"),
  function(object, value) {
    object@bms.wt <- value
    return(object)
  }
)


# computeCatch  {{{
setMethod("computeCatch", signature(object="FLStockLenBMS"),
  function(object, slot="catch", na.rm=TRUE) {
    if(slot == "n"){
    # NA to 0
      res <- landings.n(object) + discards.n(object) + bms.n(object)
    }
    else if(slot == "wt") {
      res <- (landings.wt(object) * landings.n(object) +
        discards.wt(object) * discards.n(object) +
        bms.wt(object) * bms.n(object)) /
        (landings.n(object) + discards.n(object) + bms.n(object))
    }
    else if (slot == "all") {
      ctch.n     <-computeCatch(object, slot="n")
      ctch.wt    <-computeCatch(object, slot="wt")
      ctch       <-quantSums(ctch.n*ctch.wt, na.rm=na.rm)

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

# hr
setGeneric("hr", function(object, alive, ...)
  standardGeneric("hr"))

setMethod("hr", signature(object="FLStockLenBMS", alive="FLQuant"),
  function(object, alive) {
    res <- ((discards.n(object) * alive) + landings.n(object) + bms.n(object)) / stock.n(object)
    units(res) <- "hr"
    return(res)
  })

setMethod("hr", signature(object="FLStockLenBMS", alive="numeric"),
  function(object, alive) {
    alive <- FLQuant(alive, dimnames=dimnames(discards.n(object)), units="NA")
    return(hr(object, alive))
  })

setMethod("hr", signature(object="FLStockLenBMS", alive="missing"),
  function(object) {
    return(hr(object, 0))
  })
