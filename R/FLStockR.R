# FLStockR.R - DESC
# /home/mosqu003/Projects/FLR/pkgs/mine/FLCore/R/FLStockR.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# FLStockR

#' @examples
#' library(FLSRTMB)
#' data(ple4)
#' srr <- srrTMB(as.FLSR(ple4, model='bevholtSV'), spr0=spr0y(ple4))
#' sr <- predictModel(params=params(srr), model=bevholt()$model)
#' object <- FLStockR(ple4, refpts=FLPar(BMSY=150000, SB0=1.74e6), sr=sr)

# FLStockR class {{{

setClass("FLStockR", representation(
  "FLStock",
  refpts="FLPar",
  sr="predictModel"))

# }}}

# Constructors {{{

#' @rdname FLStockR
#' @aliases FLStockR FLStockR-methods
setGeneric('FLStockR', function(object, ...) standardGeneric('FLStockR'))

#' @examples
#' data(ple4)
#' stk <- FLStockR(ple4, refpts=FLPar(FMSY=0.275, SBMSY=7.33e05),
#'   sr=predictModel(model=rec~a*ssb/(b+ssb), params=FLPar(a=1155334, b=56040)))
#' refpts(stk)

setMethod("FLStockR", signature(object="FLStock"),
  function(object, ...) {

    args <- list(...)

    if(!is.null(args$sr) & is(args$sr, "FLSR"))
      args$sr <- as(args$sr, "predictModel")

    do.call("new", c(list(Class="FLStockR"), c(object, args)))

  }
)

#' @examples
#' # Objects can be constructed from individual slots
#' stkc <- FLStockR(catch.n(ple4), refpts=FLPar(FMSY=0.275, SBMSY=7.33e05))

setMethod("FLStockR", signature(object="ANY"),
  function(object, ...) {

    args <- c(list(object=object), list(...))
    
    if(!is.null(args$sr) & is(args$sr, "FLSR"))
      args$sr <- as(args$sr, "predictModel")
    
    idr <- names(args) %in% c("refpts", "sr")

    stk <- do.call("FLStock", args[!idr])

    out <- do.call("new", c(list(Class="FLStockR", stk), args[idr]))

    return(out)

  }
)

#' @examples
#' FLStockR(catch.n=catch.n(ple4), refpts=FLPar(FMSY=0.21))

setMethod("FLStockR", signature(object="missing"),
  function(...) {

    args <- list(...)
    
    idr <- names(args) %in% c("refpts", "sr")

    stk <- do.call("FLStock", args[!idr])

    out <- do.call("new", c(list(Class="FLStockR", stk), args[idr]))

    return(out)

  }
)

# }}}

# Accessors {{{

#' @examples
#'

setMethod("refpts", signature(object="FLStockR"),
  function(object) {
    return(object@refpts)
  }
)

#' @examples
#'

setReplaceMethod("refpts", signature(object="FLStockR", value="FLPar"),
  function(object, value) {
    object@refpts <- value
    return(object)
  }
)

#' @examples
#' refpts(ple4) <- FLPar(FMSY=0.275, SBMSY=7.33e05)
#' is(ple4, "FLStockR")
#' refpts(ple4)

setReplaceMethod("refpts", signature(object="FLStock", value="FLPar"),
  function(object, value) {
    object <- FLStockR(object, refpts=value)
    return(object)
  }
)
#' @examples
#'

setMethod("sr", signature(object="FLStockR"),
  function(object, slot="missing") {
    if(missing(slot))
      return(object@sr)
    else
      return(slot(object@sr, slot))
  }
)

#' @examples
#'

setReplaceMethod("sr", signature(object="FLStockR", value="predictModel"),
  function(object, value) {
      object@sr <- value
    return(object)
  }
)

setReplaceMethod("sr", signature(object="FLStock", value="predictModel"),
  function(object, value) {
    object <- FLStockR(object, sr=value)
    return(object)
  }
)

#' @examples
#'

setReplaceMethod("sr", signature(object="FLStockR", value="FLPar"),
  function(object, value) {
      object@sr@params <- value
    return(object)
  }
)

setReplaceMethod("sr", signature(object="FLStockR", value="formula"),
  function(object, value) {
      object@sr@model <- value
    return(object)
  }
)

setReplaceMethod("sr", signature(object="FLStockR", value="FLQuants"),
  function(object, value) {
    
    object@sr@.Data <- value
    names(object@sr) <- names(value)

    return(object)
  }
)

# }}}

# depletion {{{

setMethod("depletion", signature(x="FLStockR"),
  function(x, B0=refpts(x)$SB0) {
    unitSums(ssb(x)) / c(B0)
  }
)
# }}}

# predict

# ffwd

# coerce {{{
setAs('FLStockR', 'FLStock',
  function(from) {

    res <- FLStock()
    for(i in slotNames(res))
      slot(res, i) <- slot(from, i)

    return(res)
  })
# }}}

# depletion {{{

setMethod("depletion", signature(x="FLStockR"),
  function(x, SB0=refpts(x)$SB0) {
browser()
    if(is.character(SB0))
      SB0 <- refpts(x)[SB0,]

    return(unitSums(ssb(x)) / c(SB0))
  }
)
# }}}
