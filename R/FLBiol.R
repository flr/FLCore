# FLBiol - class for representing a natural population
# FLCore/R/FLBiol.R

# Copyright 2003-2023 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

# class FLBiol {{{

#' Class FLBiol
#'
#' A class for modelling age / length or biomass structured populations.
#'
#' The \code{FLBiol} class is a representation of a biological fish population.
#' This includes information on abundances, natural mortality and fecundity.
#'
#' @name FLBiol
#' @aliases FLBiol-class n,FLBiol-method m,FLBiol-method wt,FLBiol-method fec,FLBiol-method spwn,FLBiol-method name,FLBiol-method desc,FLBiol-method range,FLBiol-method n<-,FLBiol,FLQuant-method m<-,FLBiol,FLQuant-method wt<-,FLBiol,FLQuant-method fec<-,FLBiol,FLQuant-method spwn<-,FLBiol,FLQuant-method name<-,FLBiol,character-method desc<-,FLBiol,character-method range<-,FLBiol,numeric-method FLBiol-class
#' FLBiolcpp-class
#' @docType class
#' @section Slots: \describe{
#'   \item{n}{Numbers in the population. \code{FLQuant}.}
#'   \item{m}{Mortality rate of the population. \code{FLQuant}.}
#'   \item{wt}{Mean weight of an individual. \code{FLQuant}.}
#'   \item{mat}{\code{predictModel}.}
#'   \item{fec}{\code{predictModel}.}
#'   \item{rec}{\code{predictModel}.}
#'   \item{spwn}{Proportion of time step at which spawning ocurrs. \code{FLQuant}.}
#'   \item{name}{Name of the object. \code{character}.}
#'   \item{desc}{Brief description of the object. \code{character}.}
#'   \item{range}{Named numeric vector describing the range of the object. \code{numeric}.} }
#' @template Accessors
#' @template Constructors
#' @param plusgroup Plusgroup age, to be stored in range
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author The FLR Team
#' @seealso \link{as.FLBiol}, \link{as.FLSR}, \link[methods]{coerce}, \link[graphics]{plot}, \link{ssb} \link{catch.n,FLBiol-method}
#' @keywords classes
#' @examples
#'
#' # An FLBiol example dataset
#' data(ple4.biol)
#'
#' summary(ple4.biol)
#'
setClass("FLBiol",
  representation(
    "FLComp",
    n        ="FLQuant",
    m        ="FLQuant",
    wt       ="FLQuant",
    mat      ="predictModel",
    fec      ="predictModel",
    rec      ="predictModel",
    spwn     ="FLQuant"),
  prototype=prototype(
    range    = unlist(list(min=NA, max=NA, plusgroup=NA, minyear=1, maxyear=1)),
    n        = FLQuant(),
    m        = FLQuant(),
    wt       = FLQuant(),
    mat      = new('predictModel', model=as.formula("~mat", env=emptyenv())),
    fec      = new('predictModel', model=as.formula("~fec", env=emptyenv())),
    rec      = new('predictModel', model=as.formula("~rec", env=emptyenv())),
    spwn     = FLQuant()),
  validity = function(object) {

    #TODO: 1 or N
    # dim[1] of spwn is of length 1
    # if(dim(object@spwn)[1] != 1)
    #  return(paste0("First dimension '", names(object@spwn)[1], "' of @spwn can only be of length 1"))

    return(TRUE)
  }
)

invisible(createFLAccesors("FLBiol", exclude=c('name', 'desc', 'range', 'fec', 'rec', 'mat')))  # }}}

# class FLBiolcpp {{{
setClass("FLBiolcpp",
  representation(
    "FLComp",
    n        ="FLQuant",
    m        ="FLQuant",
    wt       ="FLQuant",
    mat      ="FLQuant",
    fec      ="FLQuant",
    spwn     ="FLQuant",
    srmodel = "character",
    srparams = "FLQuant"),
  prototype=prototype(
    range    = unlist(list(min=NA, max=NA, plusgroup=NA, minyear=1, maxyear=1)),
    n        = FLQuant(),
    m        = FLQuant(),
    wt       = FLQuant(),
    mat      = FLQuant(),
    fec      = FLQuant(),
    spwn     = FLQuant(),
    srmodel = character(1),
    srparams = FLQuant())
) 

invisible(createFLAccesors("FLBiolcpp", exclude=c('name', 'desc', 'range')))  # }}}

# rec.hat {{{

#' @examples
#' data(ple4.biol)
#' # Predict recruitment from ssb() and model & params
#' rec.hat(ple4.biol)
#' # Compare with
#' rec(ple4.biol)

setMethod('rec.hat', signature('FLBiol'),
  function(object, what=TRUE, ...) {

    rec.age <- as.numeric(dimnames(n(object))[["age"]])[1]
    
    rec <- returnPredictModelSlot(object, what=what, slot="rec", ...)
    
    # CORRECT dimnames if computed rec
    if(isTRUE(what)) {

      # CORRECT dimnames$year by rec.age
      dimnames(rec)[["year"]] <- 
        as.numeric(dimnames(rec)[["year"]]) + rec.age
    
      # CORRECT dimnames$age
      dimnames(rec)[["age"]] <- rec.age
    }

    return(rec)
  })
# }}}

# rec {{{
setMethod('rec', signature('FLBiol'),
  function(object, what=TRUE, ...) {
    return(n(object)[1,])
  }
)
# }}}

# sr {{{
setMethod("sr", signature(object="FLBiol"),
  function(object) {
    return(slot(object, "rec"))
  }
) # }}}

# rec<- {{{

# rec<- predictModel
setReplaceMethod('rec', signature(object='FLBiol', value='predictModel'),
  function(object, what, value=what) {
    object@rec <- value
    return(object)
  }
)

# rec<- FLQuant: change to rec@.Data['rec']
setReplaceMethod('rec', signature(object='FLBiol', value='FLQuant'),
  function(object, name="rec", value) {
    
    # HACK Should be solved by adding name to generic
    if(missing(value))
      value <- FLQuants(rec=name)
    else {
      value <- FLQuants(rec=value)
    }
    object@rec@.Data <- value
    names(object@rec) <- names(value)
    return(object)
  }
)

# rec<- FLQuants: assign to @.Data
setReplaceMethod('rec', signature(object='FLBiol', value='FLQuants'),
  function(object, value) {
    object@rec@.Data <- value
    names(object@rec) <- names(value)
    return(object)
  }
)

# rec<- formula:
setReplaceMethod('rec', signature(object='FLBiol', value='formula'),
  function(object, ..., value) {
    object@rec@model <- value
    return(object)
  }
)

# rec<- params:
setReplaceMethod('rec', signature(object='FLBiol', value='FLPar'),
  function(object, value) {
    object@rec@params <- value
    return(object)
  }
) 

# rec<- list:
setReplaceMethod('rec', signature(object='FLBiol', value='list'),
  function(object, value) {

    idx <- unlist(lapply(value, is, 'FLQuant'))

    for(i in names(value)[!idx])
      object <- do.call("rec<-", list(object=object, value=value[[i]]))

    value <- FLQuants(value[idx])
 
    rec(object) <- value
    
    return(object)
  }
) # }}}

# sr<- {{{

# sr<- predictModel
setReplaceMethod('sr', signature(object='FLBiol', value='predictModel'),
  function(object, what, value=what) {
    object@rec <- value
    return(object)
  }
)

# sr<- FLQuant: change to rec@.Data['rec']
setReplaceMethod('sr', signature(object='FLBiol', value='FLQuant'),
  function(object, name="rec", value) {
    
    # HACK Should be solved by adding name to generic
    if(missing(value))
      value <- FLQuants(rec=name)
    else {
      value <- FLQuants(rec=value)
    }
    object@rec@.Data <- value
    names(object@rec) <- names(value)
    return(object)
  }
)

# sr<- FLQuants: assign to @.Data
setReplaceMethod('sr', signature(object='FLBiol', value='FLQuants'),
  function(object, value) {
    object@rec@.Data <- value
    names(object@rec) <- names(value)
    return(object)
  }
)

# sr<- formula:
setReplaceMethod('sr', signature(object='FLBiol', value='formula'),
  function(object, ..., value) {
    object@rec@model <- as.formula(format(value), env=emptyenv())
    return(object)
  }
)

# sr<- params:
setReplaceMethod('sr', signature(object='FLBiol', value='FLPar'),
  function(object, value) {
    object@rec@params <- value
    return(object)
  }
) 

# sr<- list:
setReplaceMethod('sr', signature(object='FLBiol', value='list'),
  function(object, value) {

    idx <- unlist(lapply(value, is, 'FLQuant'))

    for(i in names(value)[!idx])
      object <- do.call("sr<-", list(object=object, value=value[[i]]))

    value <- FLQuants(value[idx])
 
    sr(object) <- value
    
    return(object)
  }
) # }}}

# fec {{{
setMethod('fec', signature('FLBiol'),
  function(object, what=TRUE, ...) {
    return(returnPredictModelSlot(object, what=what, slot="fec", ...))
  })
# }}}

# fec<- {{{

# fec<- predictModel
setReplaceMethod('fec', signature(object='FLBiol', value='predictModel'),
  function(object, what, value=what) {
    object@fec <- value
    return(object)
  }
)

# fec<- FLQuant: change to fec@.Data['fec']
setReplaceMethod('fec', signature(object='FLBiol', value='FLQuant'),
  function(object, name="fec", value) {
    
    # HACK Should be solved by adding name to generic
    if(missing(value))
      value <- FLQuants(fec=name)
    else {
      value <- FLQuants(fec=value)
    }
    object@fec@.Data <- value
    names(object@fec) <- names(value)
    return(object)
  }
)

# fec<- FLQuants: assign to @.Data
setReplaceMethod('fec', signature(object='FLBiol', value='FLQuants'),
  function(object, value) {
    object@fec@.Data <- value
    names(object@fec) <- names(value)
    return(object)
  }
)

# fec<- formula:
setReplaceMethod('fec', signature(object='FLBiol', value='formula'),
  function(object, ..., value) {
    object@fec@model <- as.formula(format(value), env=emptyenv())
    return(object)
  }
)

# fec<- params:
setReplaceMethod('fec', signature(object='FLBiol', value='FLPar'),
  function(object, value) {
    object@fec@params <- value
    return(object)
  }
) 

# fec<- list:
setReplaceMethod('fec', signature(object='FLBiol', value='list'),
  function(object, value) {

    idx <- unlist(lapply(value, is, 'FLQuant'))

    for(i in names(value)[!idx])
      object <- do.call("fec<-", list(object=object, value=value[[i]]))

    value <- FLQuants(value[idx])
 
    fec(object) <- value
    
    return(object)
  }
) # }}}

# mat {{{
setMethod('mat', signature('FLBiol'),
  function(object, what=TRUE, ...) {
    return(returnPredictModelSlot(object, what=what, slot="mat", ...))
  })
# }}}

# mat<- {{{

# mat<- predictModel
setReplaceMethod('mat', signature(object='FLBiol', value='predictModel'),
  function(object, what, value=what) {
    object@mat <- value
    return(object)
  }
)

# mat<- FLQuant: change to mat@.Data['mat']
setReplaceMethod('mat', signature(object='FLBiol', value='FLQuant'),
  function(object, name="mat", value) {
    
    # HACK Should be solved by adding name to generic
    if(missing(value))
      value <- FLQuants(mat=name)
    else {
      value <- FLQuants(mat=value)
    }
    object@mat@.Data <- value
    names(object@mat) <- names(value)
    return(object)
  }
)

# mat<- FLQuants: assign to @.Data
setReplaceMethod('mat', signature(object='FLBiol', value='FLQuants'),
  function(object, value) {
    object@mat@.Data <- value
    names(object@mat) <- names(value)
    return(object)
  }
)

# mat<- formula:
setReplaceMethod('mat', signature(object='FLBiol', value='formula'),
  function(object, ..., value) {
    object@mat@model <- as.formula(format(value), env=emptyenv())
    return(object)
  }
)

# mat<- params:
setReplaceMethod('mat', signature(object='FLBiol', value='FLPar'),
  function(object, value) {
    object@mat@params <- value
    return(object)
  }
) 

# mat<- list:
setReplaceMethod('mat', signature(object='FLBiol', value='list'),
  function(object, value) {

    idx <- unlist(lapply(value, is, 'FLQuant'))

    for(i in names(value)[!idx])
      object <- do.call("mat<-", list(object=object, value=value[[i]]))

    value <- FLQuants(value[idx])
 
    mat(object) <- value
    
    return(object)
  }
) # }}}

# summary {{{
#' @rdname summary-methods
#' @aliases summary,FLBiol-method
setMethod("summary", signature(object="FLBiol"),
  function(object) {

    # CAT name, desc, range and FLQuant slots
    callNextMethod()

    # predictModel slots
    pnames <- getSlotNamesClass(object, 'predictModel')
    for (i in pnames) {
      # name
      cat(substr(paste0(i, "          "), start=1, stop=14),
      # model
      as.character(slot(object, i)@model), "\n")
      # FLQuants
      if(length(names(slot(object, i))) > 0) {
        for(j in names(slot(object, i))) {
          cat(substr(paste0("  ", j, "          "), start=1, stop=12),
            " : [", dim(slot(object,i)[[j]]),"], units = ",
            slot(object,i)[[j]]@units, "\n")
        }
      }
      # params
      par <- slot(object, i)@params
      cat(substr(paste0("  ", ifelse(all(sum(!is.na(par)) == 0 & 
        dimnames(par)[[1]] == ""),
        "NA", paste(dimnames(par)[[1]], collapse=", ")),
        "           "), start=1, stop=12), " : [", dim(slot(object,i)@params),
        "], units = ", slot(object,i)@params@units, "\n")
    }
  }
) # }}}

# FLBiol()   {{{
#' @rdname FLBiol
#' @aliases FLBiol,missing-method FLBiol,FLQuant-method FLBiolcpp-class
setMethod('FLBiol', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...) {

    args <- list(...)

    # empty object
    object[] <- NA
    units(object) <- "NA"

    dims <- dims(object)

    # PARSE mat and rec if FLQuant
    if(is(args$mat, "FLQuant"))
      args$mat <- new('predictModel', FLQuants(mat=args$mat),
        model=as.formula("~mat", env=emptyenv()))

    if(is(args$fec, "FLQuant"))
      args$fec <- new('predictModel', FLQuants(fec=args$fec),
        model=as.formula("~fec", env=emptyenv()))

    if(is(args$rec, "FLQuant"))
      args$rec <- new('predictModel', FLQuants(mat=args$rec),
        model=as.formula("~rec", env=emptyenv()))

    res <- new("FLBiol",
      n=object, m=object, wt=object, spwn=object[1,],
      mat = new('predictModel', FLQuants(mat=object),
        model=as.formula("~mat", env=emptyenv())),
      fec = new('predictModel', FLQuants(fec=object),
        model=as.formula("~fec", env=emptyenv())),
      rec = new('predictModel', FLQuants(rec=object[1,]),
        model=as.formula("~rec", env=emptyenv())),
      range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
        minyear=dims$minyear, maxyear=dims$maxyear)))

    # Load given slots
    for(i in names(args))
      slot(res, i) <- args[[i]]
    # FIX rec to n[1,] if no rec given
    if(! 'rec' %in% names(args) & 'n' %in% names(args))
      res@rec <- new('predictModel', FLQuants(rec=res@n[1,]),
        model=as.formula("~rec", env=emptyenv()))

    return(res)
  }
)

setMethod('FLBiol', signature(object='missing'),
  function(...) {
    args <- list(...)

    slots <- unlist(lapply(args, function(x) is(x, 'FLQuant')))
    slots <- names(slots[slots])

    # if no FLQuant argument given, then use empty FLQuant
    if(length(slots) == 0)
      object <- FLQuant()
    
    # if 1, use it
    else if(length(slots) == 1)
      object <- args[[slots[1]]]
    
    # if 2+, use !spwn
    else {
      slots <- slots[!slots %in% 'spwn']
      object <- args[[slots[1]]]
    }
    return(FLBiol(object, ...))
  }
) # }}}

# FLBiols {{{

#' Class FLBiols
#' 
#' A list of \code{FLBiol} objects.
#'
#' @name FLBiols
#' @aliases FLBiols-class 
#' @docType class
#' @section Slots: \describe{
#'   \item{.Data}{Internal S4 data representation, of class \code{list}.}
#'   \item{desc}{As textual description of the object contents}
#'   \item{lock}{Can the object be extended/trimmed? \code{TRUE} or \code{FALSE}.}
#'   \item{names}{A character vector for the element names} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \code{\link{FLlst}}, \code{\link[base]{list}},
#'   \code{\link[base]{vector}}
#' @keywords classes
#'
setClass("FLBiols", contains="FLComps",
  validity=function(object){

  # All items are FLBiol
  if(!all(unlist(lapply(object, is, 'FLBiol'))))
      return("Components must be FLBiol")

  return(TRUE)
  }
)

# constructor
#' @rdname FLBiols
#' @aliases FLBiols,FLBiol-method
setMethod("FLBiols", signature(object="FLBiol"), function(object, ...) {
    lst <- c(object, list(...))
    FLBiols(lst)
})

#' @rdname FLBiols
#' @aliases FLBiols,missing-method
setMethod("FLBiols", signature(object="missing"),
  function(...) {
    # empty
    if(missing(...)){
      new("FLBiols")
    # or not
    } else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLBiols',  c(list(object=object), args))
    }
  }
)

#' @rdname FLBiols
#' @aliases FLBiols,list-method
setMethod("FLBiols", signature(object="list"),
  function(object, ...) {

    args <- list(...)

    # names in args, ...
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLBiols", .Data=object, names=names),
      args[!names(args)%in%'names'])
    
    return(
      do.call('new', args)
      )

new('FLBiols', args[[2]])

}) # }}}

# rec.obs {{{
setMethod("rec.obs", signature(object="FLBiol"),
  function(object, age=1) {
    return(n(object)[age,])
  }
) # }}}

# qapply		{{{
setMethod('qapply', signature(X='FLBiol', FUN='function'),
	function(X, FUN, ..., exclude=missing) {

    res <- callNextMethod()
		
    FUN <- match.fun(FUN)

    slots <- getSlotNamesClass(X, 'predictModel')

		if(!missing(exclude))
      slots <- slots[!slots %in% exclude]
    
    if(is(res, 'FLBiol'))
  		for (i in slots)
        res <- do.call(paste0(i, "<-"), list(object=res,
          value=do.call(FUN, c(list(slot(X, i)), ...))))

		return(res)
	}
)   # }}}

# trim {{{

#' @rdname trim
#' @aliases trim,FLBiol-method

setMethod("trim", signature(x="FLBiol"),
  function(x, ...) {

  # trim all but spwn
  for(i in c("n", "m", "wt", "mat", "fec", "rec")) {
      slot(x, i) <- trim(slot(x, i), ...)
  }

  # spwn
  args <- list(...)
  args <- args[names(args) != dims(x)$quant]
  
  if(length(args) > 0)
    slot(x, "spwn") <- do.call("trim", c(list(x=slot(x, "spwn")), args))

  # RANGE
  x@range[c("min", "max", "minyear", "maxyear")] <- 
    unlist(dims(x@n)[c("min", "max", "minyear", "maxyear")])
  x@range["plusgroup"] <- min(range(x, c("max", "plusgroup")))

  return(x)

  }
) # }}}

# '[' {{{
setMethod('[', signature(x='FLBiol'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE) {
    
    res <- callNextMethod()
    
    pnames <- names(getSlots(class(x))[getSlots(class(x))=="predictModel"])
	
    args <- list(drop=FALSE)

		if (!missing(i))
      args <- c(args, list(i=i))
		if (!missing(j))
      args <- c(args, list(j=j))
		if (!missing(k))
      args <- c(args, list(k=k))
		if (!missing(l))
      args <- c(args, list(l=l))
		if (!missing(m))
      args <- c(args, list(m=m))
		if (!missing(n))
      args <- c(args, list(n=n))
    
    for(p in pnames) {
      if(length(slot(x, p)) > 0)
        slot(res, p) <- do.call('[', c(list(x=slot(x, p)), args))
    }

    return(res)
    }
)   # }}}

# iter {{{
setMethod("iter", signature(obj="FLBiol"),
  function(obj, iter) {
    
    res <- callNextMethod()

    for(i in c('mat', 'fec', 'rec'))
      slot(res, i) <- iter(slot(res, i), iter)
    
    return(res)
  }
) # }}}

# iter<- {{{
setMethod("iter<-", signature(object="FLBiol", value="FLBiol"),
  function(object, iter, value) {

    res <- callNextMethod()

    for(i in c('mat', 'fec', 'rec'))
      slot(res, i) <- slot(value, i)

    return(res)
  }
) # }}}

# fwdWindow {{{
setMethod("fwdWindow", signature(x="FLBiol", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3, deviances=1) {

    # RETURN if no new years
    if(end == dims(x)$maxyear)
      return(x)

    # EXTEND x with window
    res <- window(x, end=end, extend=TRUE, frequency=1)

    # NEW window years
    wyrs <- seq(dim(m(x))[2] + 1, dim(m(res))[2])
    sqyrs <- seq(dim(m(x))[2] - nsq + 1, dim(m(x))[2])

    # m
    m(res)[, wyrs] <- yearMeans(m(res)[, sqyrs])

    # wt
    wt(res)[, wyrs] <- yearMeans(wt(res)[, sqyrs])

    # spwn
    spwn(res)[, wyrs] <- yearMeans(spwn(res)[, sqyrs])

    # mat
    if(length(res@mat@.Data) > 0)
    res@mat@.Data <- lapply(mat(res, FALSE), function(x) {
      x[, wyrs] <- yearMeans(x[, sqyrs])
      return(x)})@.Data

    # fec
    if(length(res@fec@.Data) > 0)
    res@fec@.Data <- lapply(fec(res, FALSE), function(x) {
      x[, wyrs] <- yearMeans(x[, sqyrs])
      return(x)})@.Data

    # rec: EXTEND only
    if(length(res@rec@.Data) > 0)
    res@rec@.Data <- lapply(res@rec@.Data, function(y) {
      return(window(y, end=end))
      })
    names(res@rec) <- names(x@rec)

    # deviances
    if(is(deviances, "FLQuant")) {
      FLCore::deviances(res) <- append(FLCore::deviances(x), deviances)
    } else if(is(deviances, "numeric")) {
      FLCore::deviances(res)[, ac(seq(dims(x)$maxyear + 1, end))][] <- deviances
    } else if(is(deviances, 'function')) {
      FLCore::deviances(res) <- append(FLCore::deviances(x), do.call(deviances,
        list(x=FLCore::deviances(res), year=dims(x)$maxyear)))
    }

    return(res)
  }
) # }}}

# ssb  {{{

#' @rdname ssb
#' @details Objects of the *FLBiol* class do not contain any information on
#' catch or fishing mortality, so a call to `ssb()` will only correct abundances
#' for natural mortality to the moment of spawning. The method can also take
#' information on catches or fishing mortality and use them when calculating
#' abundances at spawning time. An *FLQuant* named either 'catch.n', 'f', 'hr' or
#' 'harvest' can be used. The first three are self-explanatory, while for the last
#' units must be either 'f' or 'hr'. The quantities should refer to total yearly
#' values, as the value in the 'spwn' slot will be used to calculate what fraction
#' of fishing mortality to apply.
#' @aliases ssb-FLBiol,method
#' @examples
#' biol <- as(ple4, "FLBiol")
#' # SSB from FLBiol, abundances corrected only for M
#' ssb(biol)
#' # Provide catch-at-age, F or HR to correct N
#' ssb(biol, catch.n=catch.n(ple4))
#' ssb(biol, f=harvest(ple4))
#' ssb(biol, harvest=harvest(ple4))
#' ssb(biol, hr=catch.n(ple4) / stock.n(ple4))

setMethod("ssb", signature(object="FLBiol"),
  function(object, ...) {

    args <- list(...)

    if(length(args) > 1)
      stop("Only one extra argument allowed: 'catch.n', 'harvest', 'f' or 'hr'")
    
    # NO catch data
    if(length(args) == 0) {
      res <- quantSums(n(object) * wt(object) * mat(object) %*%
        exp(-spwn(object) %*% m(object)), na.rm=FALSE)
    } else {
      
      res <- switch(names(args),
        # catch.n
        # DEBUG How good is this f approximation?
        "catch.n" = quantSums(ssb(object, f=-log(1-args$catch.n / n(object))),
          na.rm=FALSE),
        # hr
        "hr" = quantSums(ssb(object, f=-log(1-args$hr)),
          na.rm=FALSE),
        # f
  			"f" = quantSums(n(object) *
          exp(-(args$f %*% spwn(object)) - (m(object) %*% spwn(object))) *
          wt(object) * mat(object), na.rm=FALSE),
        # harvest, units == 'f' / 'hr'
        "harvest" = switch(units(args$harvest),
          "f" = ssb(object, f=args$harvest),
          "hr" = ssb(object, hr=args$harvest), NULL),
        NULL)
    }
    if(is.null(res))
      stop("catch information must be one of 'catch.n', 'f', 'hr' or 'harvest'")
    
    # units(res) <- uom("*", units(n(object)), units(wt(object)))

    return(res)
  }
)  # }}}

# tep  {{{

setMethod("tep", signature(object="FLBiol"),
  function(object, ...)
  {
    args <- list(...)

    if(length(args) > 1)
      stop("Only one extra argument allowed: 'catch.n', 'harvest', 'f' or 'hr'.")
    
    # NO catch data
    if(length(args) == 0) {
      res <- quantSums(n(object) * wt(object) * mat(object) * fec(object) %*%
        exp(-spwn(object) %*% m(object)), na.rm=FALSE)
    } else {
      res <- switch(names(args),
        # catch.n
        # DEBUG How good is this f approximation?
        "catch.n" = quantSums(ssb(object, f=-log(1-args$catch.n / n(object))),
          na.rm=FALSE),
        # hr
        "hr" = quantSums(ssb(object, f=-log(1-args$hr)),
          na.rm=FALSE),
        # f
  			"f" = quantSums(n(object) * exp(-(args$f %*%
          spwn(object) + m(object) %*% spwn(object))) *
          wt(object) * mat(object) * fec(object), na.rm=FALSE),
        # harvest, units == 'f' / 'hr'
        "harvest" = switch(units(args$harvest),
          "f" = ssb(object, f=args$harvest),
          "hr" = ssb(object, hr=args$harvest), NULL),
        NULL)
    }
    if(is.null(res))
      stop("catch information must be one of 'catch.n', 'f', 'hr' or 'harvest'")
    
    # units(res) <- uom("*", units(n(object)), units(wt(object)))

    return(res)
  }
)  # }}}

# combine {{{

setMethod('combine', signature(x='FLBiol', y='FLBiol'),
  function(x, y, ..., check=FALSE) {

    args <- c(list(x, y), list(...))

    res <- callNextMethod()

    # mat
    res@mat <- do.call(combine, lapply(args, slot, "mat"))

    # fec
    res@fec <- do.call(combine, lapply(args, slot, "fec"))
    
    # rec
    res@rec <- do.call(combine, lapply(args, slot, "rec"))

    return(res)
  }
)
# }}}

# meanLifespan {{{
setMethod("meanLifespan", signature(x="FLBiol"),
  function(x, ref.age = 'missing',...) {

    # checks
    if(missing(ref.age))
      ref.age <- dims(m(x))$min

    if(ref.age >= dims(m(x))$max)
      stop("Error in mean.lifespan: reference age greater than last true age")
    mm <- trim(m(x),age=ref.age:dims(m(x))$max)
    mm <- yearMeans(mm)
    mm <- seasonSums(mm)

    # assuming last true age's M is the future M
    # apply the actuarial formula for mean lifspan
    # ::
    # function m.lf to be applied to unit, seas

    m.lf <- function(x) {
      xx <- array(rep(NA,1000))
      xx[1:length(x)] <- x[]
      xx[(length(x)+1):1000] <- x[length(x)]
      lf <- 0
      for(i in 1:1000)
          lf <- lf + prod(exp(-xx[1:i]))
      return(lf)
    }

    mm <- apply(mm,2:6,m.lf)

    # return the FLQuant age/year aggregated but with unit, area and iter
    # specific values of the mean lifespan

    return(mm)
  }
)# }}}

# tsb  {{{
setMethod("tsb", signature(object="FLBiol"),
  function(object, ...)
  {
    res <- quantSums((n(object) * wt(object)) * exp(-spwn(object) %*%
      m(object)), na.rm=FALSE)
    return(res)
  }
)  # }}}

# tb  {{{
setMethod("tb", signature(object="FLBiol"),
  function(object, ...)
  {
    res <- quantSums(n(object) * wt(object))
    return(res)
  }
)  # }}}

# vb = vulnerable biomass {{{

setMethod("vb", signature(x="FLBiol", sel="FLQuant"),
  function(x, sel) {
    
    vb <- quantSums(n(x) * wt(x) %*% sel)

    units(vb) <- uom("*", units(n(x)), units(wt(x)))
    
    return(vb)
  }
)

# }}}

# computeStock  {{{
setMethod("computeStock", signature(object="FLBiol"),
  function(object, ...)
    return(quantSums(n(object) * wt(object) , ...))
)  # }}}

# ssn  {{{
setMethod("ssn", signature(object="FLBiol"),
  function(object, ...)
    return(quantSums(n(object) * fec(object) * exp(-spwn(object) %*% m(object)), ...))
)  # }}}

# harvest {{{
setMethod('harvest', signature(object='FLBiol', catch='missing'),
  function(object, fratio=1)
    {
    now <- object@n
    dims <- dim(now)
    res <- now
    res[1:(dims[1]-1), 1:(dims[2]-1)] <- now[2:dims[1], 2:dims[2]]

    res <- log(now/res)

    # last age as previous
    res[dims[1],] <- res[dims[1]-1,]

    # SUBSTRACT m
    res[, 1:(dims[2]-1)] <- res[, 1:(dims[2]-1)] - m(object)[, 1:(dims[2]-1)]

  ##Plusgroup stuff
  pgF<-function(object, hrvst, a=1) {

    #deriv(y~n1*exp(-f-m2)+n2*exp(-f*a-m2)-n3,"f")
    d.<-function(f,n1,n2,n3,m1,m2,a=1){
            .expr1 <- -f
            .expr4 <- n1 * exp(.expr1 - m2)
            .expr7 <- exp(.expr1 * a - m2)
            .value <- .expr4 + n2 * .expr7 - n3
            .grad <- array(0, c(length(.value), 1L), list(NULL, c("f")))
            .grad[, "f"] <- -(n2 * (.expr7 * a) + .expr4)
            attr(.value, "gradient") <- .grad

            return(.value)
        }

    for (i in 1:(dims(hrvst)$year)){
      n1<-c(n(object)[ac(range(object,"plusgroup")-1),i])
      n2<-c(n(object)[ac(range(object,"plusgroup"))  ,i])
      n3<-c(n(object)[ac(range(object,"plusgroup"))  ,i+1])

      m1<-c(m(object)[ac(range(object,"plusgroup")-1),i])
      m2<-c(m(object)[ac(range(object,"plusgroup"))  ,i])

      x    <-0.1
      f.   <-10
      Iters<-0
      while (abs(f.) >= 10e-10 && Iters <= 50)
        {
        Iters<-Iters+1
        res<-d.(x,n1,n2,n3,m1,m2,a)

        f.   = res[[1]]
        dfdx = attr(res,"gradient")

        x = (x - f./dfdx)
        }

      hrvst[ac(range(object,"plusgroup"))  ,i]<-x
      hrvst[ac(range(object,"plusgroup")-1),i]<-x*a
      }

    return(hrvst)
    }

    if (("plusgroup" %in% names(range(object)) && !is.na(range(object,"plusgroup"))))
     res[, 1:(dims[2]-1)] <-pgF(object, res[, 1:(dims[2]-1)], a=fratio)
    
    # NA last year
    res[, dims[2]] <- NA

    units(res) <- 'f'

    return(res)
  }
) # }}}

# r {{{
# calculates the intrinsic rate of increase from the Leslie-transition matrix
# or the Euler-Lotka equation by year or by cohort.
setMethod("r", signature(m="FLQuant", fec="FLQuant"),
  function(m, fec, by = 'year', method = 'el',...)
  {
    # checks
    if(by != 'year' && by != 'cohort')
      stop("Error in r: direction of estimation is neither year nor cohort")

    if(method != 'leslie' && method != 'el')
      stop("Error in r: method used is neither Leslie matrix or Euler-Lotka")

    # estimate by year
    if(by == 'year')
    {
      dmf <- dim(fec)
      dmm <- dim(m)
      age <- as.numeric(dimnames(fec)$age)

      # solve Euler-Lotka equation
      if(method == 'el')
      {
        m <- survprob(m)

        elfunc <- function(ff, p, age)
        {
          # solve Euler-Lotka using optimise
          elfn <- function(x)
            return((sum(exp(-x[1] * age) * p * ff) - 1) ^ 2)

          res.r <- optimise(elfn, interval=c(-10,10))[[1]]
          return(res.r)
        }

        if(dmf[6] > 1 && dmm[6] > 1 && (dmf[6] != dmm[6]))
          stop("Error in r: iteration dimensions are not the same for fec and m")

        nits <- max(dmf[6], dmm[6])

        if(dmf[6] > 1 && dmm[6] == 1)
        {
          tmp <- m
          ps <- fec
          ps[] <- tmp[]
          rm(tmp)
          nits <- dmf[6]
        }

        if(dmf[6] == 1 && dmm[6] > 1)
        {
          tmp <- fec
          f <- m
          f[] <- tmp[]
          rm(tmp)
          nits <- dmm[6]
        }

        r.ret <- FLQuant(dim=c(1,dmf[2],1,1,1,nits),
          dimnames=dimnames(quantMeans(fec))[1:5])

        # define required variables for the estimation
        for(y in 1:dmf[2])
        {
          # loop over the iterations
          for(i in 1:nits)
          {
            ff <- as.vector(fec[,y,,,,i])
            p <- as.vector(m[,y,,,,i])

            r.ret[,y,,,,i] <- elfunc(ff, p, age)
          }
        }
      }

      # use Leslie matrix lead eigenvalues
      else if(method == 'leslie')
      {
        m <- exp(-m)

        # define function to construct leslie matrix and calculate r

        lesfunc <- function(ff, p) {

          # construct the leslie matrix
          lesm <- matrix(ncol=length(ff),nrow=length(ff))

          lesm[,] <- 0
          lesm[1,] <- ff[]
          na <- length(ff)
          for(a in 1:(na-1))
            lesm[a+1,a] <- p[a+1]

          # calculate log of real part of the lead eigenvalue of the leslie matrix
          res.r <- log(max(Re(eigen(lesm)[['values']])))

          return(res.r)
        }

        if(dmf[6] > 1 && dmm[6] > 1 && (dmf[6] != dmm[6]))
          stop("Error in r: iteration dimensions are not the same for fec and m")

        nits <- max(dmf[6], dmm[6])

        if(dmf[6] > 1 && dmm[6] == 1)
        {
          tmp <- m
          ps <- fec
          ps[] <- tmp[]
          rm(tmp)
          nits <- dmf[6]
        }

        if(dmf[6] == 1 && dmm[6] > 1)
        {
          tmp <- fec
          f <- m
          f[] <- tmp[]
          rm(tmp)
          nits <- dmm[6]
        }

        r.ret <- FLQuant(dim=c(1,dmf[2],1,1,1,nits),
          dimnames=dimnames(quantMeans(fec))[1:5])

        for(y in 1:dmf[2])
        {
          # loop over the iterations
          for(i in 1:nits)
          {
            ff <- as.vector(fec[,y,,,,i])
            p <- as.vector(m[,y,,,,i])
            r.ret[,y,,,,i] <- lesfunc(ff,p)
          }
        }
      }
    }

    # estimate by cohort

    else if(by == 'cohort') {
      stop("not implemented yet")
    }

    return(r.ret)
  }
)

setMethod("r", signature(m="FLBiol", fec="missing"),
  function(m, by = 'year', method = 'el',...)
  {
    r(m(m), fec(m), by=by, method=method,)
  }
) # }}}

# survprob {{{
# estimate survival probabilities by year or cohort
setMethod("survprob", signature(object="FLBiol"),
  function(object, by = 'year',...) {

    # estimate by year
    if(by == 'year')
      return(survprob(m(object)))

    # estimate by cohort
    else if(by == 'cohort')
      return(survprob(FLCohort(m(object))))

  }
) # }}}

# setPlusGroup {{{
setMethod('setPlusGroup', signature(x='FLBiol', plusgroup='numeric'),
s.<-  function(x, plusgroup, na.rm=FALSE)
  {
  pg.wt.mean <-c("wt","m","fec","spwn")

  #check plusgroup valid
  if (!missing(plusgroup))
     x@range["plusgroup"]<-plusgroup
  if(x@range["plusgroup"] > x@range["max"])
     return("Error : plus group greater than oldest age")

  #Perform +grp calcs
  pg.range <- as.character(x@range["max"]:x@range["plusgroup"])

  #do the weighted stuff first
  for (i in pg.wt.mean){
     if (dims(n(x))$iter!=dims(slot(x,i))$iter)
         slot(x,i)<-propagate(slot(x,i),dims(n(x))$iter)
     slot(x,i)[as.character(x@range["plusgroup"])]<-quantSums(slot(x,i)[pg.range]*x@n[pg.range])/quantSums(x@n[pg.range])
     }
  x@n[as.character(x@range["plusgroup"])]<-quantSums(x@n[pg.range])

  x<-x[as.character(x@range["min"]:x@range["plusgroup"])]

  x@range["max"]<-x@range["plusgroup"]

  return(x)
  }
)# }}}

# fbar {{{
setMethod("fbar", signature(object="FLBiol"),
 function(object, minAge=dims(object)$min + 1, maxAge=dims(object)$max - 1, ...)
 {
  if (!("minfbar" %in% names(range(object))))
    range(object,"minfbar") <- minAge

  if (!("maxfbar" %in% names(range(object))))
    range(object,"maxfbar") <- maxAge

  if (is.na(object@range["minfbar"]))
    object@range["minfbar"] <- minAge

  if (is.na(object@range["maxfbar"]))
    object@range["maxfbar"] <- maxAge

  fbarRng<-range(object,"minfbar"):(range(object,"maxfbar")-1)

  res <- log(n(object)[ac(fbarRng),-dims(object)$year]/n(object)[ac(fbarRng+
    1),-1])-m(object)[ac(fbarRng),-dims(object)$year]

  res<-apply(res,c(2:6),mean)

  return(res)

  }
) # }}}

# catch.n {{{
setMethod("catch.n", signature(object="FLBiol"),
  function(object)
  {
    hrvst<-harvest(object)
    z <- hrvst+m(object)[,-dims(n(object))$year]
    res <- n(object)[,-dims(n(object))$year]*hrvst/z*(1-exp(-z))
    return(res)
   }
) # }}}

# propagate {{{
setMethod("propagate", signature(object="FLBiol"),
	function(object, iter, fill.iter=TRUE) {

    # FLQs
    res <- callNextMethod()

    # pMs
    for(i in c('mat', 'fec', 'rec'))
      slot(res, i) <- propagate(slot(object, i), iter, fill.iter=fill.iter)

    return(res)

  }) # }}}

# standardUnits {{{

#' @rdname standardUnits-methods
#' @details For objects derived of class *FLBiol* the adopted standard
#' units are: 'kg' for individual weights, '1000' for number of individuals,
#' 'm' for natural mortality, and an empty string for proportions (spwn, mat).
#' @examples
#' bio <- FLBiol(n=FLQuant(runif(50, 2, 120), dim=c(5, 10)))
#' # Object has no units
#' summary(bio)
#' # Obtain standard units for the class as a list
#' standardUnits(bio)
#' # which can then be assigned to the object
#' units(bio) <- standardUnits(bio)
#' summary(stk)

setMethod("standardUnits", signature(object="FLBiol"),
  function(object, ...) {

    standard <- list(n="1000", m="m", wt="kg", spwn="", mat="mat", fec="NA",
      rec="1000")

    args <- list(...)
    standard[names(args)] <- args

    return(standard)
    }
) # }}}

# units<-{{{

setReplaceMethod("units", signature(x="FLBiol", value="list"),
  function(x, value) {

    slt <- names(value)

    # FLQuant
    for(i in slt[slt %in% c("n", "m", "wt", "spwn")])
      units(slot(x, i)) <- value[[i]]

    # predictModel

    return(x)
  }
) # }}}

# deviances @rec {{{

setMethod("deviances", signature(object="FLBiol"),
  function(object) {
    res <- sr(object)[["deviances"]]

    if(!is.null(res))
      return(res)
    else
      return(rec(object) %=% 1)
  }
) 

setReplaceMethod("deviances", signature(object="FLBiol", value="FLQuant"),
  function(object, value) {
    
    sr(object)[["deviances"]] <- value

    return(object)
  }
)

# }}}

# metrics {{{

#' @rdname metrics
#' @examples
#' data(ple4.biol)
#' # Get default metrics
#' metrics(ple4.biol)
#' # Adds to defaults
#' metrics(ple4.biol, PG=function(x) n(x)[10,])
#' # Defines metrics to be computed
#' metrics(ple4.biol, metrics=list(PG=function(x) n(x)[10,]))

setMethod("metrics", signature(object="FLBiol", metrics="missing"),
  function(object, ...) {

    # HACK for some method dispatch problem
    foo <- selectMethod("metrics", c(object="FLBiol", metrics="list"))

    return(foo(object=object, metrics=c(list(R=rec, B=tsb), list(...))))
  }
)

# }}}
