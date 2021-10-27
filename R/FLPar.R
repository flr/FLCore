# FLPar - common structure for parameter matrices of various types.
# FLCore/R/FLPar.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

# Constructors  {{{

# FLPar(array)
setMethod('FLPar', signature(object="array"),
  function(object, params=letters[1:dim(object)[1]],
    iter=seq(dim(object)[length(dim(object))]), units=rep('NA', dim(object)[1]),
    dimnames= c(list(params=params), lapply(as.list(dim(object)[-c(1,
      length(dim(object)))]), seq), list(iter=iter))) {

    # if no dimnames, 1st is params, last is iter
    if(!is.null(dimnames(object))) {
      dimnames <- dimnames(object)
      
      # if iter missing, add it
      if(!any(names(dimnames) == "") & !'iter' %in% names(dimnames))
      {
        dimnames <- c(dimnames, list(iter=1))
        object <- array(object, dimnames=dimnames,
          dim=c(unlist(lapply(dimnames, length))))
      }
      
      # dimnames with no names, last one is iter ...
      if(names(dimnames)[length(dimnames)] == "")
        names(dimnames)[length(dimnames)] <- 'iter'
      # ... others are dim*
      if(any(names(dimnames) == ""))
        names(dimnames)[names(dimnames) == ""] <-
          paste('dim', seq(sum(names(dimnames) == "")))

      # forcing iter to be last dim, all others as given
      iterpos <- match(c('iter'), names(dimnames))
      object <- aperm(object,
        c(seq(1, length(dimnames))[!seq(1,length(dimnames)) %in% iterpos],
        iterpos))
    }
    if(!is.numeric(object))
      mode(object) <- 'double'
    res <- array(object, dim=dim(object), dimnames=dimnames)
    return(new('FLPar', res, units=units))
  }
)
 
# FLPar(missing, iter, param)
setMethod('FLPar', signature(object="missing"),
  function(..., params=character(1), iter=1, units='NA') {

    args <- list(...)

    # NO args, but iter and/or units
    if(length(args) == 0) {
      return(FLPar(array(as.numeric(NA), dim=c(length(params), iter),
        dimnames=list(params=params, iter=seq(iter))), units=units))
    }
   
    # TREAT all inputs as vectors
    args <- lapply(args, c)

    # WARN if iter forces extending params
    its <- unlist(lapply(args, length))
    if(iter > 1 && any(its[its > 1] != iter))
      warning("params with iterations will be extended to 'iter' length")

    # CREATE dimnames, iter as max(length(args), iter)
    dmns <- list(params=names(args), iter=seq(max(c(iter, its))))
    dm <- unlist(lapply(dmns, length))

    # EXTEND by iter
    args <- lapply(args, rep, length.out=dm[2])

    res <- array(do.call(rbind, args), dim=unname(dm), dimnames=dmns)
    return(FLPar(res, units=units))
  }
)

# FLPar(vector)
setMethod('FLPar', signature(object='vector'),
  function(object, params= if(length(names(object))==length(object))
    names(object) else letters[seq(length(object)/length(iter))], iter=1,
    dimnames=list(params=params, iter=seq(iter)), byrow=FALSE,
    units=rep('NA', dim(res)[1]))
  {
    # if length(iter) == 1, then expand
    if(length(iter) == 1 && as.character(iter) != '1')
      iter <- seq(iter)

    res <- array(object, dim=unlist(lapply(dimnames, length)))
    # FLPar(array)
    return(FLPar(res, units=units, dimnames=dimnames))
  }
)

# FLPar(FLPar)
setMethod('FLPar', signature('FLPar'),
  function(object, dimnames=attr(object, 'dimnames'), params=dimnames$params,
    iter=dimnames$iter, units=object@units, newDim="missing")
  {
    
    # get iter as vector if single number given
    if(!missing(iter) && length(iter) == 1 && ac(iter) != '1')
      iter <- ac(seq(as.numeric(iter)))

    dimnames$params <- params
    dimnames$iter <- ac(iter)
    res <- FLPar(as.numeric(NA), dimnames=dimnames, units=units)
    
    # select target dimnames and change names for '[<-'
    dimnames <- dimnames(object)
    names(dimnames) <- letters[seq(9,length=length(dimnames))]
    
    res2=do.call('[<-', c(list(res), dimnames, list(value=object)))
    
    if (missing(newDim))
      return(res2)
    
    dimnames <- dimnames(res2)
    ord <- length(dimnames)
    ord <- c(1:(ord-1),ord+1:length(newDim),ord)
    dimnames[names(newDim)] <- newDim
    res3  <- FLPar(rep(c(res2), length(unlist(newDim))),
      dimnames=dimnames[ord],units=units)
 
    return(res3)
  }
) # }}}

# [   {{{
#' @rdname Extract
#' @aliases [,FLPar,ANY,ANY,ANY-method
setMethod('[', signature(x='FLPar'),
  function(x, i, j, k, l, m, n, ..., drop=FALSE) {

    dx <- lapply(as.list(dim(x)), seq_len)
    names(dx) <- letters[seq(9, length=length(dx))]

    # EXTRACT non-standard attributes
    attrs <- attributes(x)
    attrs <- attrs[!names(attrs) %in% c("dim", "dimnames", "units", "class")]
      
			ldx <- length(dim(x))
			
			# PARSE dims
			for(ds in names(dx)) {
				# MISSING arg
				if(!do.call(missing, list(x=ds)))
					dx[[ds]] <- get(ds)
				}
			
			if(drop) {
				return(do.call('[', c(list(x=x@.Data), dx, list(drop=TRUE))))
			}
    res <- new(class(x), do.call('[', c(list(x=x@.Data), dx, list(drop=FALSE))))
    if(!missing(i))
      units(res) <- units(x)[ifelse(is.numeric(i), i, match(i, dimnames(x)$params))]
    else
      units(res) <- units(x)

    # Add attributes not in standard object   
    if(length(attrs) > 0) {
      for(i in names(attrs)) {
        attr(res, i) <- attrs[[i]]
      }
    }
    return(res)
  }
)

#' @rdname Extract
#' @aliases [,FLPar,array,missing,missing-method
setMethod('[', signature(x='FLPar', i='array', j='missing', drop='missing'),
  function(x, i) {
    return(x@.Data[i])
  }
) # }}}

# [<-     {{{
#' @rdname Extract
#' @aliases [<-,FLPar,ANY,ANY,ANY-method
setMethod("[<-", signature(x="FLPar", value="ANY"),
  function(x, i, j, k, l, m, n, ..., value)
  {
    # SUBSET on i if array
    if(!missing(i) && is.array(i))
    {
      x@.Data[i] <- value
      return(x)
    }
    
    # EXTRACT non-standard attributes
    attrs <- attributes(x)
    attrs <- attrs[!names(attrs) %in% c("dim", "dimnames", "units", "class")]
    dx <- lapply(as.list(dim(x)), seq_len)
    
    names(dx) <- letters[seq(9, length=length(dx))]
    
    for(ds in names(dx)) {
      # MISSING arg
      if(!do.call(missing, list(x=ds)))
        dx[[ds]] <- get(ds)
    }
    
    res  <- new(class(x), do.call('[<-', c(list(x=x@.Data), dx, list(value=value))))
   
    # Add attributes not in standard object   
    if(length(attrs) > 0) {
      for(i in names(attrs)) {
        attr(res, i) <- attrs[[i]]
      }
    }
    return(res)
  }
)   # }}}

# iter, iter<-     {{{
setMethod("iter", signature(obj="FLPar"),
  function(obj, iter) {
    if(dim(obj)[length(dim(obj))] == 1)
      return(obj)
    else {
      lst <- list(x=obj, pos=iter)
      names(lst) <- c('x', letters[8+length(dim(obj))])
      return(do.call('[', lst))
    }
  }
)
setMethod("iter<-", signature(object="FLPar", value="FLPar"),
  function(object, iter, value)
  {
    lst <- list(x=object, pos=iter, value=value)
    names(lst) <- c('x', letters[8+length(dim(object))], 'value')
    object <- do.call('[<-', lst)
    return(object)
  }
) 

setMethod("iter<-", signature(object="FLPar", value="numeric"),
  function(object, iter, value)
  {
    lst <- list(x=object, pos=iter, value=value)
    names(lst) <- c('x', letters[8+length(dim(object))], 'value')
    object <- do.call('[<-', lst)
    return(object)
  }
)

# }}}

# summary   {{{
#' @rdname summary-methods
#' @aliases summary,FLPar-method
setMethod('summary', signature(object='FLPar'),
  function(object, title=TRUE, ...) {
    if(title)
      cat("An object of class \"", class(object), "\"\n\n", sep="")
    if(dim(object)[length(dim(object))] == 1)
      return(object)
    else
      return(apply(object@.Data, seq(dim(object))[-length(dim(object))], summary))
  }
)   # }}}

# units        {{{
#' @rdname units-FLCore
setMethod("units", signature(x="FLPar"),
  function(x)
    return(x@units)
) # }}}

# units<-      {{{
#' @rdname units-FLCore
setMethod("units<-", signature(x="FLPar", value="character"),
  function(x, value) {
    x@units <- value
    return(x)
  }
) # }}}

# as.data.frame     {{{
setMethod("as.data.frame", signature(x="FLPar"),
  function(x, row.names='col', optional=FALSE, drop=FALSE) {
    res <- as(x, 'data.frame')
    if(drop) {
      idx <- names(x)[dim(x) > 1]
      res <- res[, c(idx, 'data')]
    }
    return(res)
  }
)   # }}}

# iterMedians, Means & Vars {{{

#' @rdname dimSummaries
setMethod("iterMeans", "FLPar",
  function(x, na.rm=TRUE) {
    dim <- seq(length=length(dim(x)))
    apply(x, dim[-length(dim)], mean, na.rm=na.rm)
  }
)

#' @rdname dimSummaries
setMethod("iterMedians", "FLPar",
  function(x, na.rm=TRUE) {
    dim <- seq(length=length(dim(x)))
    apply(x, dim[-length(dim)], median, na.rm=na.rm)
  }
)

#' @rdname dimSummaries
setMethod("iterVars", "FLPar",
  function(x, na.rm=TRUE) {
    dim <- seq(length=length(dim(x)))
    apply(x, dim[-length(dim)], var, na.rm=na.rm)
  }
)

#' @rdname dimSummaries
setMethod("iterSums", "FLPar",
  function(x, na.rm=TRUE) {
    dim <- seq(length=length(dim(x)))
    apply(x, dim[-length(dim)], sum, na.rm=na.rm)
  }
)

# }}}

# coerce  {{{
setAs('FLPar', 'numeric',
  function(from)
  {
    res <- as.vector(from[1,])
    names(res) <- dimnames(from)$param
    
    return(res)
  }
)

setAs("FLPar", "list",
  function(from) {
    lst <- split(from@.Data, 1:nrow(from))
    names(lst) <- dimnames(from)[[1]]
    return(lst)
  }
)

setAs('FLQuant', 'FLPar',
  function(from)
  {
    # check quant(from) == 'params'
    if(quant(from) != 'params')
      stop("'quant' in FLQuant must be 'params'")

    # extract array with dims of length < 1 collapsed
    res <- from@.Data[,,,,,,drop=TRUE]

    res <- FLPar(res)

    if(validObject(res))
      return(res)
    else
      stop("created object is not valid, please check input")
  }
)

setAs('FLPar', 'FLQuant',
  function(from)
  {
    # extract array
    data <- from@.Data
    # and names
    names <- names(dimnames(data))

    # output FLQuant
    res <- FLQuant(quant='params', units=ifelse(all(units(from) == 'NA'),
      'NA', paste(units(from), collapse='_')))

    # reshape data for FLQuant dimnames
    idx <- match(names(res), names)
    idx <- idx[!is.na(idx)]
    data <- aperm(data, idx)


    # get dim and dimnames for FLQuant
    idx <- names(res) %in% names(dimnames(data))
    dim <- rep(1,6)
    dim[idx] <- dim(data)
    dnames <- dimnames(res)
    dnames[idx] <- dimnames(data)
  
    res@.Data <- array(data, dim=dim, dimnames=dnames)

    return(res)

  }
)


# }}}

# propagate {{{
setMethod("propagate", signature(object="FLPar"),
  function(object, iter, fill.iter=TRUE) {

    # RETURN object if iter == iters
    dob <- dim(object)

    if(iter == dob[length(dob)])
      return(object)

    # dimnames of input object
    dnames <- dimnames(object)
    dnames$iter <- seq(iter)
    
    # CREATE new object
    res <- FLPar(NA, dimnames=dnames)
    # PLACE content in iters(object)
    res[,,seq(dob[length(dob)])] <- object

    # fill.iter
    if(fill.iter == TRUE)
    {
      args <- list(x=res, iter=seq(iter)[-1], value=object)
      names(args)[2] <- letters[seq(9,
        length=length(dim(res)))][names(dimnames(res))=='iter']
      res <- do.call('[<-', args)
    }
    return(res)
  }
) # }}}

# dims       {{{
setMethod("dims", signature(obj="FLPar"),
  # Return a list with different parameters
  function(obj, ...) {
    dimnames(obj)
    names(obj)
    iter <- length(dimnames(obj)$iter)
    params <- dimnames(obj)$params
    return(list(iter=iter, params=params))
  }
)   # }}}

# names         {{{
#' @rdname names
#' @aliases names,FLPar-method
setMethod("names", signature(x="FLPar"),
  function(x)
    names(dimnames(x))
)
# }}}

# names<-         {{{
#' @rdname names
#' @aliases names<-,FLPar,character-method
setMethod("names<-", signature(x="FLPar", value="character"),
  function(x, value)
  {
    # last dim must be 'iter', as in validFLPar
    if(value[length(value)] != 'iter')
      stop("last dimension must be named 'iter'")
    
    names(dimnames(x)) <- value
    return(x)
  }
)
# }}}

# show     {{{
setMethod("show", signature(object="FLPar"),
  function(object) {
    ndim <- length(dim(object))
    cat("An object of class \"", as.character(class(object)), "\"\n", sep="")
    if(dim(object)[ndim] != 1)
      cat("iters: ", dim(object)[ndim],"\n\n")
    if(dim(object)[ndim] > 1)
    {
      v1 <- apply(object@.Data, 1:(ndim-1), median, na.rm=TRUE)
      v2 <- apply(object@.Data, 1:(ndim-1), mad, na.rm=TRUE)   
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
    }
    else
      v3 <- format(object@.Data, digits=3)
    
    print(array(v3, dim=dim(object)[1:(ndim-1)], dimnames=dimnames(object)[1:(ndim-1)]),
      quote=FALSE)

    cat("units: ", object@units, "\n")
  }
)   # }}}

# print {{{
setMethod("print", signature(x="FLPar"),
  function(x, reduced=FALSE){

    if(reduced) {
      if(length(dimnames(x)[['iter']]) == 1) {
        print(t(x@.Data), digits=3)
      } else {
        v1 <- apply(x@.Data, 1, median, na.rm=TRUE)
        v2 <- apply(x@.Data, 1, mad, na.rm=TRUE)
        v3 <- paste(format(v1, digits=3),"(", format(v2, digits=3), ")", sep="")
        names(v3) <- names(v1)
        print(v3, quote=FALSE)
      }
    } else {
      show(x)
      invisible(x)
    }
  }
) # }}}

# Arith    {{{

#' @rdname Arith-methods
setMethod("Arith", ##  "+", "-", "*", "^", "%%", "%/%", "/"
  signature(e1 = "FLPar", e2 = "FLPar"),
  function(e1, e2)
  {
    return(new('FLPar', callGeneric(e1@.Data, e2@.Data)))
  }
)

#' @rdname Arith-methods
#' @examples
#' # FLQuant and FLPar
#' flq / flp
setMethod("Arith", signature(e1 = "FLArray", e2 = "FLPar"),
  function(e1, e2) {
    # objects dims
    d1 <- dim(e1)
    d2 <- dim(e2)
    l2 <- length(d2)
    n1 <- names(dimnames(e1))
    n2 <- names(dimnames(e2))

    # iter of output
    it <- max(d1[6], d2[l2])

    # dims of length > 1 (except iter), must be in FLArray
    if(any(!(n2[-l2][d2[-l2] > 1]) %in% (n1[-6][d1[-6] > 1])))
      stop(paste("FLPar object cannot have dimensions of length > 1 not in",
        ac(class(e1))))

    # reshape FLPar
    m2 <- unique(c(match(n2[-length(d2)], n1[-6]), seq(1, 6)))
    m2 <- m2[!is.na(m2)]

    e2 <- aperm(array(e2, dim=d1[m2]), m2)

    # iters from FLQ
    if(d1[6] >= d2[l2]) {
      return(new(class(e1), array(callGeneric(e1@.Data, e2), dim=d1,
        dimnames=dimnames(e1)), units=units(e1)))
    } else {
      return(new(class(e1), array(callGeneric(array(e1@.Data, dim=c(c(d1[-6], it))),
        array(e2, dim=c(d1[-6], it))), dim=c(d1[-6], it),
        dimnames=c(dimnames(e1)[-6], list(iter=seq(it)))), units=units(e1)))
    }
  }
)

#' @rdname Arith-methods
setMethod("Arith", signature(e1 = "FLPar", e2 = "FLArray"),
  function(e1, e2) {
    # objects dims
    d1 <- dim(e1)
    d2 <- dim(e2)
    l1 <- length(d1)
    n1 <- names(dimnames(e1))
    n2 <- names(dimnames(e2))

    # iter of output
    it <- max(d1[l1], d1[6])

    # dims of length > 1 (except iter), must be in FLArray
    if(any(!(n1[-l1][d1[-l1] > 1]) %in% (n2[-6][d2[-6] > 1])))
      stop(paste("FLPar object cannot have dimensions of length > 1 not in",
        ac(class(e2))))

    # reshape FLPar
    m1 <- unique(c(match(n1[-length(d1)], n2[-6]), seq(1, 6)))
    m1 <- m1[!is.na(m1)]

    e1 <- aperm(array(e1, dim=d2[m1]), m1)

    # iters from FLQ
    if(d2[6] >= d1[l1]) {
      return(new(class(e2), array(callGeneric(e1, e2@.Data), dim=d2,
        dimnames=dimnames(e2)), units=units(e2)))
    } else {
      return(new(class(e2), array(callGeneric(e1, e2@.Data), dim=c(d2[-6], it),
        dimnames=c(dimnames(e2)[-6], list(iter=seq(it)))), units=units(e2)))
    }
  }
)



 
 # }}}

# ab {{{
setMethod('ab', signature(x='FLPar', model='character'),
  function(x, model, spr0=NULL)
  {
    # input params and default values
    args <- c(as(x, 'list'), as.list(spr0))
    names(args)[length(args)] <- "spr0"
    args['model'] <- model

    res <- do.call('abPars', args)

    # get back c and d and spr0
    cd <- args[c('c', 'd', 'spr0')]
    res <- c(res, unlist(cd[!unlist(lapply(cd, is.null))]))

    return(do.call('FLPar', res))
  })


setMethod('ab', signature(x='FLPar', model='formula'),
  function(x, model, spr0=NULL)
  {
    model <- SRModelName(model)
    if(is.null(model))
      stop("model provided has not been identified")
    else
      return(ab(x, model))
  })# }}}

# sv {{{
setMethod('sv', signature(x='FLPar', model='character'),
  function(x, model, spr0)
  {
    # input params and default values
    args <- c(as(x, 'list'), as.list(spr0))
    names(args)[length(args)] <- "spr0"
    args['model'] <- model

    res <- do.call('svPars', args)
    # get back c and d
    cd <- args[c('c', 'd')]
    res <- c(res, unlist(cd[!unlist(lapply(cd, is.null))]))

    return(do.call('FLPar', res))
  })

setMethod('sv', signature(x='FLPar', model='formula'),
  function(x, model, spr0)
  {
   model <- SRModelName(model)
   if(is.null(model))
      stop("model provided has not been identified")
    else
     return(sv(x, model=model, spr0=spr0))
  })# }}}

# sweep {{{
#' @rdname sweep-methods
setMethod('sweep', signature(x='FLPar'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    res <- callNextMethod()
    do.call(class(x), list(res, units=units(x)))
  }
) # }}}

# apply {{{
#' @rdname apply-methods
setMethod('apply', signature(X='FLPar'),
  function(X, MARGIN, FUN, ...)
  {
    res <- callNextMethod()
    do.call(class(X), list(res, units=units(X)))
  }
) # }}}

# rbind2 {{{
setMethod('rbind2', signature(x='FLPar', y='FLPar'),
  function(x, y, ...) {
    
    args <- c(list(x=x, y=y), list(...))
    
    # FLPars dimensions and dimnames
    dimar <- lapply(args, dim)
    nrow <- sum(unlist(lapply(dimar, `[`, 1)))
    dnms <- lapply(args, dimnames)
    
    # CHECK all have same length(dims)
    if(length(unique(unlist(lapply(dimar, length)))) != 1)
      stop("FLPar object to rbind must share number of dimensions")

    # CHECK all have same dim[-1]
    dms <- apply(matrix(unlist(dimar), byrow=TRUE, nrow=length(args)), 2,
      function(x) length(unique(x)))
    if(any(dms[-1] != 1))
      stop("FLPar object to rbind must share number of dimensions except params")

    # CHECK names(dimnames) match
    if(!all.equal(Reduce(intersect, lapply(dnms, names)), names(dnms[[1]])))
       stop("Names of dimnames must match")

    # TODO
    # CHECK dimnames[-c(1, iter)] match
    #onms <- lapply(dnms, `[`, -c(1, length(dimar[[1]])))
    #for(i in names(onms[[1]])) {
    #  if(length(unique(unlist(lapply(onms, `[[`, i)))) > 1)
    #    stop(paste0("dimnames across objects must match for dimension '", i, "'"))
    #}

    # params names
    nargs <- lapply(lapply(args, dimnames), `[`, 1)

    # aperm to join by row
    idx <- c(seq(2, length(dimar[[1]])), 1)
    pargs <- lapply(args, aperm, idx)
    # then re-aperm
    idx <- c(length(dimar[[1]]), seq(1, length(dimar[[1]]) - 1))
    res <- aperm(array(unlist(pargs), dim=c(dimar[[1]][-1], nrow)), idx)

    # dimnames
    dimnames(res) <- c(list(params=unlist(nargs, use.names=FALSE)), dimnames(x)[-1])

    res <- FLPar(res, units=units(args[[1]]))

    # keep names of x
    names(res) <- names(x)
   
    return(res)
  }
) # }}}

# cbind2 {{{
setMethod('cbind2', signature(x='FLPar', y='FLPar'),
  function(x, y, ...) {
    
    args <- c(list(x=x, y=y), list(...))
    
    idx <- unlist(lapply(args, is, 'FLPar'))
    if(!all(idx))
      stop("input objects must all be of class 'FLPar'")

    res <- args[[1]]@.Data
    if(length(args) > 1)
      for (i in seq(length(args))[-1])
        res <- cbind(res, args[[i]]@.Data)
    
    # dimnames
    names(dimnames(res)) <- names(dimnames(args[[1]]))
    # correct for iter dimnames
    dimnames(res)$iter <- seq(length(dimnames(res)$iter))

    if(any(unlist(lapply(dimnames(res), function(x) any((x==x[1])[-1])))))
      warning("Repeated dimnames in output FLPar")
   
    return(FLPar(res, units=units(args[[1]])))
  }
) # }}}

# rlnorm {{{
setMethod("rlnorm", signature(n='numeric', meanlog="FLPar", sdlog="FLPar"),
  function(n=1, meanlog, sdlog) {
    if(all(dim(meanlog) != dim(sdlog)))
      stop("dims of meanlog and sdlog must be equal")

    lastdim <- length(dim(meanlog))
    
    FLPar(array(rlnorm(prod(dim(meanlog)[-lastdim])*n,
      rep(iter(meanlog, 1)[drop=TRUE], n),
      rep(iter(sdlog, 1)[drop=TRUE],n)),
        dim=c(dim(meanlog)[-lastdim], n)),
      dimnames=c(dimnames(meanlog)[-lastdim], list(iter=seq(n))))
  }
)

setMethod("rlnorm", signature(n='numeric', meanlog="FLPar", sdlog="numeric"),
  function(n=1, meanlog, sdlog) {
    rlnorm(n, meanlog, FLPar(sdlog, dimnames=dimnames(meanlog)))
  }
)

setMethod("rlnorm", signature(n='numeric', meanlog="numeric", sdlog="FLPar"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, FLPar(meanlog, dimnames=dimnames(sdlog)), sdlog)
)

setMethod("rlnorm", signature(n='numeric', meanlog="FLPar", sdlog="missing"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, meanlog, 1)
)

setMethod("rlnorm", signature(n='numeric', meanlog="missing", sdlog="FLPar"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, 0, sdlog)
)

setMethod("rlnorm", signature(n='FLPar', meanlog="ANY", sdlog="ANY"),
  function(n, meanlog=0, sdlog=1) {
    FLPar(rlnorm(length(n), meanlog, sdlog), dimnames=dimnames(n))
  }
)

# }}}

# rnorm {{{
setMethod("rnorm", signature(n='numeric', mean="FLPar", sd="FLPar"),
  function(n=1, mean, sd) {
    if(all(dim(mean) != dim(sd)))
      stop("dims of mean and sd must be equal")

    lastdim <- length(dim(mean))
    
    FLPar(array(rnorm(prod(dim(mean)[-lastdim])*n,
      rep(iter(mean, 1)[drop=TRUE], n),
      rep(iter(sd, 1)[drop=TRUE],n)),
        dim=c(dim(mean)[-lastdim], n)),
      dimnames=c(dimnames(mean)[-lastdim], list(iter=seq(n))))
  }
)

setMethod("rnorm", signature(n='numeric', mean="FLPar", sd="numeric"),
  function(n=1, mean, sd) {
    rnorm(n, mean, FLPar(sd, dimnames=dimnames(mean)))
  }
)

setMethod("rnorm", signature(n='numeric', mean="numeric", sd="FLPar"),
  function(n=1, mean, sd)
    rnorm(n, FLPar(mean, dimnames=dimnames(sd)), sd)
)

setMethod("rnorm", signature(n='numeric', mean="FLPar", sd="missing"),
  function(n=1, mean, sd)
    rnorm(n, mean, 1)
)

setMethod("rnorm", signature(n='numeric', mean="missing", sd="FLPar"),
  function(n=1, mean, sd)
    rnorm(n, 0, sd)
)

setMethod("rnorm", signature(n='FLPar', mean="ANY", sd="ANY"),
  function(n, mean=0, sd=1) {
    FLPar(rnorm(length(n), mean, sd), dimnames=dimnames(n))
  }
)

# }}}

# mvrnorm {{{
setMethod("mvrnorm", signature(n="numeric", mu="FLPar", Sigma="matrix",
  tol="missing", empirical="missing", EISPACK="missing"),
  function(n, mu, Sigma) {
    
    dm <- dim(mu)
    dnm <- dimnames(mu)
    
    # Check that params second dim is "iter"
    if(names(dnm)[2]!="iter")
      stop("To apply this method params must have 2 dimensions only and the
        second has to be \"iter\".")  

    # Check dims
    if(dm[2] > 1)
      stop("mu FLPar cannot have iterations")

    res <- do.call("mvrnorm", list(mu=c(mu), Sigma=Sigma, n=n))
    
    if(n>1)
      res <- t(res)
    else
      res <- matrix(res, ncol=1)

    dnm$iter <- 1:n
    dimnames(res) <- dnm
    res <- FLPar(res)
    units(res) <- units(mu)

    return(res)
  }
) # }}}

# model.frame {{{
setMethod("model.frame", signature(formula="FLPar"),
  function(formula, ...) {
    
    dmn <- dim(formula)

    # extract array
    res <- formula@.Data

    # shape into matrix (no. params, all other dims)
    dim(res) <- c(dmn[1], prod(dmn[-1]))

    # rotate and data.frame
    res <- as.data.frame(t(res))

    # add params names
    names(res) <- dimnames(formula)[[1]]

    # add other cols
    res <- cbind(res, expand.grid(dimnames(formula)[-1]))

    # make year numeric
    if("year" %in% names(res))
      res$year <- as.numeric(res$year)

    return(res)
  }
) # }}}

# $ {{{
#' @rdname Extract
#' @aliases $,FLPar-method
setMethod('$', signature(x='FLPar'),
  function(x, name) {
    return(x[name,])
  }
) 

#' @rdname Extract
#' @aliases $<-,FLPar,ANY-method
setReplaceMethod("$", signature(x="FLPar", value="ANY"),
  function(x, name, value) {
    # SUBSTITUTE existing param
    if(name %in% dimnames(x)$params) {
      x[name,] <- value
    # or ADD new one
    } else {
      value <- FLPar(value)
      dimnames(value)$params <- name
      x <- rbind(x, value)
    }
    return(x)
  }
) 


# }}}

# expand {{{
setMethod('expand', signature(x='FLPar'),
  function(x, ...) {

    args <- list(...)
    dnx <- dimnames(x)
    
    # dimension names
    nargs <- names(args)
    qnames <- names(dnx)
    
    # check input names match dimnames
    if(!all(nargs %in% qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # turn into characters
    select <- lapply(args, as.character)
    
    # match specified dimensions and dimnames
    dimnames <- dnx
    
    # new dimnames
    dimnames[names(select)] <- select

    # output object
    res <- new(class(x), array(as.numeric(NA), dimnames=dimnames,
      dim=unlist(lapply(dimnames, length))), units=units(x))
    
    # assigment list by position to avoid "" dimnames
    dnx <- setNames(lapply(as.list(dim(x)), seq),
      c('i', 'j', 'k', 'l', 'm', 'n')[seq(length(dim(res)))])
    
    return(do.call('[<-', c(list(x=res, value=x), dnx)))
  }
) # }}}

# window           {{{
setMethod("window", signature(x="FLPar"),
  function(x, start=as.numeric(dimnames(x)$year[1]),
    end=as.numeric(dimnames(x)$year[length(dimnames(x)$year)]), extend=TRUE,
    frequency=1) {
    #
    if(!"year" %in% names(x))
      stop("window can only be called to objects with a 'year' dimension")

    # get original min and max
    yrs <- dimnames(x)$year
    min <- as.numeric(yrs[1])
    max <- as.numeric(yrs[length(yrs)])
    pos <- match("year", names(x))

    # if extend=FALSE and end/start ask for it, error
    if(!extend && (start < min | end > max))
      stop("FLPar to be extended but extend=FALSE")

    # if extend is a number, added to end
    if(is.numeric(extend))
        if (missing(end))
          end <- max + extend
        else
          stop("'extend' is numeric and 'end' provided, don't know what to do")
    
    # construct new FLPar
    dnames <- dimnames(x)
    dnames[[pos]] <- seq(start, end, by=frequency)
    res <- do.call(class(x), list(NA, units=units(x), dimnames=dnames))

    # add data for matching years
    dnames <- dnames[pos]
    names(dnames) <- c('i', 'j', 'k', 'l', 'm', 'n')[pos]

    do.call('[<-', c(list(x=res, value=x), dimnames))
  }
)   # }}}

# divide {{{

#' @example
#' divide(FLPar(FMSY=0.21, BMSY=120012), names=c(F="FMSY", B="BMSY"))

setMethod("divide", signature(object="FLPar"),
  function(object, dim=1, names=setNames(nm=dimnames(object)[[dim]])) {
  return(FLPars(lapply(names, function(x) object[x,])))
  }
)

# }}}

