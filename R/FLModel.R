# FLModel - Extendable class for all types of models to be fitted and analysed
# FLCore/R/FLModel.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# FLModel  {{{
validFLModel <- function(object)
{
  # All FLArray slots are of the same exact class
  flarr <- getSlotNamesClass(object, 'FLArray')
  class <- class(slot(object, flarr[1]))
  for(i in flarr[-1])
    if(class(slot(object, i)) != class)
      return(paste('FLQuant/FLCohort slots in object should all be of the same class: ',
        i))
  return(TRUE)
}
setClass('FLModel',
  representation('FLComp',
    model='formula',
    logl='function',
    grad='function',
    initial='function',
    params='FLPar',
    logLik='logLik',
    vcov='array',
    details='list',
    residuals='FLArray',
    fitted='FLArray'),
  prototype(name=character(0),
    desc=character(0),
    range=unlist(list(min=NA, max=NA, minyear=1, maxyear=1)),
    model=formula(NULL),
    fitted=FLQuant(),
    residuals=FLQuant())
)
invisible(createFLAccesors("FLModel", exclude=c('name', 'desc', 'range', 'params')))  # }}}

# FLModel()  {{{
setGeneric('FLModel', function(model, ...)
    standardGeneric('FLModel'))

setMethod('FLModel', signature(model='missing'),
  function(..., class='FLModel')
  {
    do.call('FLModel', c(list(model=formula(NULL), class=class), list(...)))
  }
)

setMethod('FLModel', signature(model='character'),
  function(model, class='FLModel', ...)
  {
    modelst <- do.call(model, list())
    do.call('FLModel', c(modelst, list(...), list(class=class)))
  }
)

setMethod('FLModel', signature(model='function'),
  function(model, class='FLModel', ...)
  {
    modelst <- do.call(model, list())
    do.call('FLModel', c(modelst, list(...), list(class=class)))
  }
)

setMethod('FLModel', signature(model='formula'),
  function(model, class='FLModel', ...)
  {
    # new object
    res <- new(class)
    slot(res, 'model') <- model

    # args
    args <- list(...)
    for (i in names(args))
      slot(res, i) <- args[[i]]

    # size other FLArray slots
    slots <- getSlotNamesClass(res, 'FLArray')
    
    if(any(slots %in% names(args)))
    {
      # use output element if given
      if(length(model) > 1 && as.character(as.list(model)[[2]]) %in% names(args))
      {
        first <- as.character(as.list(model)[[2]])
      }
      #otherwise first argument of right class
      else
      {
        first <- names(args)[names(args) %in% slots][1]
      }
      
      # create 'empty' FLArray
      empty <- args[[first]]
      empty[] <- as.numeric(NA)
      
      # modify as in 'empty'
      for(s in slots[!slots %in% names(args)])
        slot(res, s) <- empty
    
      # range
      dempty <- dims(empty)
      rnames <- dempty[grep('min', names(dempty))]
      rnames <- c(rnames, dempty[grep('max', names(dempty))])
      res@range <- unlist(rnames)
    }

    # params
    params <- getFLPar(res, formula=model)
    res@params <- params

    # logLik is created, as is a virtual class
    loglik <- as.numeric(NA)
    class(loglik) <- 'logLik'
    slot(res, 'logLik') <- loglik

    return(res)
  }
) # }}}

# logLik  {{{
setReplaceMethod('logLik', signature(object='FLModel', value='numeric'),
  function(object, value, df='missing', nall='missing', nobs='missing')
  {
    # check length
    if(length(value) > 1)
      stop('value must be of length 1')

    attr(value, 'class') <- 'logLik'
    if(!missing(df))
      attr(value, 'df') <- df
    if(!missing(nall))
      attr(value, 'nall') <- nall
    if(!missing(nobs))
      attr(value, 'nobs') <- nobs
    slot(object, 'logLik') <- value
    return(object)
  }
)  # }}}
  
# coef - same as params  {{{
if (!isGeneric("coef"))
  setGeneric('coef', useAsDefault = coef)
setMethod('coef',
  signature(object='FLModel'),
  function(object, ...)
  {
    return(object@params)
  }
)  # }}}

# fmle()    {{{
setGeneric('fmle', function(object, start, ...)
    standardGeneric('fmle'))

setMethod('fmle',
  signature(object='FLModel', start='FLPar'),
  function(object, start, method='L-BFGS-B', fixed=list(),
    control=list(trace=1), lower=rep(-Inf, dim(params(object))[2]),
    upper=rep(Inf, dim(params(object))[2]), ...)
  {
    values <- as.list(iter(start,1))
    names(values) <- dimnames(start)$params

    fmle(object, values, method, fixed, control, lower, upper, ...)
  }
)
setMethod('fmle',
  signature(object='FLModel', start='ANY'),
  function(object, start, method='L-BFGS-B', fixed=list(),
    control=list(trace=1), lower=rep(-Inf, dim(params(object))[2]),
    upper=rep(Inf, dim(params(object))[2]), seq.iter=TRUE, ...)
  {
    # TODO Check with FL
    call <- sys.call(1)
    logl <- object@logl
    
    # get parameter names by matching elements in param slot
    parnm <- names(formals(logl))[names(formals(logl))%in%
      dimnames(object@params)$param]
    
    # get fixed parameter names
    fixnm <- names(fixed)
    # fixed must match params
    if(any(!fixnm %in% parnm))
      stop("some named arguments in 'fixed' are not arguments to the
        supplied log-likelihood")

    # create list of input data
    #   get FLQuant slots' names
    datanm <- getSlotNamesClass(object, 'FLArray')
    datanm <- c(datanm, getSlotNamesClass(object, 'numeric'))
    #   get those in formals of logl
    datanm <- datanm[datanm%in%names(formals(logl))]

    # limits
    if(method == 'L-BFGS-B')
    {
      if(missing(lower) && !is.null(lower(object)))
        lower <- lower(object)[match(parnm, names(fixed), nomatch=0)==0]
      if(missing(upper) && !is.null(upper(object)))
        upper <- upper(object)[match(parnm, names(fixed), nomatch=0)==0]
    }
    else
    {
      lower <- -Inf
      upper <- Inf
    }

    # grad function
    if(!is.null(body(object@grad)))
      gr <- object@grad
    else
      gr <- NULL

    # create logl function
    loglfoo <- function(par) {
      params <- as.list(par)
      names(params) <- names(start)
      params[fixnm] <- fixed
      return(-1*(do.call(logl, args=c(params, data))))
    }

    # iterations
    if(seq.iter)
      iter <- dim(slot(object, datanm[1]))[6]
    else
      iter <- 1

    # logLik
    logLik <- rep(NA, iter)
    class(logLik) <- 'logLik'
    attr(logLik, 'df') <- length(parnm) - length(fixed)
    object@logLik <- logLik

    # Correct FLPar, fitted and residuals
    if(iter > dim(object@params)[1])
    {
      params(object) <- FLPar(iter=iter, params=dimnames(object@params)$params)
    }

    fitted(object) <- propagate(fitted(object), iter)
    residuals(object) <- propagate(residuals(object), iter)

    # vcov
    object@vcov <- array(NA, dim=c(rep(length(parnm)-length(fixed),2), iter),
      dimnames=list(parnm[!parnm%in%names(fixed)],parnm[!parnm%in%names(fixed)],
      iter=1:iter))

    # input data
    alldata <- list()
    for (i in datanm)
      alldata[[i]] <- slot(object, i)

    for (it in 1:iter)
    {
      # data
      if(seq.iter)
        data <- lapply(alldata, iter, it)
      else
        data <- alldata
      
      # check values
      toolarge <- names(data)[unlist(lapply(data, max, na.rm=TRUE))>10000]
      if(length(toolarge) > 0)
        warning(paste("Values might be too large for optimizer in ",
          paste(toolarge, collapse=", ")))
    
      # add covar if defined and available
      if('covar' %in% slotNames(object))
      {
        covarnm <- names(object@covar)
        covarnm <- covarnm[covarnm%in%names(formals(logl))]
        if(length(covarnm))
          data <- c(data, covar(object)[covarnm])
      }

      # start values
      if(missing(start))
      # add call to @initial
      if(is.function(object@initial))
        start <- do.call(object@initial, args=data[names(formals(object@initial))])
      else
        start <- formals(logl)[names(formals(logl))%in%parnm]
      if(!is.null(fixnm))
        start[fixnm] <- NULL
      if(any(!names(start) %in% parnm))
        stop("some named arguments in 'start' are not arguments to the
          supplied log-likelihood")
      start <- start[order(match(names(start), parnm))]

      if(is.null(start))
        stop("No starting values provided and no initial function available")

      # TODO protect environment
      out <- do.call('optim', c(list(par=unlist(start), fn=loglfoo, method=method,
        hessian=TRUE, control=control, lower=lower, upper=upper, gr=gr, ...)))
      
      # output
      # place out$par in right iter dim
      iter(object@params[names(out$par),], it) <- out$par
      # fixed
      if(length(fixed) > 0)
        iter(object@params, it)[fixnm,] <- unlist(fixed)
      # TODO make details list of lists if iter > 1?
      object@details <- list(call=call, value=out$value, count=out$counts, 
        convergence=out$convergence, message=out$message)  
      # vcov & hessian
      coef <- out$par
      object@vcov[,,it] <-
        if (length(coef))
        {
          if(det(out$hessian) != 0)
          {
            tmphess <- try(solve(out$hessian))
            if(class(tmphess) =='try-error')
            {
              matrix(numeric(0), length(coef), length(coef), dimnames=list(names(coef),
                names(coef)))
            } else
            tmphess
          } else
            0
        } else
          0
      
      # logLik
      object@logLik[it] <- -out$value
      attr(object@logLik, 'nobs') <- length(data[[1]])
      
      # fitted & residuals
      iter(fitted(object), it) <- predict(iter(object, it))
      iter(residuals(object), it) <- log(iter(slot(object,
        as.list(object@model)[[2]]),it) / iter(fitted(object), it))
    }
    # force dimnames[1:5] in 'fitted' and 'residuals' to match
    dimnames(fitted(object))[1:5] <- dimnames(do.call(as.character(
      as.list(object@model)[2]), list(object)))[1:5]
    dimnames(residuals(object)) <- dimnames(fitted(object))

    # return object
    return(object)
  }
)   # }}}

# predict   {{{
if (!isGeneric("predict"))
  setGeneric('predict', useAsDefault = predict)
setMethod('predict', signature(object='FLModel'),
  function(object, ...)
  {
    args <- list(...)
    if(length(args) > 0 && is.null(names(args)))
      stop('FLQuant or FLCohort inputs must be named to apply formula')
    # call
    call <- as.list(object@model)[[3]]

    # check vars in call match input in args
    if(length(args) > 0 & !any(names(args)%in%all.vars(call)))
      warning(paste("Input names do not match those in model formula: '",
        paste(names(args)[!names(args)%in%all.vars(call)], collapse=','), "'", sep=""))
    
    # create list of input data
    #   get FLQuant/FLCohort slots' names
    datanm <- getSlotNamesClass(object, 'FLArray')
    datanm <- c(datanm, getSlotNamesClass(object, 'numeric'))

    # add dimnames if used
    dimna <- dimnames(slot(object, datanm[1]))[names(slot(object, datanm[1]))%in%
      all.vars(object@model)]
    # get them in the right shape
    dimdat <- lapply(dimna, function(x)
      {
        out <- slot(object, datanm[1])
        out[] <- as.numeric(x)
        return(out)
      })
    
    # iterations
    #   from object
    iter <- max(unlist(qapply(object, function(x) dims(x)$iter)))
    #   from extra input
    if(length(args) > 0)
    {
      iterarg <- lapply(args, function(x) {
        itera <- try(dims(x)$iter)
        if(class(iter) =='try-error')
          return(1)
        else
          return(itera)
      })
      iterarg <- max(unlist(iterarg))
    }
    else
      iterarg <- 1
    #   decision
    if (iter == iterarg)
      iters <- iter
    else if(iter > iterarg && iterarg == 1)
      iters <- iter
    else if(iterarg > iter && iter == 1)
      iters <- iterarg
    else
      stop("Iter for object and input arguments do not match")

    for (it in 1:iters)
    {
     obj <- iter(object, it)

      #   input data
      data <- list()
      for (i in datanm)
        data[[i]] <- slot(obj, i)

      # add covar if defined and available
      if('covar' %in% slotNames(obj))
      {
        covarnm <- names(obj@covar)
        if(length(covarnm))
          data <- c(data, covar(obj)[covarnm])
      }
  
      # add newdata
      data[names(args)] <- lapply(args, iter, it)

      params <- as.vector(obj@params@.Data)
      names(params) <- dimnames(obj@params)[['params']]
      # check inputs
      if(it == 1)
      {
        res <- propagate(eval(call, envir=c(params, data, dimdat)),
          iters, fill.iter=FALSE)
      }
      else
      {
        iter(res, it) <- eval(call, envir=c(params, data, dimdat))
      }
    }
    return(res)
  }
)   # }}}

# AIC & BIC   {{{
if (!isGeneric("AIC"))
  setGeneric('AIC', useAsDefault = stats::AIC)
setMethod('AIC', signature(object='FLModel', k='numeric'),
  function(object, k=2)
    return(AIC(object@logLik, k))
)
# AIC with RSS: 2k + n * ln(RSS/n)
setMethod('AIC', signature(object='FLModel', k='missing'),
  function(object)
    return(AIC(object@logLik))
)

if (!isGeneric("BIC"))
  setGeneric('BIC', useAsDefault = stats::BIC)
setMethod('BIC', signature(object='FLModel'),
  function(object)
    return(BIC(object@logLik))
)  # }}}

# nls   {{{
if (!isGeneric("nls"))
  setGeneric('nls', useAsDefault = nls)

setMethod('nls',
  signature(formula='FLModel', data='missing',  start='ANY',  
    control='ANY',  algorithm='ANY',  trace='ANY',  subset='ANY',
    weights='ANY',  na.action='ANY',  model='ANY',  lower='ANY',
    upper='ANY'),
  # TODO deal with arguments without default values
  function(formula, start, control=nls.control(), algorithm='default', trace=FALSE,
    subset, weights, na.action, model=FALSE, lower=attr(logl(formula), 'lower'),
    upper=attr(logl(formula), 'upper'), ...)
  {
    call <- sys.call(1)
    # create list of input data
    #   get FLQuant/FLCohort slots' names ...
    flarrnm <- getSlotNamesClass(formula, 'FLArray')
    #   ... and those of class 'numeric'
    numernm <- getSlotNamesClass(formula, 'numeric')
    datanm <- c(flarrnm, numernm)
    #   now get only those in formula
    datanm <- datanm[datanm%in%all.vars(formula@model)]
    # get names of parameters ...
    parnm <- all.vars(formula@model)[!all.vars(formula@model)%in%datanm]
    # ... leaving out dimnames
    parnm <- parnm[!parnm%in%names(dimnames(slot(formula, datanm[1])))]
    # and same thing for data names
    datanm <- datanm[!datanm%in%names(dimnames(slot(formula, datanm[1])))]
    flarrnm <- flarrnm[flarrnm%in%datanm]
    numernm <- numernm[numernm%in%datanm]

    # input data
    data <- list()
    for (i in datanm)
      data[[i]] <- slot(formula, i)

    # get content of FLQuants (covar)
    if('covar' %in% slotNames(formula))
    {
      covnm <- names(covar(formula))
      if(length(covnm) > 0)
        for (i in covnm)
          data[[i]] <- covar(formula)[[i]]
      datanm <- c(datanm, covnm)
    }

    # modeldata
    modeldata <- model.frame(FLlst(data[unlist(lapply(data, is, 'FLArray'))]))
    if(length(numernm > 0))
      modeldata <- merge(modeldata, data.frame(data[!unlist(lapply(data, is, 'FLArray'))]))

    # start values
    if(missing(start))
      if(is.function(formula@initial))
        start <- do.call(formula@initial, args=data)[parnm]
      else
        stop("No start values provided and no initial function in object")

    # nls
    out <- nls(formula@model, data=modeldata, control=control,
      algorithm=algorithm, trace=trace, model=model, start=start)
    # output
    formula@details <- list(call=call, convInfo=out$convInfo, control=out$control)
    formula@params[,] <- NA
    iter(formula@params, 1)[parnm,] <- out$m$getPars()
    formula@vcov <- vcov(out)
    formula@logLik <- logLik(out)
    formula@fitted <- predict(formula)
    formula@residuals <- log(formula@fitted/eval(as.list(formula@model)[[2]], data))
    
    # force dimnames[1:5] in 'fitted' and 'residuals' to match
    dimnames(fitted(formula))[1:5] <- dimnames(do.call(as.character(
      as.list(formula@model)[2]), list(formula)))[1:5]
    dimnames(residuals(formula)) <- dimnames(fitted(formula))

    return(formula)
  }
)   # }}}

# summary  {{{
setMethod('summary', signature(object='FLModel'), 
  function(object, ...)
  {
    callNextMethod()
    cat("\n")
    # covar, model, logl, params, logLik, FLQuants, details
    cat("Model: \t")
      print(slot(object, 'model'))
    if(is.null(slot(object, 'params')))
      cat("Parameters: EMPTY\n")
    else if(length(dimnames(slot(object, 'params'))['iter']) == 1) {
      cat("Parameters: \n")
        print(t(slot(object, 'params')@.Data), digits=4)
    } else {
      cat("Parameters (median): \n")
      v1 <- apply(object@params@.Data, 1, median, na.rm=TRUE)
      v2 <- apply(object@params@.Data, 1, mad, na.rm=TRUE)	 
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
      names(v3) <- names(v1)
      print(v3, quote=FALSE)
#        print(apply(slot(object, 'params')@.Data, 1, median), digits=4)
    }
    cat("\n")
    cat("Log-likelihood: ", paste(format(median(slot(object, 'logLik')), digits=5),
      "(", format(mad(slot(object, 'logLik')), digits=5), ")", sep=""),
      "\n")
    cat("Variance-covariance: ")
    if(all(dim(object@vcov) > 0))
    {
      if(length(dim(object@vcov)) == 3 && dim(object@vcov)[3] > 1)
      {
        v1 <- apply(object@vcov, 1:2, median, na.rm=TRUE)
        v2 <- apply(object@vcov, 1:2, mad, na.rm=TRUE)	 
        v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
      }
      else
        v3 <- apply(object@vcov, 1:2, median, na.rm=TRUE)
      m <- array(v3, dim=dim(object@vcov)[1:2], dimnames=dimnames(object@vcov)[1:2])
    }
    else
      m <- matrix(NA, ncol=nrow(slot(object, 'params')), nrow=nrow(slot(object, 'params')), dimnames=rep(dimnames(params(object))['params'],2))
    print(m, quote=FALSE)

  }
)  # }}}

# sd {{{
if (!isGeneric('sd'))
  setGeneric('sd', useAsDefault = sd)
setMethod('sd', signature(x='FLModel', na.rm='missing'),
  function(x)
  {
    if(dim(params(x))[1] == 1)
    {
      res <- as.vector(t(sqrt(apply(x@vcov, 3, diag))))
      names(res) <- dimnames(params(x))$params
      return(res)
    }
    else
      return(sd(params(x)))
  }
) # }}}

# cv {{{
setMethod('cv', signature(object='FLModel'),
  function(object, ...)
  {
    if(dim(params(object))[1] == 1)
      return(sd(object)/apply(params(object), 2, mean))

        #dimnames(vcov(object))[[1]]
    else
      return(sd(params(object))/mean(params(object)))
  }
) # }}}

# model<-   {{{
setReplaceMethod('model', signature(object='FLModel', value='character'),
  function(object, value)
  {
    modelst <- do.call(value, list())
    model(object) <- modelst
    return(object)
  }
)

setReplaceMethod('model', signature(object='FLModel', value='function'),
  function(object, value)
  {
    modelst <- do.call(value, list())
    model(object) <- modelst
    return(object)
  }
)
setReplaceMethod('model', signature(object='FLModel', value='list'),
  function(object, value)
  {
    for(i in names(value))
      slot(object, i) <- value[[i]]
    params(object) <- getFLPar(object)
    fitted(object) <- as.numeric(NA)
    residuals(object) <- as.numeric(NA)
    return(object)
  }
)
setReplaceMethod('model', signature(object='FLModel', value='formula'),
  function(object, value)
  {
    slot(object, 'model') <- value
    params(object) <- getFLPar(object)
    return(object)
  }
) # }}}

# update  {{{
if (!isGeneric("update"))
  setGeneric('update', useAsDefault = update)
setMethod('update', signature(object='FLModel'),
  function(object, ...)
  {
    call <- details(object)[['call']]
    if (is.null(call))
    	stop("need an object with call component")

    args <- list(...)
    for(i in names(args))
      slot(object, i) <- args[[i]]
    return(do.call(as.character(as.list(call)[[1]]), list(object)))
  }
) # }}}

# lm  {{{
if (!isGeneric("lm"))
  setGeneric('lm', useAsDefault = lm)
setMethod('lm', signature(formula='FLModel', data = "missing", subset = "missing",
  weights = "missing", na.action = "missing", method = "missing", model = "missing",
  x = "missing", y = "missing", qr = "missing", singular.ok = "missing",
  contrasts = "missing", offset = "missing"),
  function(formula, ...)
  {
    res <- lm(formula@model, model.frame(as(formula, 'FLlst')), ...)

    # params
    params(formula) <- FLPar(res$coefficients,
      params=c('a', paste('b', seq(length(res$coefficients)-1), sep='')))
    
    # residuals
    residuals(formula) <- FLQuant(res$residuals, dimnames=dimnames(fitted(formula)))

    # fitted
    fitted(formula) <- FLQuant(res$fitted.values, dimnames=dimnames(fitted(formula)))

    # details
    details(formula) <- list(call=res$call)

    return(formula)
  }
) # }}}

# getFLPar {{{
getFLPar <- function(object, formula=object@model)
{
  # get FLQuant slots' names
  datanm <- getSlotNamesClass(object, 'FLArray')
  datanm <- c(datanm, getSlotNamesClass(object, 'numeric'))
  datanm <- datanm[!datanm %in% c('residuals', 'fitted')]
  if(length(datanm) > 0)
    datanm <- c(datanm, names(dimnames(slot(object, datanm[1]))))

  # get those in formula
  datanm <- datanm[datanm%in%all.vars(formula)]
  parnm <- all.vars(formula)[!all.vars(formula) %in% datanm]

  # covar
  if('covar' %in% slotNames(object))
  {
    covarnm <- names(object@covar)
    covarnm <- covarnm[covarnm%in%parnm]
    if(length(covarnm))
    {
      datanm <- c(datanm, covarnm)
      parnm <- parnm[!parnm %in% covarnm]
    }
  }

  # check likelihood
  if(!is.null(object@logl))
  {
    lkhnm <- names(formals(object@logl))
    lkhnm <- lkhnm[!lkhnm %in% parnm]
    lkhnm <- lkhnm[!lkhnm %in% datanm]
    parnm <- c(parnm, lkhnm)
  }
    
  # params
  params <- FLPar(params=parnm)
  return(params)
} # }}}

# coerce(FLPar, list) {{{
# for re-using parameter values as 'start' in fmle()
setAs('FLPar', 'list',
  function(from)
  {
    lst <- lapply(apply(par, 2, as.list), function(x) as.vector(unlist(x)))
    return(lst)
  }
) # }}}

# lower & upper {{{
if (!isGeneric("lower"))
  setGeneric("lower", function(object, ...)
    standardGeneric("lower"))
setMethod("lower", signature(object="FLModel"),
  function(object)
    return(attr(slot(object, 'initial'), 'lower'))
)
if (!isGeneric("lower<-"))
  setGeneric("lower<-", function(object, ..., value)
    standardGeneric("lower<-"))
setReplaceMethod("lower", signature(object="FLModel", value="numeric"),
  function(object, value)
  {
    if(length(value) != dim(params(object))[2])
      stop('Need to specify a lower value for each parameter')
    attr(slot(object, 'initial'), 'lower') <- value
    return(object)
  }
)

if (!isGeneric("upper"))
  setGeneric("upper", function(object, ...)
    standardGeneric("upper"))
setMethod("upper", signature(object="FLModel"),
  function(object)
    return(attr(slot(object, 'initial'), 'upper'))
)
if (!isGeneric("upper<-"))
  setGeneric("upper<-", function(object, ..., value)
    standardGeneric("upper<-"))
setReplaceMethod("upper", signature(object="FLModel", value="numeric"),
  function(object, value)
  {
    if(length(value) != dim(params(object))[2])
      stop('Need to specify a upper value for each parameter')
    attr(slot(object, 'initial'), 'upper') <- value
    return(object)
  }
) # }}}

# iter {{{
setMethod("iter", signature(object="FLModel"),
	  function(object, it) {
  
    # FLArray
    object <- qapply(object, FUN=iter, it)
    # params
    params(object) <- iter(params(object), it)
    # vcov
    if(length(dim(vcov)) > 2)
      if(dim(vcov)[3] > 1)
        vcov(object) <- vcov(object)[,,it]
      else
        vcov(object) <- vcov(object)[,,1]
    # logLik
    logLik(object) <- iter(object@logLik, it)

		return(object)
	  }
) # }}}

# iter     {{{
setMethod("iter", signature(object="vector"),
	function(object, iter) {
    if(length(object)== 1)
      return(object)
    else
      return(object[iter])
	}
)   # }}}

# iter     {{{
setMethod("iter", signature(object="logLik"),
	function(object, iter) {
    if(length(object)== 1)
      return(object)
    else
      return(object[iter])
	}
)   # }}}

# confint
#     signature(object = "mle"): Confidence intervals from likelihood profiles.
# profile(fitted, which)

# glm

# params        {{{
if (!isGeneric("params"))
	setGeneric("params", function(object, ...)
		standardGeneric("params"))

setMethod("params", signature(object="FLModel"),
	function(object, param=missing)
  {
    if(missing(param))
      return(object@params)
    else
      return(object@params[,param])
	}
) # }}}

# params<-      {{{
if (!isGeneric("params<-"))
	setGeneric("params<-", function(object, value)
		standardGeneric("params<-"))

setMethod("params<-", signature(object="FLModel", value='FLPar'),
	function(object, value)
  {
    object@params <- value
    return(object)
	}
) # }}}
