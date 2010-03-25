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
  
  # initial returns an FLPar
  init <- do.call(initial(object), lapply(formals(initial(object)), function(x) x<-0.1))
  if(!is.null(init) & !is(init, 'FLPar'))
    return("initial function must return an 'FLPar'")

  return(TRUE)
}
setClass('FLModel',
  representation('FLComp',
    model='formula',
    logl='function',
    gr='function',
    initial='function',
    params='FLPar',
    logLik='logLik',
    vcov='array',
    hessian='array',
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
    #if(length(value) > 1)
    #  stop('value must be of length 1')

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
setMethod('coef', signature(object='FLModel'),
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
    control=list(trace=1), lower=rep(-Inf, dim(params(object))[1]),
    upper=rep(Inf, dim(params(object))[1]), seq.iter=TRUE, autoParscale=FALSE,
    tiny_number=1e-6, relAutoParscale=TRUE, ...)
  {
    # TODO Check with FL
    args <- list(...)
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
    # HACK! clean up fixed list if elements are named vectors
    fixed <- lapply(fixed, function(x){ names(x) <- NULL; x})

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
        # if is(lower, function)
        lower <- lower(object)[match(parnm, names(fixed), nomatch=0)==0]
      if(missing(upper) && !is.null(upper(object)))
        upper <- upper(object)[match(parnm, names(fixed), nomatch=0)==0]
    }
    else
    {
      lower <- -Inf
      upper <- Inf
    }

    # gr function
    if(!is.null(body(object@gr)))
      gr <- object@gr
    else
      gr <- NULL

    # create logl function
    loglfoo <- function(par) {
      params <- as.list(par)
      names(params) <- names(start)
      params[fixnm] <- lapply(fixed, iter, it)
      return(-1*(do.call(logl, args=c(params, data))))
    }
    
    # input data
    alldata <- list()
    for (i in datanm)
      alldata[[i]] <- slot(object, i)
    # iterations
    if(seq.iter)
    {
      iter <- dims(object)$iter
      # iters in fixed
      if(length(fixnm) >= 1)
      {
        fiter <- unlist(lapply(fixed, length))
        fiter <- fiter[fiter > 1]
        # all ietrs in fixed are equal?
        if(any(fiter/fiter[1] != 1))
          stop("objects in fixed have different number of iters")
        # are iter in object 1 and fixiter > 1? use fixiter
        if(iter == 1 & fiter > 1)
          iter <- fiter
        # are they different and > 1? STOP
        else if(fiter > 1 & fiter != iter)
          stop("different iters in fixed and object")
      }
    }
    else
      iter <- 1

    # logLik
    logLik <- rep(NA, iter)
    class(logLik) <- 'logLik'
    attr(logLik, 'df') <- length(parnm) - length(fixed)
    object@logLik <- logLik

    # Correct FLPar, fitted and residuals
    if(iter > dim(object@params)[length(dim(object@params))])
    {
      params(object) <- FLPar(iter=iter, params=dimnames(object@params)$params)
    }

    fitted(object) <- propagate(fitted(object), iter)
    residuals(object) <- propagate(residuals(object), iter)

    # vcov
    object@vcov <- array(NA, dim=c(rep(length(parnm)-length(fixed),2), iter),
      dimnames=list(parnm[!parnm%in%names(fixed)],parnm[!parnm%in%names(fixed)],
      iter=1:iter))
    object@hessian <- object@vcov


    for (it in 1:iter)
    {
      # data
      if(seq.iter)
        data <- lapply(alldata, iter, it)
      else
        data <- alldata
      
      # add covar if defined and available
      if('covar' %in% slotNames(object))
      {
        covarnm <- names(object@covar)
        covarnm <- covarnm[covarnm%in%names(formals(logl))]
        if(length(covarnm))
          data <- c(data, covar(object)[covarnm])
      }
      # start values
      if(missing(start)) {
        # add call to @initial
        if(is.function(object@initial))
         start <- as(do.call(object@initial, args=data[names(formals(object@initial))]),
           'list')
        else
          start <- formals(logl)[names(formals(logl))%in%parnm]
      }

      if(!is.null(fixnm))
        start[fixnm] <- NULL
      if(any(!names(start) %in% parnm))
        stop("some named arguments in 'start' are not arguments to the
          supplied log-likelihood")
      start <- start[order(match(names(start), parnm))]

      # add small number to start if 0
      start <- lapply(start, function(x) if(x == 0) x/100000 else x)
      
      if(is.null(start))
        stop("No starting values provided and no initial function available")
    
      # autoParscale
      if(autoParscale && !'parscale' %in% names(control))
      {
        # named vectors for logl plus/minus tiny_number and diff
        diff_logl <- logl_bump1 <- logl_bump2 <- unlist(start)

        # get logLik for start values
        logl_start <- do.call(logl, args=c(start, data, fixed)) 

        for(j in names(start))
        {
          # bump up & down each param by tiny_number
          bump_params <- start
          bump_params[[j]] <- bump_params[[j]] * (1 + tiny_number)
          logl_bump1[[j]] <- do.call(logl, args=c(data, bump_params, fixed))
          #
          bump_params <- start
          bump_params[[j]] <- bump_params[[j]] * (1 - tiny_number)
          logl_bump2[[j]] <- do.call(logl, args=c(data, bump_params, fixed))
        }
          diff_logl <- 1 / (abs(logl_bump1) + abs(logl_bump2)) / (unlist(start) *
            2 * tiny_number)
        
        # relative
        if(relAutoParscale)
          diff_logl <- diff_logl / max(diff_logl)

        control <- c(control, list(parscale=diff_logl))
      }
      
      # TODO protect environment
      out <- do.call('optim', c(list(par=unlist(start), fn=loglfoo, method=method,
        hessian=TRUE, control=control, lower=lower, upper=upper, gr=gr)))
      
      # output
      # place out$par in right iter dim
      iter(object@params[names(out$par),], it) <- out$par
      # fixed
      if(length(fixed) > 0)
        iter(object@params, it)[fixnm,] <- unlist(lapply(fixed, iter, it))
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
            tmphess <- try(solve(out$hessian), silent=TRUE)
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
      object@hessian[,,it] <- -out$hessian
      
      # logLik
      object@logLik[it] <- -out$value
      attr(object@logLik, 'nobs') <- length(data[[1]])

      # fitted & residuals
      iter(fitted(object), it) <- predict(iter(object, it))
      iter(residuals(object), it) <- iter(slot(object,
        as.list(object@model)[[2]]),it) - iter(fitted(object), it)
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
setMethod('predict', signature(object='FLModel'),
  function(object, ...)
  {
    args <- list(...)
    if(length(args) > 0 && is.null(names(args)))
      stop('FLQuant or FLCohort inputs must be named to apply formula')
    # call
    call <- as.list(object@model)[[3]]
    fittedSlot <- as.list(object@model)[[2]]

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

      # get right dimnames
      if(length(args) > 0)
        dimnames <- dimnames(args[[1]])
      else
        dimnames <- dimnames(slot(obj, fittedSlot))

      # check inputs
      if(it == 1)
      {
        res <- propagate(do.call(class(object@fitted), list(eval(call,
          envir=c(params, data, dimdat)))), iters, fill.iter=FALSE)
      }
      else
      {
        iter(res, it) <- do.call(class(object@fitted), list(eval(call,
          envir=c(params, data, dimdat))))
      }
    }
    dimnames(res)[1:5] <- dimnames[1:5]
    return(res)
  }
)   # }}}

# AIC & BIC   {{{
setMethod('AIC', signature(object='FLModel', k='numeric'),
  function(object, k=2)
    return(AIC(object@logLik, k))
)
# AIC with RSS: 2k + n * ln(RSS/n)
setMethod('AIC', signature(object='FLModel', k='missing'),
  function(object)
    return(AIC(object@logLik))
)

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
        start <- as(do.call(formula@initial, args=data)[parnm], 'list')
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
    
    dimnames(fitted(formula))[1:5] <- dimnames(do.call(slot, list(formula,
    as.character(as.list(formula@model)[2]))))[1:5]
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
    else if(length(dimnames(slot(object, 'params'))[['iter']]) == 1) {
      cat("Parameters: \n")
        print(t(slot(object, 'params')@.Data), digits=4)
    } else {
      cat("Parameters median(mad): \n")
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
  if(!is.null(body(object@logl)))
  {
    lkhnm <- names(formals(object@logl))
    lkhnm <- lkhnm[!lkhnm %in% datanm]
    parnm <- c(lkhnm, sort(parnm)[!sort(parnm) %in% sort(lkhnm)])
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
setMethod("lower", signature(object="FLModel"),
  function(object)
    return(attr(slot(object, 'initial'), 'lower'))
)
setReplaceMethod("lower", signature(object="FLModel", value="numeric"),
  function(object, value)
  {
    if(length(value) < dim(params(object))[1])
      value <- rep(value, length=dim(params(object))[1])
    attr(slot(object, 'initial'), 'lower') <- value
    return(object)
  }
)

setMethod("upper", signature(object="FLModel"),
  function(object)
    return(attr(slot(object, 'initial'), 'upper'))
)
setReplaceMethod("upper", signature(object="FLModel", value="numeric"),
  function(object, value)
  {
    if(length(value) != dim(params(object))[1])
      value <- rep(value, length=dim(params(object))[1])
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

# params        {{{
setMethod("params", signature(object="FLModel"),
	function(object, param=missing)
  {
    if(missing(param))
      return(object@params)
    else
      return(object@params[param,])
	}
) # }}}

# params<-      {{{
setMethod("params<-", signature(object="FLModel", value="FLPar"),
	function(object, value)
  {
    object@params <- value
    return(object)
	}
) # }}}

# parscale  {{{
setMethod("parscale", signature(object="FLModel"),
  function(object, start=missing, tiny_number=1e-6, ...) {
    # get data
    loglnames <- names(formals(logl(nsher)))
    data <- loglnames[loglnames %in% slotNames(object)]
    args <- list()
    logl <- logl(object)

    for(i in data)
      args[[i]] <- slot(object, i)

    # get start
    if(missing(start))
      start <- as(do.call(initial(object), args), 'list')

    # named vectors for logl plus/minus tiny_number and diff
    diff_logl <- logl_bump1 <- logl_bump2 <- unlist(start)

    # get logLik for start values
    logl_start <- do.call(logl, args=c(start, args)) 

    for(j in names(start))
    {
      # bump up & down each param by tiny_number
      bump_params <- start
      bump_params[[j]] <- bump_params[[j]] * (1 + tiny_number)
      logl_bump1[[j]] <- do.call(logl, args=c(args, bump_params))
      #
      bump_params <- start
      bump_params[[j]] <- bump_params[[j]] * (1 - tiny_number)
      logl_bump2[[j]] <- do.call(logl, args=c(args, bump_params))
    }
    diff_logl <- 1 / (abs(logl_bump1) + abs(logl_bump2)) / (unlist(start) * 2 * tiny_number)
        
    # relative
    if(relAutoParscale)
      diff_logl <- diff_logl / max(diff_logl)

    return(diff_logl)
  }
) # }}}

# profile {{{
setMethod("profile", signature(fitted="FLModel"),
  function(fitted, which, maxsteps=11, range=0.5, ci=c(0.25, 0.5, 0.75, 0.95),
      plot=TRUE, fixed=list(), print=FALSE, control=list(trace=0), ...)
  {
    # vars
    foo <- logl(fitted)
    params <- params(fitted)
    parnames <- dimnames(params)$params
    fixnames <- names(fixed)
    profiled <- list()
    grid <- list()
    plotfit <- TRUE

    # HACK! clean up fixed list if elements are named vectors
    fixed <- lapply(fixed, function(x){ names(x) <- NULL; x})

    # which params to profile
    if(missing(which))
      which <- parnames[!parnames %in% fixnames]
    if(length(which) > 2)
        stop("surface only works over 2 parameters")
    
    # data
    args <- list()
    data <- names(formals(foo))
    data <- data[data %in% slotNames(fitted)]
    for(i in data)
      args[i] <- list(slot(fitted, i))
      
    # use initial if model has not been estimated
    if(all(is.na(params)))
    {
      params <- do.call(initial(fitted), args)
      plotfit <- FALSE
    }

    # (1) create grid of param values for numeric range
    if(is.numeric(range) && length(range) == 1)
    {
      if(!plotfit)
        warning("model has not been fitted: initial values are used for profile range")
      for(i in which)
      {
        # steps for param[i]
        estim <- c(params[i,])
        steps <- estim * seq(1-range, 1+range, length=maxsteps)
        profiled[[i]] <- steps
      }
    # (2) and for list of ranges
    } else if (is.list(range)) 
    {
      # if missing(which), which is names in range
      if(missing(which))
        which <- names(range)
      else
        # checks all params to be profiled specified
        if(any(names(range) != which))
          stop("range not specified for parameters:", which[!which%in%names(range)])
      profiled <- lapply(range, sort)
    }

    # grid
    grid <- do.call(expand.grid, profiled)

    # col for logLik
    grid$logLik <- as.numeric(NA)

    dots <- list(...)
    # calculate logLik for grid if no fitting
    if(identical(order(c(which, fixnames)), order(parnames)))
      for(i in seq(nrow(grid)))
        grid[i, 'logLik'] <- do.call(logl(fitted), c(args, as.list(grid[i,which]), fixed))

    # or fit over grid
    else
      for(i in seq(nrow(grid)))
      {
        fixed <- as.list(grid[i,which])
        names(fixed) <- which
        grid[i, 'logLik'] <- do.call('fmle', c(list(object=fitted, fixed=fixed,
          control=control), dots[names(dots) %in% names(formals(optim))]))@logLik
      }
   
    surface <- tapply(grid$logLik, grid[,which], sum)

    # print
    if(print)
    {
      cat(paste("max(profile) =", format(max(grid$logLik), digits=5), " "))
      for(i in which)
        cat(paste(i, " = ", format(grid[grid$logLik==max(grid$logLik),i], digits=5), " "))
      cat("\n")
      if(plotfit)
      {
        cat(paste("logLik =", format(logLik(fitted), digits=5), " "))
        for(i in which)
          cat(paste(i, " = ", format(c(params(fitted)[i]), digits=5), " "))
        cat("\n")
      }
    }

    # CIs
    cis <- max(surface) - qchisq(ci, 2)
    
    # plot
    if(plot)
    {
      if(length(which) == 2)
      {
        do.call('image', c(list(x=profiled[[1]], y=profiled[[2]], z=surface,
          xlab=which[1], ylab=which[2]), dots[!names(dots) %in% names(formals(optim))]))

        if(plotfit)
          points(params[which[1]], params[which[2]], pch=19)

        do.call('contour', list(x=sort(profiled[[1]]), y=sort(profiled[[2]]), z=surface,
          levels=cis, add=TRUE, labcex=0.8, labels=ci))
      }
      else if(length(which) == 1)
      {
        plot(grid[,which], grid[,'logLik'], type='l', xlab=which, ylab="logLik", axes=F)
        axis(1); box()
        points(params[which], logLik(fitted), pch=19)
      }
    }
    if(length(which) == 2)
      invisible(list(x=grid[,which[1]], y=grid[,which[2]], z=surface))
    else if(length(which) == 1)
      invisible(list(x=grid[which], y=grid['logLik']))
  }
) # }}}

# cor2cov {{{
cor2cov <- function(Correl,Var)
{
  Covar  <-Correl

   for (i in 1:dim(Correl)[1])
      for (j in 1:dim(Correl)[2])
      {
         Prod = Correl[i,i] * Correl[j,j]

         if (abs(Prod) > 0.0)
            Covar[i,j] = Correl[i,j] * abs(Var[i] * Var[j])^0.5
         else
            Covar[i,j] = 0.0

         if (i != j)
            Covar[j,i] = Covar[i,j]
         }

   return(Covar)
   }
# }}}
