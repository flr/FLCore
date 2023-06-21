# FLSR - Stock-recruitment relationships
# FLCore/R/FLSR.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLSR  {{{


#' Class FLSR
#'
#' Class for stock-recruitment models.
#'
#' A series of commonly-used stock-recruitment models are already available,
#' including the corresponding likelihood functions and calculation of initial
#' values. See \code{\link{SRModels}} for more details and the exact
#' formulation implemented for each of them.
#'
#' @name FLSR
#' @aliases FLSR-class covar,FLSR-method desc,FLSR-method details,FLSR-method
#' distribution,FLSR-method fitted,FLSR-method gr,FLSR-method
#' hessian,FLSR-method initial,FLSR-method logl,FLSR-method
#' logLik,FLSR-method model,FLSR-method name,FLSR-method
#' params,FLSR-method range,FLSR-method rec,FLSR-method
#' residuals,FLSR-method ssb,FLSR-method vcov,FLSR-method
#' covar<-,FLSR,FLQuants-method desc<-,FLSR,character-method
#' details<-,FLSR,list-method distribution<-,FLSR,character-method
#' distribution<-,FLSR,factor-method fitted<-,FLSR,FLArray-method
#' fitted<-,FLSR,numeric-method gr<-,FLSR,function-method
#' hessian<-,FLSR,array-method initial<-,FLSR,function-method
#' logl<-,FLSR,function-method logLik<-,FLSR,logLik-method
#' logLik<-,FLSR,numeric-method model<-,FLSR,character-method
#' model<-,FLSR,formula-method model<-,FLSR,function-method
#' model<-,FLSR,list-method name<-,FLSR,character-method
#' params<-,FLSR,FLPar-method range<-,FLSR,numeric-method
#' rec<-,FLSR,FLQuant-method rec<-,FLSR,numeric-method
#' residuals<-,FLSR,FLArray-method residuals<-,FLSR,numeric-method
#' ssb<-,FLSR,FLQuant-method ssb<-,FLSR,numeric-method
#' vcov<-,FLSR,array-method
#' @docType class
#' @section Slots: \describe{
#'     \item{name}{Name of the object (\code{character}).}
#'     \item{desc}{Description of the object (\code{character}).}
#'     \item{range}{Range (\code{numeric}).}
#'     \item{rec}{Recruitment series (\code{FLQuant}).}
#'     \item{ssb}{Index of reproductive potential, e.g. SSB or egg oor egg production (\code{FLQuant}).}
#'     \item{fitted}{Estimated values for rec (\code{FLQuant}).}
#'     \item{residuals}{Residuals obtained from the model fit (\code{FLArray}).}
#'     \item{covar}{Covariates for SR model (\code{FLQuants}).}
#'     \item{model}{Model formula (\code{formula}).}
#'     \item{gr}{Function returning the gradient of the likelihood (\code{function}).}
#'     \item{logl}{Log-likelihood function (\code{function}).}
#'     \item{initial}{Function returning initial parameter values for the optimizer (\code{function}).}
#'     \item{params}{Estimated parameter values (\code{FLPar}).}
#'     \item{logLik}{Value of the log-likelihood (\code{logLik}).}
#'     \item{vcov}{Variance-covariance matrix (\code{array}).}
#'     \item{details}{Extra information on the model fit procedure (\code{list}).}
#'     \item{logerror}{Is the error on a log scale (\code{logical}).}
#'     \item{distribution}{(\code{factor}).}
#'     \item{hessian}{Resulting Hessian matrix from the fit (\code{array}).}
#' }
#' @author The FLR Team
#' @seealso \link{FLModel}, \link{FLComp}
#' @keywords classes
#' @examples
#'
#'   # Create an empty FLSR object.
#'   sr1 <- FLSR()
#'
#'   # Create an  FLSR object using the existing SR models.
#'   sr2 <- FLSR(model = 'ricker')
#'   sr2@@model
#'   sr2@@initial
#'   sr2@@logl
#'
#'   sr3 <- FLSR(model = 'bevholt')
#'   sr3@@model
#'   sr3@@initial
#'   sr3@@logl
#'
#'   # Create an FLSR using a function.
#'   mysr1 <- function(){
#'     model <- rec ~ a*ssb^b
#'     return(list(model = model))}
#'
#'   sr4 <- FLSR(model = mysr1)
#'
#'   # Create an FLSR using a function and check that it works.
#'   mysr2 <- function(){
#'     formula <- rec ~ a+ssb*b
#'
#'     logl <- function(a, b, sigma, rec, ssb) sum(dnorm(rec,
#'       a + ssb*b, sqrt(sigma), TRUE))
#'
#'    initial <- structure(function(rec, ssb) {
#'       a   <- mean(rec)
#'       b   <- 1
#'       sigma <- sqrt(var(rec))
#'
#'       return(list(a=a, b=b, sigma=sigma))},
#'         lower = c(0, 1e-04, 1e-04), upper = rep(Inf, 3))
#'
#'    return(list(model = formula, initial = initial, logl = logl))
#'   }
#'
#'   ssb <- FLQuant(runif(10, 10000, 100000))
#'   rec <- 10000 + 2*ssb + rnorm(10,0,1)
#'   sr5 <- FLSR(model = mysr2, ssb = ssb, rec = rec)
#'
#'   sr5.mle <- fmle(sr5)
#'   sr5.nls <- nls(sr5)
#'
#' # NS Herring stock-recruitment dataset
#' data(nsher)
#'
#' # already fitted with a Ricker SR model
#' summary(nsher)
#'
#' plot(nsher)
#'
#' # change model
#' model(nsher) <- bevholt()
#'
#' # fit through MLE
#' nsher <- fmle(nsher)
#'
#' plot(nsher)
#'
setClass('FLSR',
  representation(
	  'FLModel',
  	rec='FLQuant',
	  ssb='FLQuant',
  	covar='FLQuants',
    logerror='logical'),
  prototype(
    residuals=FLQuant(),
    fitted=FLQuant(),
    logerror=TRUE,
    covar=new('FLQuants')),
	validity=function(object)
  {
	  # params must have dims equal to quants
  	return(TRUE)
  }
)

invisible(createFLAccesors("FLSR", include=c('rec', 'ssb', 'covar'))) # }}}

# FLSR()	{{{
#' @rdname FLSR
#' @aliases FLSR,ANY-method
setMethod('FLSR', signature(model='ANY'),
  function(model, ...)
  {
  # TODO if no proper rec.age
    args <- list(...)
    # If both rec and ssb given
    if(all(c('rec', 'ssb') %in% names(args)))
    {
      res <- FLModel(model, ..., class='FLSR')
    }
    # if rec given, then ssb is dims$min years less
    else if ('rec' %in% names(args))
    {
      drec <- dims(args[['rec']])
      ssb <- FLQuant(dimnames=dimnames(window(args[['rec']],
        start=drec$minyear-drec$min, end=drec$maxyear-drec$min)))
      res <- FLModel(model, ssb=ssb, ..., class='FLSR')
    }
    # ssb
    else if ('ssb' %in% names(args))
    {
      dssb <- dims(args[['ssb']])
      rec <- FLQuant(dimnames=dimnames(window(args[['ssb']],
        start=dssb$minyear-1, end=dssb$maxyear-1)))
      res <- FLModel(model, rec=rec, ..., class='FLSR')
    }
    else
      res <- FLModel(model, ..., class='FLSR')

    # check if years in 'rec' and 'ssb' dimnames match with 'rec' age
      if(isTRUE(try(dims(rec(res))$minyear - dims(ssb(res))$minyear != dims(rec(res))$min)))
        warning("year dimnames for 'rec' and 'ssb' do not match with recruitment age")
    return(res)
  }
)
#' @rdname FLSR
#' @aliases FLSR,missing-method
setMethod('FLSR', signature(model='missing'),
	function(...)
		return(FLSR(formula(NULL), ...))) # }}}

# as.FLSR   {{{
setMethod("as.FLSR", signature(object="FLStock"),
  function(object, rec.age = dims(stock.n(object))$min, ...)
	{
	  # check rec.age
    if(rec.age < dims(stock.n(object))$min)
      stop("Supplied recruitment age less than minimum age class")

    args <- list(...)

    # calculate ssb and create FLSR object incorprating rec.age
    rec <- object@stock.n[as.character(rec.age),]
    ssb <- ssb(object)

    # now alter stock and recruitment to factor in the recruitement age
    if((dim(rec)[2]-1) <= rec.age)
      stop("FLStock recruitment data set too short")

    # SET rec and ssb time lags
    rec <- rec[, (1 + rec.age) : dim(rec)[2]]
    ssb <- ssb[, 1 : (dim(ssb)[2] - rec.age)]

    # create the FLSR object
    sr <- FLSR(rec=rec, ssb=ssb, name=object@name,
		fitted = FLQuant(dimnames = dimnames(rec), units=units(rec)),
		residuals = FLQuant(dimnames = dimnames(rec)),
    desc = "'rec' and 'ssb' slots obtained from a 'FLStock' object", ...)

    validObject(sr)
    return(sr)
   }
) # }}}

# lowess  {{{
setMethod('lowess', signature(x='FLSR', y='missing', f='ANY', delta='ANY', iter='ANY'),
  function(x, f=2/3, iter=3, delta=0.01 * diff(range(ssb(x)[!is.na(ssb(x))])))
  {
    # output object
    rec <- FLQuant(dimnames=dimnames(rec(x))[1:5], iter=dims(x)$iter, units=units(rec(x)))
    ssb <- FLQuant(dimnames=dimnames(ssb(x))[1:5], iter=dims(x)$iter, units=units(ssb(x)))

    for(i in seq(dims(x)$iter))
    {
      idx <- array(as.logical(is.na(iter(rec(x), i)) + is.na(iter(ssb(x), i))),
        dim=dim(iter(rec(x),i)))
      out <- lowess(iter(rec(x),i)@.Data[!idx]~iter(ssb(x),i)@.Data[!idx],
        f=f, delta=delta, iter=iter)
      suppressWarnings(iter(rec, i)[!idx][order(ssb(x)[!idx])] <- out$y)
      suppressWarnings(iter(ssb, i)[!idx][order(ssb(x)[!idx])] <- out$x)
     }

    return(FLQuants(rec=rec, ssb=ssb))
  }
) # }}}

# fmle {{{
setMethod("fmle", signature(object="FLSR", start="ANY"),
  function(object, start, ...)
  {
    res <- callNextMethod()
    # AR1 models
    if('rho' %in% dimnames(params(object))$params)
    {
      n <- dim(rec(res))[2]
      rho <- c(params(res)['rho',])
      residuals(res) <- as.numeric(NA)
      residuals(res)[,-1] <- (rec(res)[,-1] - rho*rec(res)[,-n] - fitted(res)[,-1] +
        rho*fitted(res)[,-n])
    }
    # lognormal models
    else if(object@logerror)
      residuals(res) <- log(rec(res)) - log(fitted(res))
    return(res)
  }
) # }}}

# ab  {{{
setMethod('ab', signature(x='FLSR', model='missing'),
  function(x)
  {
    res <- x
    model(res) <- sub('SV', '', SRModelName(model(x)))
    params(res) <- ab(params(x), SRModelName(model(x)))
    return(res)
  }
) # }}}

# sv  {{{
setMethod('sv', signature(x='FLSR', model='missing'),
  function(x, spr0=params(x)['spr0',])
  {
    res <- x
    model(res) <- SRModelName(model(x))
    model(res) <- paste(SRModelName(model(x)), 'SV', sep='')
    params(res) <- sv(params(x), SRModelName(model(x)), spr0=spr0)
    return(res)
  }
) # }}}

# parscale {{{
setMethod('parscale', signature(object='FLSR'),
  function(object) {
    rec <- rec(object)
    ssb <- ssb(object)
    res <- switch(SRModelName(model(object)),
      bevholt     =c(a=mean(rec,na.rm=T),      b=mean(ssb,na.rm=T)),
      ricker      =c(a=mean(rec/ssb,na.rm=T),  b=mean(ssb,na.rm=T)),
      segreg      =c(a=mean(rec/ssb,na.rm=T),  b=mean(ssb,na.rm=T)),
      shepherd    =c(a=mean(rec,na.rm=T),      b=mean(ssb,na.rm=T), c=1),
      cushing     =c(a=mean(rec/ssb,na.rm=T),  b=1),
      bevholtSV   =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
      rickerSV    =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
      segregSV    =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
      cushingSV   =c(s=1,v=mean(ssb,na.rm=T),spr0=mean(ssb/rec,na.rm=T)),
      shepherdSV  =c(s=1,v=mean(ssb,na.rm=T),c=1,spr0=mean(ssb/rec,na.rm=T)),
      NULL)
    if(is.null(res))
      stop("SR model not recognized")
    else
      return(res)
  }
) # }}}

# FLSRs {{{

#' 
#' \code{FLSRS} is a class that extends \code{list} through \code{FLlst} but
#' implements a set of features that give a little bit more structure to list
#' objects. The elements of \code{FLSRs} must all be of class
#' \code{FLSR}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#'
#' @name FLSRs
#' @aliases FLSRs-class FLSRs FLSRs,ANY-method
#' FLSRs,missing-method FLSRs,list-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @author The FLR Team
#' @seealso \link{FLlst}, \link[base]{list}, \link{FLSR}
#' @keywords classes
#' @examples
#' 
#' data(nsher)
#' bnsher <- nsher
#' model(bnsher) <- bevholt
#' bnsher <- fmle(bnsher)
#' fls <- FLSRs(Ricker=nsher, BevHolt=bnsher)
#' summary(fls)
#'
setClass("FLSRs", contains="FLComps",
	validity=function(object) {
    # All items are FLSR
    if(!all(unlist(lapply(object, is, 'FLSR'))))
      return("Components must be FLSR")
	  return(TRUE)
  }
)

#' @rdname FLSRs
#' @aliases FLSRs,FLSR-method
setMethod("FLSRs", signature(object="FLSR"), function(object, ...) {
    lst <- c(object, list(...))
    FLSRs(lst)
})

#' @rdname FLSRs
#' @aliases FLSRs,missing-method
setMethod("FLSRs", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLSRs")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLSRs',  c(list(object=object), args))
	  }
  }
)

#' @rdname FLSRs
#' @aliases FLSRs,list-method
setMethod("FLSRs", signature(object="list"),
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
    args <- c(list(Class="FLSRs", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# as.FLSRs {{{

#' Convert an FLStock into a list of one or FLSR objects.
#'
#' A single `FLStock` can be coerced into a list with one or more objects of
#' class `FLSR`, each of them typically set to a diefferemt stock-recruit model.
#'
#' @param x An estimated FLStock object to coerce.
#' @param models Name(s) of model(s) to fit.
#' @param ... Any extra arguments to be passed to *as.FLSR*.
#'
#' @return An objecdt of class `FLSRs`
#'
#' @name as.FLSRs
#'
#' @author FLR Team, 2023.
#' @seealso [FLSRs-class] [FLSRs-class] [as.FLSR()]
#' @keywords classes
#' @examples
#' data(ple4)
#' as.FLSRs(ple4, model=c("bevholt", "segreg"))

as.FLSRs <- function(x, models=NULL, ...) {

  # IF no models
  if(is.null(models))
    return(FLSRs(A=as.FLSR(x)))

  # NAME models
  if(is.null(names(models)) & is.character(models))
    models <- setNames(models, nm=models)

  FLSRs(lapply(models, function(i) as.FLSR(x, model=i)))
}
# }}}

# rec<- {{{
setReplaceMethod('rec', signature(object='FLBiol', value='FLSR'),
  function(object, value) {
    object@rec@params <- value@params
    object@rec@model <- value@model
    return(object)
  }
) # }}}
