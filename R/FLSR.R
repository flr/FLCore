# FLSR - Stock-recruitment relationships
# FLCore/R/FLSR.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Dorleta García, AZTI
# $Id$

# Reference:
# Notes:

# FLSR  {{{
validFLSR <- function(object)
{
	return(TRUE)
}
setClass('FLSR',
  representation(
	  'FLModel',
  	rec='FLQuant',
	  ssb='FLQuant',
  	covar='FLQuants'),
  prototype(residuals=FLQuant(), fitted=FLQuant()),
	validity=validFLSR)
remove(validFLSR)

invisible(createFLAccesors("FLSR", include=c('rec', 'ssb', 'covar'))) # }}}

# FLSR()	{{{
setGeneric('FLSR', function(model, ...)
		standardGeneric('FLSR'))
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
setMethod('FLSR', signature(model='missing'),
	function(...)
		return(FLSR(formula(NULL), ...))) # }}}

# sr()  {{{
sr <- function(sr, ...)
{
	# if logl present, run fmle()
	try <- try(fmle(sr), silent=TRUE)

	if(is(try, 'try-error'))
		# else if model present run nls()
		try <- try(nls(sr), silent=TRUE)
			if(is(try, 'try-error'))
				stop('neither nls() nor fmle() could be run on this object')
	return(try)
}   #}}}

## as.FLSR   {{{
if (!isGeneric("as.FLSR")) 
	setGeneric("as.FLSR", function(object, ...)
		standardGeneric("as.FLSR"))

setMethod("as.FLSR", signature(object="FLStock"),
  function(object, rec.age = dims(stock.n(object))$min, ...)
	{
	  # check rec.age
    if(rec.age < dims(stock.n(object))$min)
      stop("Supplied recruitment age less than minimum age class")

    args <- list(...)
    slots <- names(args)[ifelse(length(which(names(args) == "rec.age"))>0,
		  -which(names(args) == "rec.age"), 1:length(args))]

    # calculate ssb and create FLSR object incorprating rec.age
    rec <- dimSums(object@stock.n[as.character(rec.age),])
    ssb <- ssb(object)

    # now alter stock and recruitment to factor in the recruitement age
    if((dim(rec)[2]-1) <= rec.age)
      stop("FLStock recruitment data set too short")

    rec <- rec[,(1+rec.age):dim(rec)[2]]
    units(rec) <- units(slot(object, "stock.n"))
    ssb <- ssb[,1:(dim(ssb)[2] - rec.age)]
		units(ssb) <- units(slot(object, "stock.wt"))

    # create the FLSR object
    sr <- FLSR(rec=rec, ssb=ssb, name=object@name,
		fitted = FLQuant(dimnames = dimnames(rec), units=units(rec)),
		residuals = FLQuant(dimnames = dimnames(rec)),
    desc = "'rec' and 'ssb' slots obtained from a 'FLStock' object", ...)

    validObject(sr)
    return(sr)
   }
)
setMethod("as.FLSR", signature(object="FLBiol"),
    function(object, rec.age = "missing", ...)
	{
        validObject(object)

        # recruitment delay set using minage
        # from the FLStock object
        if(missing(rec.age))
            rec.age <- dims(n(object))$min
        else{
            if(rec.age < dims(n(object))$min)
                stop("Supplied recruitment age less than minimum age class")
        }

        if (all(is.na(slot(object, "n"))) || all(is.na(slot(object, "wt"))) ||
            all(is.na(slot(object, "fec"))) || all(is.na(slot(object, "spwn"))))
            stop("biol must have 'n', 'wt', 'm', 'fec' and 'spwn'")

        args <- list(...)
        slots <- names(args)[ifelse(length(which(names(args) == "rec.age"))>0,-which(names(args) == "rec.age"), 1:length(args))]

        # calculate ssb and create FLSR object incorporating rec.age
        rec <- dimSums(object@n[as.character(rec.age),])

        ssb <- apply(slot(object, "n") * exp(- slot(object, "m")*slot(object, "spwn"))*
            slot(object, "wt")*slot(object, "fec"), 2:5, sum)

        # now alter the stock and recruitment
        # vectors to factor in the recruitement age

        if((dim(rec)[2]-1) <= rec.age)
            stop("FLBiol recruitment data set too short")

        rec <- rec[,(1+rec.age):dim(rec)[2],,,]
        ssb <- ssb[,1:(dim(ssb)[2] - rec.age),,,]

        # create the FLSR object
        sr <- FLSR(rec = rec,ssb = ssb, name = object@name,
            desc = "'rec' and 'ssb' slots obtained from a 'FLBiol' object")

        slot(sr, "fitted") <- FLQuant(dimnames = dimnames(slot(sr, "rec")))
        slot(sr, "residuals") <- FLQuant(dimnames = dimnames(slot(sr, "rec")))

        units(slot(sr, "rec")) <- units(slot(object, "n"))
	      units(slot(sr, "ssb")) <- units(slot(object, "wt"))
        units(slot(sr, "fitted")) <- units(slot(sr, "rec"))

        for(s in slots)
            slot(sr, s) <- args[[s]]

        validObject(sr)
        return(sr)
   }
) # }}}

# plot  {{{
setMethod("plot", signature(x="FLSR", y="missing"),
	function(x)
	{
		years <- as.numeric(dimnames(residuals(x))$year)
		
    # initial device settings
    trellis.device(new=FALSE)
    trellis.par.set(list(layout.heights = list(bottom.padding = -0.5,
      axis.xlab.padding = 0.5, xlab = -0.5), layout.widths = list(left.padding = -0.5,
      right.padding = -0.5, ylab.axis.padding = -0.5)))
		
		# panel functions
		srpanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='gray40', cex=0.8)
			panel.loess(x,y, col='red')
			panel.abline(a=0, b=0, lty=2, col='blue')
		}
		respanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='gray40', cex=0.8)
      panel.lmline(x, y, ..., col='red')
			panel.abline(a=0, b=0, lty=2, col='blue')
		}
		# get dimensions to condition on (skip quant)
		condnames <- names((fitted(x)))[c(3:5)][dim(fitted(x))[c(3:5)]!=1]
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)
		# 1. SR values with fitted curve
    ssb <- FLQuant(seq(0, max(ssb(x)), length=dim(ssb(x))[2]),
      dimnames=dimnames(ssb(x)))
    print(xyplot(formula(paste("fitted~ssb", cond)), ylab='Recruits', xlab='SSB',
			model.frame(FLQuants(rec=x@rec, ssb=ssb, fitted=predict(x, ssb=ssb))),
      col='red', main='Stock Recruit',
      xlim=c(0, max(ssb, na.rm=TRUE)), ylim=c(0, max(x@rec, na.rm=TRUE)+
      (max(x@rec,na.rm=TRUE)/10)), groups=iter, type='l'), split=c(1,1,2,3), more=TRUE)

		# Add model line
		# TODO Model line by unit/area/season, if params are so too
		trellis.focus("panel", 1, 1)
    lpoints(x@ssb, x@rec, col='black', cex=0.8)
		trellis.unfocus()

		# 2. Residuals plotted against year
		print(xyplot(formula(paste("resid~year", cond)), ylab='Residuals', xlab='',
			data=model.frame(FLQuants(resid=residuals(x))),
			panel=srpanel, main='Residuals by year'), split=c(2,1,2,3), more=TRUE)
		# 3. Residuals at time t vs. residuals at time t+1
		print(xyplot(formula(paste("resid1~resid", cond)), ylab='Residuals at t+1',
      xlab='Residuals at t', model.frame(FLQuants(resid=residuals(x), 
      resid1=FLQuant(residuals(x),  
      dimnames=list(year=as.numeric(dimnames(residuals(x))$year)+1)))),
		  panel=respanel, main='AR(1) Residuals'), split=c(1,2,2,3), more=TRUE)
		# 4. Residuals plotted against SSB
		print(xyplot(formula(paste("resid~ssb", cond)), ylab='Residuals', xlab='SSB',
			model.frame(FLQuants(resid=residuals(x), ssb=x@ssb)),
			panel=srpanel, main='Residuals by SSB'), split=c(2,2,2,3), more=TRUE)
		# 5. Residuals plotted against Recruits
		print(xyplot(formula(paste("resid~fitted", cond)), ylab='Residuals', xlab='R hat',
			model.frame(FLQuants(resid=residuals(x), fitted=fitted(x))),
			panel=srpanel, main='Residuals by Estimated Recruits'), split=c(1,3,2,3),
			more=TRUE)
		# 6. qqplot of residuals
		print(qqmath(formula(paste("~resid", cond)), ylab='Residuals',
    xlab='Sample Quantiles', model.frame(FLQuants(resid=residuals(x))),
      panel = function(x, ...) {
          panel.qqmath(x, ..., , col='gray40', cex=0.8)
          panel.qqmathline(x, ..., col='red')
       }, main='Normal Q-Q Plot'), split=c(2,3,2,3), more=FALSE)
		invisible()
	}
)
# }}}

# lowess  {{{
if (!isGeneric("lowess"))
  setGeneric("lowess", useAsDefault = lowess)
setMethod('lowess', signature(x='FLSR', y='missing', f='ANY', delta='ANY', iter='ANY'),
  function(x, f=2/3, iter=3, delta=0.01 * diff(range(ssb(x))))
  {
    res <- lowess(rec(x)~ssb(x), f=f, delta=delta, iter=iter)
    idx <- order(as.numeric(ssb(x)))
    fitted(x) <- FLQuant(res$y[idx], dimnames=dimnames(ssb(x)))
    residuals(x) <- log(rec(x)/fitted(x))
    model(x) <- rec~FLQuant(lowess(ssb, rec)$y[order(as.numeric(ssb))], 
      dimnames=dimnames(rec))
    params(x) <- FLPar()
    return(x)
  }
) # }}}
