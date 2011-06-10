# FLSR - Stock-recruitment relationships
# FLCore/R/FLSR.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Dorleta García, AZTI
# $Id$


# FLSR()	{{{
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
	try <- try(fmle(sr), silent=TRUE, ...)

	if(is(try, 'try-error'))
		# else if model present run nls()
		try <- try(nls(sr), silent=TRUE, ...)
			if(is(try, 'try-error'))
				stop('neither nls() nor fmle() could be run on this object')
	return(try)
}   #}}}

## as.FLSR   {{{
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
    rec <- object@stock.n[as.character(rec.age),]
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
        slots <- names(args)[ifelse(length(which(names(args) == "rec.age"))>0,
            -which(names(args) == "rec.age"), 1:length(args))]

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
            desc = "'rec' and 'ssb' slots obtained from a 'FLBiol' object", ...)

        slot(sr, "fitted") <- FLQuant(dimnames = dimnames(slot(sr, "rec")))
        slot(sr, "residuals") <- FLQuant(dimnames = dimnames(slot(sr, "rec")))

        units(slot(sr, "rec")) <- units(slot(object, "n"))
	      units(slot(sr, "ssb")) <- units(slot(object, "wt"))
        units(slot(sr, "fitted")) <- units(slot(sr, "rec"))

        return(sr)
   }
) # }}}

# plot  {{{
setMethod("plot", signature(x="FLSR", y="missing"),
	function(x, main="Functional form", log.resid=FALSE, cex=0.8)
	{
		years <- as.numeric(dimnames(residuals(x))$year)
    scales <- list(y=list(rot=90), tck=c(1,0))

    # initial device settings
    trellis.device(new=FALSE)
    trellis.par.set(list(layout.heights = list(bottom.padding = -0.3,
      axis.xlab.padding = 0.3, xlab = -0.3), layout.widths = list(left.padding = -0.3,
      right.padding = -0.3, ylab.axis.padding = -0.3)))
		
		# panel functions
		srpanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='black', cex=cex)
			panel.loess(x,y, col='blue', lty=4)
			panel.abline(a=0, b=0, lty=2, col='gray60')
		}
		respanel <- function(x, y, ...) {
			panel.xyplot(x, y, col='black', cex=cex)
      panel.lmline(x, y, ..., col='red')
			panel.abline(a=0, b=0, lty=2, col='gray60')
		}
		# get dimensions to condition on (skip quant)
		condnames <- names((fitted(x)))[c(3:5)][dim(fitted(x))[c(3:5)]!=1]
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)

		# 1. SR values with fitted curve
    ssb <- FLQuant(seq(0, max(ssb(x) + (ssb(x)*0.15), na.rm=TRUE), length=dim(ssb(x))[2]),
      dimnames=dimnames(ssb(x))[1:5])
    fitted <- predict(x, ssb=ssb)
    print(xyplot(formula(paste("fitted~ssb", cond)), ylab='Recruits', xlab='SSB',
			model.frame(FLQuants(rec=propagate(x@rec, dims(x)$iter), ssb=ssb, fitted=fitted)),
      col='red', main=main, xlim=c(0, max(ssb, na.rm=TRUE)),
      ylim=c(0, max(x@rec, na.rm=TRUE)+(max(x@rec,na.rm=TRUE)/10)),
      groups=iter, type='l', scales=scales), split=c(1,1,2,3), more=TRUE)
		# Add model line & lowess
		trellis.focus("panel", 1, 1)
    lpoints(x@ssb, x@rec, col='black', cex=cex)
    llines(lowess(x)$ssb, lowess(x)$rec, col='blue', lty=4)
		trellis.unfocus()

    # residuals or log residuals?
    if(log.resid)
      resid <- model.frame(FLQuants(resid=log(rec(x) / fitted(x))))
    else
      resid <- model.frame(FLQuants(resid=residuals(x)))

    # 2. Residuals plotted against year
		print(xyplot(formula(paste("resid~year", cond)), ylab='Residuals', xlab='',
			data=resid, scales=scales, panel=srpanel, groups=iter,
      main='Residuals by year'), split=c(2,1,2,3), more=TRUE)

		# 3. Residuals at time t vs. residuals at time t+1
		print(xyplot(formula(paste("resid1~resid", cond)), ylab='Residuals at t+1',
      xlab='Residuals at t', data=cbind(resid, resid1=c(resid$resid[-1], NA)),
		  panel=respanel, main='AR(1) Residuals', scales=scales), split=c(1,2,2,3), more=TRUE)
		
    # 4. Residuals plotted against SSB
		print(xyplot(formula(paste("resid~ssb", cond)), ylab='Residuals', xlab='SSB',
      data=cbind(resid, ssb=c(x@ssb)),
			panel=srpanel, main='Residuals by SSB', scales=scales), split=c(2,2,2,3), more=TRUE)

    # 5. qqplot of residuals
		print(qqmath(formula(paste("~resid", cond)), ylab='Residuals',
    xlab='Sample Quantiles', data=resid, scales=scales,
      panel = function(x, ...) {
          panel.qqmath(x, ..., , col='gray40', cex=cex)
          panel.qqmathline(x, ..., col='red')
       }, main='Normal Q-Q Plot'), split=c(1,3,2,3), more=TRUE)

		# 6. Residuals plotted against Recruits
		print(xyplot(formula(paste("resid~fitted", cond)), ylab='Residuals',
      xlab='Recruits hat', data=cbind(resid, fitted=c(x@fitted)),
			panel=srpanel, main='Residuals by Estimated Recruits', scales=scales),
      split=c(2,3,2,3), more=FALSE)

				invisible()
	}
)
# }}}

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
      iter(rec, i)[!idx][order(ssb(x)[!idx])] <- out$y
      iter(ssb, i)[!idx][order(ssb(x)[!idx])] <- out$x
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
      shepherdSV  =c(s=1,v=mean(ssb,na.rm=T),c=1,spr0=mean(ssb/rec,na.rm=T)))
    if(is.null(res))
      stop("SR model not recognized")
    else
      return(res)
  }
) # }}}
