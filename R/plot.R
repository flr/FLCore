# plot.R - DESC
# /plot.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# plot {{{

#' Method plot
#' 
#' Standard plot methods for every FLCore class. FLR plot methods are based on
#' \code{\link[lattice]{lattice}}, and attempt to show a general view of the
#' object contents.
#' 
#' Users are encouraged to write their own plotting code and make use of the
#' overloaded \code{\link[lattice]{lattice}} methods, for example
#' \code{\link[lattice]{xyplot}} or \code{\link[lattice]{bwplot}}. See also
#' \code{\link{lattice-FLCore}}.
#'
#' @name plot
#' @aliases plot,FLQuant,missing-method plot,FLQuantPoint,missing-method
#' plot,FLPar,missing-method plot,FLStock,missing-method
#' plot,FLStocks,missing-method plot,FLStocks,FLPar-method
#' plot,FLBiol,missing-method plot,FLCohort,missing-method
#' plot,FLIndex,missing-method plot,FLIndices,missing-method
#' plot,FLSR,missing-method
#' @docType methods
#' @section Generic function: plot(x,y)
#' @author The FLR Team
#' @seealso \link[graphics]{plot}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' 
NULL # }}}

# FLQuant     {{{

#' @rdname plot
#' @aliases plot,FLQuant,missing-method
#' @examples
#' # FLQuant
#' plot(catch.n(ple4)[, 1:20])
#' plot(catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)
#'
setMethod("plot", signature(x="FLQuant", y="missing"),
  function(x, xlab="year", ylab=paste("data (", units(x), ")", sep=""), type='p', ...) {

    # get dimensions to condition on (length !=1)
  condnames <- names(dimnames(x)[c(1,3:5)][dim(x)[c(1,3:5)]!=1])
  cond <- paste(condnames, collapse="+")

  if(cond != "") cond <- paste("|", cond)
    formula <- formula(paste("data~year", cond))

  # set strip to show conditioning dimensions names
  strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

  # using do.call to avoid eval of some arguments
  lst <- substitute(list(...))
  lst <- as.list(lst)[-1]
  lst$data <- x
  lst$x <- formula
  lst$xlab <- xlab
  lst$ylab <- ylab
  lst$strip <- strip
  lst$type <- type
  if(dim(x)[6] == 1)
    do.call("xyplot", lst)
  else
    do.call("bwplot", lst)
  }
) # }}}

# FLStock  {{{

#' @rdname plot
#' @aliases plot,FLStock,missing-method
#' @examples
#' # FLStock
#' data(ple4sex)
#' plot(ple4)
#' plot(ple4sex)
#'
setMethod("plot", signature(x="FLStock", y="missing"),
	function(x, auto.key=TRUE, ...)
  {
    # create data.frame with catch/landings+discards/discards
    obj <- as.data.frame(FLQuants(catch=apply(catch(x), c(1,2,6), sum),
      landings=apply(landings(x), c(1,2,6), sum)))
    obj$panel <- 'catch'

    # ssb
    obj <- rbind(obj, data.frame(as.data.frame(FLQuants(ssb=apply(ssb(x), c(1,2,6),
      sum))), panel='SSB'))

    # harvest
    if(units(harvest(x)) == "f")
      obj <- rbind(obj, data.frame(as.data.frame(FLQuants(harvest=apply(fbar(x),
        c(1,2,6), sum))), panel='harvest'))
    else if(units(harvest(x)) == "harvest")
      obj <- rbind(obj, data.frame(as.data.frame(FLQuants(harvest=apply(
        quantSums(harvest(x)), c(1,2,6), sum))), panel='harvest'))

    # and rec
    obj <- rbind(obj[,-1], data.frame(as.data.frame(FLQuants(rec=apply(rec(x),
      c(1,2,6), sum))), panel='recruits')[,-1])

    # default options
    options <- list(scales=list(relation='free'), ylab="", xlab="",
      main=ifelse(length(name(x)) > 0, name(x), ""), col='black', lwd=2, cex=0.6,
      box.width=1)
    args <- list(...)
    options[names(args)] <- args

    # pfun
    pfun <- function(x, y, groups, subscripts, iter=obj$iter, ...)
    {
      # catch/landings/discards
      if(panel.number() == 1)
      {
        idx <- groups == 'catch'
        if(length(levels(iter)) > 1)
        {
          # median
          #panel.xyplot(x[idx][iter[idx] == levels(iter[idx])[1]],
          panel.xyplot(unique(x[idx]),
            tapply(y[idx], x[idx], median, na.rm=TRUE), type= 'l', ...)
          # 95% quantile
          #panel.xyplot(x[idx][iter[idx] == levels(iter[idx])[1]],
          panel.xyplot(unique(x[idx]),
            tapply(y[idx], x[idx], quantile, 0.95, na.rm=TRUE), type= 'l', lwd=1, lty=2,
            col='grey50')
          # 5% quantile
          #panel.xyplot(x[idx][iter[idx] == levels(iter[idx])[1]],
          panel.xyplot(unique(x[idx]),
            tapply(y[idx], x[idx], quantile, 0.05, na.rm=TRUE), type= 'l', lwd=1, lty=2,
            col='grey50')
          # landings bars
          idx <- groups == 'landings'
          #panel.barchart(x[idx][iter[idx] == levels(iter[idx])[1]],
          panel.barchart(unique(x[idx]),
            tapply(y[idx], x[idx], median), horizontal=FALSE, col=rgb(0.1, 0.1, 0, 0.1),
            box.width=options$box.width, lwd=0, origin=0)
        }
        else
        {
          panel.xyplot(x[idx], y[idx], type= 'l', ...)
          panel.xyplot(x[idx][x==max(x)], y[idx][x==max(x)], type='p', ...)
          # landings & discards bars
          idx <- groups == 'landings'
          panel.barchart(x[idx], y[idx], horizontal=FALSE, col=rgb(0.1, 0.1, 0, 0.1),
            box.width=options$box.width, lwd=0, origin=0)
        }
        # key
        draw.key(list(text=list(lab='catch'),
          lines=list(lwd=c(2)),
          text=list(lab='landings'),
          rectangles=list(col=rgb(0.1, 0.1, 0, 0.1))),
          vp = viewport(x = unit(0.5, "npc"), y = unit(0.95, "npc")), draw=TRUE)
      }
      else
      {
        if(length(levels(iter)) > 1)
        {
          # median
          panel.xyplot(unique(x), tapply(y, x, median, na.rm=TRUE), type= 'l', ...)
          # 95% quantile
          panel.xyplot(unique(x), tapply(y, x, quantile, 0.95, na.rm=TRUE), type= 'l',
            lwd=1, lty=2, col='grey50')
          # 5% quantile
          panel.xyplot(unique(x), tapply(y, x, quantile, 0.05, na.rm=TRUE), type= 'l',
            lwd=1, lty=2, col='grey50')
        }
        else
        {
          panel.xyplot(x, y, type='l', ...)
          panel.xyplot(x[x==max(x)], y[x==max(x)], type='p', ...)
        }
      }
    }
    do.call(xyplot, c(list(x=data ~ year|panel, data=obj, panel=pfun,
      groups=expression(qname)), options))
	}
)	# }}}

# FLBiol {{{

#' @rdname plot
#' @aliases plot,FLBiol,missing-method
#' @examples
#' # FLBiol
#' data(ple4.biol)
#' plot(ple4.biol)
#'
setMethod("plot", signature(x="FLBiol", y="missing"),
	function(x, y, ...)
  {
    data <- as.data.frame(FLQuants(ssb=ssb(x), recruitment=n(x)[1,]))

     if(length(levels(data$iter)) > 1)
     pfun <- function(x, y, ...)
      {
        args <- list(...)
        # median
        do.call(panel.xyplot, c(list(x=unique(x), y=tapply(y, x, median), lwd=2, lty=1,
          type='l'), args[!names(args) %in% c('lwd', 'lty', 'type')]))
        # lowq
        do.call(panel.xyplot, c(list(x=unique(x), y=tapply(y, x, quantile, 0.025), lwd=1,
        lty=2, type='l'), args[!names(args) %in% c('lwd', 'lty', 'type')]))
        # uppq
        do.call(panel.xyplot, c(list(x=unique(x), y=tapply(y, x, quantile, 0.975), lwd=1,
        lty=2, type='l'), args[!names(args) %in% c('lwd', 'lty', 'type')]))
      }
    else
    pfun <- function(x, y, ...)
    {
      panel.xyplot(x, y, ...)
      args <- list(...)
      args <- args[!names(args) %in% c('type', 'pch')]
      do.call(panel.xyplot, c(args, list(x=x[length(x)], y=y[length(y)], pch=19)))
    }

    options <- list(aspect='xy', type='l', col='black', pch=19, cex=0.5, lwd=2,
      scales=list(relation='free'), ylab='', xlab='', panel=pfun)
    args <- list(...)
    for(i in names(args))
      options[i] <- args[i]

		condnames <- names(dimnames(x@n)[c(3:5)][dim(x@n)[c(3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "")
      cond <- paste("|qname*", cond)
    else
      cond <- paste("|qname")
		formula <- formula(paste("data~year", cond))
    do.call(xyplot, c(options, list(x=formula, data=data)))

	}
) # }}}

# FLIndex {{{

#' @rdname plot
#' @aliases plot,FLIndex,missing-method
#' @examples
#' # FLIndex
#' data(ple4.index)
#' plot(ple4.index)
#'
# Author: Mark Payne, DIFRES
setMethod("plot", signature(x="FLIndex",y="missing"),
  function(x, type=c("splom"), ...)
  {
    # The body of the plot method
    validObject(x)
		type <- type[1]

	  res <- switch(type,
		  "splom" = plotinternal(x=x, ... ),
		  "ts" = plotts(x=x, ... ),
		  "pairwise"=pairwiseConsistency(idx=x,...),
		  "internal"=plotInternalConsistency(idx=x,...),
		  cat("type must be 'splom', 'ts', 'pairwise' or 'internal'!\n"))
	  # Return result invisibly
	  invisible(res)
    }
)

# plotinternal  {{{
plotinternal <- function(x, ... )
{
  pfun <- function(x,y,...)
  {
    panel.xyplot(x,y, ...)
    if (length(x) > 1)
      panel.lmline(x,y, lty=1)
  }
  skip <- matrix(1,nrow=dims(x@index)$age-1, ncol=dims(x@index)$age-1)
  xydf <- NULL
  for (age in dims(x@index)$min:(dims(x@index)$max-1) )
  {
    for (inc in 1:(dims(x@index)$max-age))
      {
        years <- dims(x@index)$minyear:(dims(x@index)$maxyear-inc)
        xd <- as.numeric(x@index[as.character(age),as.character(years),])
        yd <- as.numeric(x@index[as.character(age+inc),as.character(years+inc),])
        d <- paste("age",age,"vs",age+inc)
        xydf <- rbind(xydf,cbind(as.data.frame(cbind(xd,yd)),d))
        skip[dims(x@index)$max-dims(x@index)$min+1-inc, age-dims(x@index)$min + 1] <-0
      }
  }
  xydf <- xydf[xydf$xd != 0 & xydf$yd != 0 & !is.na(xydf$xd) & !is.na(xydf$yd),]
  print(xyplot(yd~xd|d,outer=F, col="black", data=xydf, panel=pfun,
    scales=list(log="e",relation="sliced",draw=FALSE), layout=c(dims(x@index)$age-1,
    dims(x@index)$age-1), xlab="log index", ylab="log index", skip=as.numeric(skip), 
    main=x@name))
} # }}}

# plotts  {{{
plotts <- function(x, ...)
{
  dps <- as.data.frame(sweep(x@index,1,apply(x@index,  1,mean,na.rm=T),"/"))
  dps$year <- as.numeric(as.character(dps$year))
  dps$age <- as.factor(dps$age)
  print(xyplot(data~year|age,outer=T, type="b", data=dps, scales=list(relation="sliced",
  draw=TRUE),lty=(5:1),lwd=1.5, ylab="standardised index", ...))
} # }}}

# pairwise comparison of age consistency  {{{
pairwiseConsistency <- function(idx, show.scales=FALSE, log.scales=TRUE, ...)
{
  #Convert to Cohorts
  flc <- FLCohort(idx@index)
  
  #Convert to log scales if necessary
  if(log.scales)
    flc <- log10(flc)
  
  #Convert to data frame, filter NAs, non-positive values
  cohort.df     <-  as.data.frame(flc)
  cohort.df       <-  cohort.df[is.finite(cohort.df$data),]
  cohort.df       <-  cohort.df[(cohort.df$data>0),]
  
  #Subset by age
  age.list        <-  as.list(dimnames(idx@index)$age)
  n.ages          <-  length(age.list)
  index.by.age.l  <-  lapply(age.list, function(d)
                        {subset(cohort.df, cohort.df$age==d)})
  #Matched sequential ages together
  paired.ages.l   <-  lapply(as.list(1:(n.ages-1)),
    function(i)
    {
      merge(index.by.age.l[[i]],index.by.age.l[[i+1]],by=c("cohort","unit","season",
        "area", "iter"),all=FALSE)
    }
  )

  paired.ages     <-  do.call(rbind,paired.ages.l)
  paired.ages$id  <-  paste("Age",paired.ages$age.x,"vs",paired.ages$age.y)
  
  #Do plot
  p  <-  xyplot((data.y) ~ (data.x) | id, type = "p",
    data = paired.ages, scales = list(relation = "free",draw=show.scales),
    as.table=TRUE, sub=list(label="Dotted lines are 95% confidence interval for the mean.",
    cex=0.7), panel=function(x,y,...)
    {
      # Plot data points
      panel.xyplot(x,y,...)
      #Fit linear model and confidence limits, calc rsq
      #but only if there are sufficient data points
      if(length(x) > 2)
      {
        g   <-  lm(y~x)
        x.rng   <-  data.frame(x=seq(min(pretty(x)),max(pretty(x)),length.out=10))
        ci  <-  data.frame(x.rng,predict(g,x.rng,interval="confidence"))
        panel.xyplot(ci$x,ci$fit,lwd=2,type="l")
        panel.xyplot(ci$x,ci$upr,lwd=1,lty=2,type="l")
        panel.xyplot(ci$x,ci$lwr,lwd=1,lty=2,type="l")
        
        #Put Rsq on plot in top left corner
        rsq <-  sprintf("%4.3f",round(summary(g)$r.squared,3))
        grid::grid.text(label = bquote(r^2 == .(rsq)),x = unit(1, "npc") - unit(0.25,
          "lines"), y = unit(1,"lines"),just="right",gp=gpar(cex=0.8))
      }
    },
    xlab = if(log.scales) {expression(paste(Log[10]," (Younger Age)"))}
      else {"Younger Age"},
    ylab = if(log.scales) {expression(paste(Log[10]," (Older Age)"))} 
      else { "Older Age"},
    ...)
    print(p)
    
    return(p)
}   # }}}

# internal consistency  {{{
plotInternalConsistency <-  function(idx,log.scales=TRUE,
  cols=c("white", "yellow", "red"),use.rsq=TRUE,mark.significant=FALSE,...)
  {

  # Define colour function
  if(!length(cols)>0) stop("Colour definitions do not contain sufficient number of colours (at least one required)")
  if(length(cols)==1) cols <- rep(cols,2)
  colFn <- colorRamp(colors=cols)

  #Number of ages
  ages <- dimnames(idx@index)[[1]]

  #Convert to Cohorts, reshape into appropriate format for splom
  flc <-  if(log.scales) {log10(idx@index)} 
    else {idx@index}
  flc  <- as.data.frame(FLCohort(flc))
  flc.wide <-  reshape(flc,direction="wide",timevar=names(flc)[1],idvar=names(flc)[2:6])
  names(flc.wide) <-  gsub("data.","",names(flc.wide))
  
  #Default plot settings
  plot.args <- list(~flc.wide[ages],data=flc.wide, pscales=0,varname.font=2,varname.cex=1.5,
      xlab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))} 
        else {"Index Value"},
      ylab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))}
        else { "Index Value"},
      sub=list(if(use.rsq) {expression(paste("Lower right panels show the Coefficient of Determination (",italic(r^2),")"))}
        else { expression(paste("Lower right panels show the Coefficient of Correlation (",italic(r),")"))},cex=0.7),
      upper.panel=function(x,y,...)
      {
        # Filter out NAs
        both.points  <-  is.finite(x) & is.finite(y)
        x.filtered <-  x[both.points]
        y.filtered <-  y[both.points]
        # Only plot lmline if there is more than one point - colour panel according to rsq.
        if(length(x.filtered)>2)
        {
          r <-  cor(y.filtered,x.filtered)    
          if(use.rsq) {
            panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2)
          } else {
            panel.colour <- 0.5*r+0.5     #Colour & number panel based on the correlation coefficient (r)
          }
          if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) {
              lm.model <- lm(y.filtered ~ x.filtered)
              p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2    #Halve the p-value, as we are doing a one sided test, not a two
              slope    <- summary(lm.model)$coefficients["x.filtered",1]
              signif.level <- 0.05
              if(is.numeric(mark.significant)) signif.level <- mark.significant 
              if(p.value < signif.level & slope >0) {  #If marking significance, only fill panel and draw line when its significant
                number.format <- "%4.3f*"      #If its a significant correlation, mark with a *
                panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
                panel.lmline(x.filtered,y.filtered,lwd=2)
              }                
          } else {  #If not marking significance, always fill panel and draw best fit line
              panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
              panel.lmline(x.filtered,y.filtered,lwd=2)
          }
        }
        panel.splom(x.filtered,y.filtered,col="black",...)
      },
      lower.panel=function(x, y, ...)
      {
        #Filter out NAs
        both.points  <-  is.finite(x) & is.finite(y)
        x.filtered <-  x[both.points]
        y.filtered <-  y[both.points]
        
        #Calculate r squared - but only if there is enough data to do so
        if(length(x.filtered)>2)
        {
          r <-  cor(y.filtered,x.filtered)
          if(use.rsq) {
            panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2)
            panel.number <- round(r^2,3)
          } else {
            panel.colour <- 0.5*r+0.5  #Colour & number panel based on the correlation coefficient (r)
            panel.number <- round(r,3)
          }
          number.format <- "%4.3f"
          if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) {
              lm.model <- lm(y.filtered ~ x.filtered)
              p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2
              slope    <- summary(lm.model)$coefficients["x.filtered",1]
              signif.level <- 0.05
              if(is.numeric(mark.significant)) signif.level <- mark.significant 
              if(p.value < signif.level & slope > 0) {  #If marking significance, only fill panel when its significant & positive
                number.format <- "%4.3f*"      #If its a significant correlation, mark with a *
                panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
              }                
          } else {  #If not marking significance, always fill panel 
              panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
          }
          grid::grid.text(label =sprintf(number.format,panel.number),x = unit(0.5, "npc"),
            y = unit(0.5,"npc"),just="center")}})

  #Passed settings
  passed.args   <- list(...)
  plot.args[names(passed.args)] <- passed.args
  
  #Do plot
  p <- do.call(splom,plot.args)
  print(p)
  return(p)
}   # }}}
# }}}

# FLSR  {{{

#' @rdname plot
#' @aliases plot,FLSR,missing-method
#' @examples
#' # FLSR
#' data(nsher)
#' plot(nsher)
#'
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

# FLPar {{{

#' @rdname plot
#' @aliases plot,FLPar,missing-method
#' @examples
#' # FLPar
#' fpa <- FLPar(a=rnorm(100, 1, 20), b=rlnorm(100, 0.5, 0.2))
#' plot(fpa)
#'
setMethod("plot", signature(x="FLPar", y="missing"),
  function(x, y="missing", ...) {
    # get dimensions to condition on (skip iter)
    condnames <- names(dimnames(x))[names(dimnames(x)) != 'iter']
    cond <- paste(condnames, collapse="+")
    if(cond != "") cond <- paste("|", cond)
      formula <- formula(paste("~data", cond))
    # set strip to show conditioning dimensions names
    strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

    do.call('densityplot', list(x=formula, data=as.data.frame(x, row.names='row'),
      ylab="", xlab="", scales=list(y=list(draw=FALSE), relation='free'), col='black'))
  }
)

# densityplot


# histogram
setMethod("histogram", signature("formula", "FLPar"), function(x, data, ...){
  lst <- substitute(list(...))
  lst <- as.list(lst)[-1]
  data <- as.data.frame(data)
  lst$data <- data.frame(param=rep(names(data), each=nrow(data)),
    data=as.vector(unlist(c(data))))
  lst$x <- x
  do.call("histogram", lst)
})

# splom

#' Method splom
#' 
#' Draws a conditional scatter plot matrix.
#' 
#' See the help page in \code{\link[lattice]{lattice}} for a full description
#' of each plot and all possible arguments.
#'
#' @name splom
#' @aliases splom,FLPar,missing-method
#' @docType methods
#' @section Generic function: splom(x,data)
#' @author The FLR Team
#' @seealso \link[lattice]{splom}
#' @keywords methods
#' @examples
#' 
#' flp <- FLPar(c(t(mvrnorm(500, mu=c(0, 120, 0.01, 20),
#'   Sigma=matrix(.7, nrow=4, ncol=4) + diag(4) * 0.3))),
#'   dimnames=list(params=c('a','b','c','d'), iter=1:500), units="NA")
#'
#' splom(flp)
#'

setMethod("splom", signature("FLPar", "missing"),
  function(x, data, ...){
    splom(as.data.frame(x))
  }
)   # }}}

# FLCohort  {{{
setMethod("plot", signature(x="FLCohort", y="missing"),
  function(x, y="missing", ...){
    dots <- list(...)
    condnames <- names(dimnames(x)[c(3:5)][dim(x)[c(3:5)]!=1])
    cond <- paste(condnames, collapse="+")
    if(cond != "") cond <- paste("*", cond)
    formula <- formula(paste("data~age|as.factor(cohort)", cond))
    dots$x <- formula
    dots$data <- x
    dots$ylab <- units(x)
    dots$xlab <- "age"
    dots$type <- c("l")  
    do.call("xyplot", dots)
  }
) # }}}

# FLIndices  {{{
setMethod("plot", signature(x="FLIndices",y="missing"),
  function(x,show.scales=FALSE,log.scales=TRUE,...)
  {
    # Generate combinations of surveys to compare
		surv.comb	<-combn(1:length(x),2,simplify=FALSE)
		p.list <- lapply(surv.comb,function(survs)
    {
      # Extract surveys to compare
			surv.x <- x[[survs[1]]]
      surv.y <- x[[survs[2]]]
      # Convert to log scales if necessary
      if(log.scales)
        surv.x@index <- log10(surv.x@index);surv.y@index <- log10(surv.y@index)

      # Match up survey data to compare
			comb.surv <- merge(as.data.frame(surv.x@index),as.data.frame(surv.y@index),
        by=c("age","year","unit","season","area","iter"),all=FALSE)

      # Filter NAs, negative values
      comb.surv <- comb.surv[is.finite(comb.surv$data.x) & is.finite(comb.surv$data.y),]
      comb.surv <- comb.surv[comb.surv$data.x >0 & comb.surv$data.y>0,]

      # Plot comparison figure
			p <- xyplot((data.y) ~ (data.x) | paste("Age",age), data=comb.surv,
        as.table=TRUE, sub=list(label="Dotted lines are 95% confidence interval
        for the mean.",cex=0.7), scales = list(relation = "free",draw=show.scales),
        xlab=if(log.scales) {substitute(expression(paste(Log[10],group("(",survxname,
          ")"))),list(survxname=surv.x@name))} else { surv.x@name },
        ylab=if(log.scales) {substitute(expression(paste(Log[10],group("(",survyname,
          ")"))),list(survyname=surv.y@name))} else { surv.y@name },
        panel=function(x,y,...)
        {
          # Plot data points
          panel.xyplot(x,y,...)

          # Fit linear model and plot, along with confidence limits
					# but only if there are sufficient data points
					g <- lm(y~x)
          x.rng <- data.frame(x=seq(min(pretty(x)),max(pretty(x)),length.out=10))
          ci  <-  data.frame(x.rng,predict(g,x.rng,interval="confidence"))
          panel.xyplot(ci$x,ci$fit,lwd=2,type="l")
          panel.xyplot(ci$x,ci$upr,lwd=1,lty=2,type="l")
          panel.xyplot(ci$x,ci$lwr,lwd=1,lty=2,type="l")

          # Put Rsq on plot in top left corner
          rsq <- sprintf("%4.3f",round(summary(g)$r.squared,3))
					grid::grid.text(label = bquote(r^2 == .(rsq)),x = unit(1, "npc") - unit(0.25,
            "lines"), y = unit(1,"lines"),just="right",gp=gpar(cex=0.8))
			  }, ...)
      })  #End lapply

			#Print figures, return
			lapply(p.list,print)
			return(p.list)
  }
) # }}}

# FLStocks {{{
setMethod('plot', signature(x='FLStocks', y='missing'),
    function(x, ...){
        foo <- function(x) {
        if(units(harvest(x)) == 'f')
            har <- fbar(x)
        else
            har <- quantSums(harvest(x))
          return(as.data.frame(FLQuants(catch=catch(x), ssb=ssb(x), rec=rec(x), harvest=har)))
        }
    dfs <- lapply(x, foo)
    args <- list(...)

    # element names
    names <- names(x)
    names[names == ""]  <- seq(length(dfs))[names == ""]
    for(i in seq(length(dfs))) dfs[[i]] <- cbind(dfs[[i]], stock=names[i])
    dfs <- Reduce(rbind, dfs)

    # to set up ylims later, if needed
    yl <- with(dfs, tapply(data, qname, max, na.rm=TRUE))*1.2

    # default options
    options <- list(ylab="", xlab="", scales=list(y=list(relation="free")))
    options$par.settings=list(
        superpose.line=list(col=rainbow(length(x)), lwd=2),
        superpose.symbol=list(col=rainbow(length(x)), pch=19, cex=0.6),
        strip.background=list(col="gray85")
    )
    options$data <- dfs

    if(length(levels(dfs$iter)) == 1){
        options$panel <- function(x, y, groups, subscripts, ...){
            panel.xyplot(x, y, type=c('g','l'), groups=groups, subscripts=subscripts, ...)
            idx <- x==max(x)
            panel.xyplot(x[idx], y[idx], type='p', groups=groups, subscripts=subscripts[idx], ...)
        }
        options$x <- data~year|qname
        options$groups <- expression(stock)
        # key
        options[names(args)] <- args
        if(isTRUE(options$auto.key)) options$auto.key <- list(points=FALSE, lines=TRUE, space="right")
        do.call("xyplot", options)
    } else {
        options$panel <- function(x, y, subscripts, iter=dfs$iter, ...){
            panel.grid()
            panel.polygon(x=c(unique(x), rev(unique(x))), y=c(tapply(y, x, quantile, 0.95, na.rm=TRUE), rev(tapply(y, x, quantile, 0.05, na.rm=TRUE))), col="gray75", border="gray75")
            panel.xyplot(unique(x), tapply(y, x, median, na.rm=TRUE), type= 'l', col=1, ...)
        }
        options$prepanel <- function(x, y, subscripts, ...){
            lst <- prepanel.default.xyplot(x,y)
            lst$ylim <- c(0,1.1*max(dfs[dfs$qname==dfs[subscripts[1],"qname"],"data"]))
            lst
        }
        options$x <- data~year|stock*qname
        options$key <- simpleKey(c("median", "90% CI"), points=FALSE, lines=TRUE, space="top", columns=2)
        options$key$lines$col <- c("black", "gray75")
        options$layout <- c(length(x),4)
	    options$scales <- list(y=list(relation="free"))
        args[names(options)] <- options
#        if(is(latticeExtra::useOuterStrips, "function")) latticeExtra::useOuterStrips(do.call("xyplot", args)) else 
          do.call("xyplot", args)
    }
})

setMethod('plot', signature(x='FLStocks', y='FLPar'),
  function(x, y, ylab="", xlab="", ...)
  {
    # check y is as present in FLBRP
    y <- y[1,c('harvest', 'yield', 'rec', 'ssb'), 'msy']

    # basic plot
    plot(x, ylab=ylab, xlab=xlab, ...)

  }
) # }}}

# lattice {{{

#' Lattice methods 
#'
#' Implementation of Trellis graphics in FLR
#' 
#' Plot methods in the \code{\link[lattice]{lattice}} package are available for
#' an object of classes \code{FLQuant}, \code{FLQuants} or those derived from
#' \code{FLComp}.
#' 
#' See the help page in \code{\link[lattice]{lattice}} for a full description
#' of each plot method and all possible arguments.
#' 
#' Plot methods from lattice are called by passing a \link[base]{data.frame}
#' obtained by converting the FLR objects using as.data.frame. For details on
#' this transformation, see \link{as.data.frame-FLCore}.
#'
#' @name lattice
#' @aliases lattice-FLCore
#' @docType methods
#' @section Generic function:
#' barchart(x, data, ...)
#' 
#' bwplot(x, data, ...)
#' 
#' densityplot(x, data, ...)
#' 
#' dotplot(x, data, ...)
#' 
#' histogram(x, data, ...)
#' 
#' stripplot(x, data, ...)
#' 
#' xyplot(x, data, ...)
#' @author The FLR Team
#' @seealso \link[lattice]{xyplot}, \link[lattice]{barchart},
#' \link[lattice]{bwplot}, \link[lattice]{densityplot},
#' \link[lattice]{dotplot}, \link[lattice]{histogram},
#' \link[lattice]{stripplot}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' # xyplot on FLQuant
#'   xyplot(data~year|age, catch.n(ple4)[, 1:20])
#'   xyplot(data~year|as.factor(age), catch.n(ple4)[, 1:20], type='b', pch=19,
#'     cex=0.5)
#' 
#' # bwplot on FLQuant with iter...
#'   flq <- rnorm(100, catch.n(ple4)[, 1:20], catch.n(ple4)[,1:20])
#'   bwplot(data~year|as.factor(age), flq)
#' # ...now with same style modifications
#'   bwplot(data~year|as.factor(age), flq, scales=list(relation='free',
#'     x=list(at=seq(1, 20, by=5),
#'     labels=dimnames(catch.n(ple4)[,1:20])$year[seq(1, 20, by=5)])),
#'     cex=0.5, strip=strip.custom(strip.names=TRUE, strip.levels=TRUE,
#'     var.name='age'))
#' 
NULL # }}}

# xyplot {{{

#' @rdname lattice
setMethod("xyplot", signature("formula", "FLQuant"),
function(x, data, ...){
lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("xyplot", lst)
})

#' @rdname lattice
setMethod("xyplot", signature("formula", "FLCohort"), function(x, data, ...){
  lst <- substitute(list(...))
  lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
  lst$x <- x
  do.call("xyplot", lst)
})

#' @rdname lattice
setMethod("xyplot", signature("formula", "FLQuants"), function(x, data, ...)
	{
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    	lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
}) 

#' @rdname lattice
setMethod("xyplot", signature("formula", "FLComp"),
	function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
})
# }}}

# bwplot {{{
#' @rdname lattice
setMethod("bwplot", signature("formula", "FLQuant"),

function(x, data, ...){
lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
lst$x <- x
do.call("bwplot", lst)

})

#' @rdname lattice
setMethod("bwplot", signature("formula", "FLComp"),

	function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	lst$x <- x
	do.call("bwplot", lst)

}) # }}}

# dotplot {{{
#' @rdname lattice
setMethod("dotplot", signature("formula", "FLQuant"),
  function(x, data, ...) {
    lst <- substitute(list(...))
    lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
    lst$x <- x
    do.call("dotplot", lst)
  })

#' @rdname lattice
setMethod("dotplot", signature("formula", "FLComp"),
  function(x, data, ...){
    lst <- substitute(list(...))
  	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	  lst$x <- x
  	do.call("dotplot", lst)
  }) # }}}

# barchart {{{
#' @rdname lattice
setMethod("barchart", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("barchart", lst)

})

#' @rdname lattice
setMethod("barchart", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("barchart", lst)

}) # }}}

# stripplot {{{
#' @rdname lattice
setMethod("stripplot", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
lst$x <- x
do.call("stripplot", lst)

})

#' @rdname lattice
setMethod("stripplot", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	lst$x <- x
	do.call("stripplot", lst)

}) # }}}

# histogram {{{

#' @rdname lattice
setMethod("histogram", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("histogram", lst)

})

#' @rdname lattice
setMethod("histogram", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("histogram", lst)

})

#' @rdname lattice
setMethod("histogram", signature("formula", "FLQuants"), function(x, data, ...)
	{
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    	lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("histogram", lst)
}) # }}}

# densityplot {{{
#' @rdname lattice
setMethod("densityplot", signature("formula", "FLPar"), function(x, data, ...){
  lst <- substitute(list(...))
  lst <- as.list(lst)[-1]
  lst$data <- as.data.frame(data, row.names='row')
  lst$x <- x
  do.call("densityplot", lst)
}) # }}}

# bubbles {{{

#' Method Bubbles plot
#' 
#' This method plots three dimensional data such as matrices by age and year or
#' age-class, very common in fisheries. The area of each bubble is proportional
#' to the corresponding value in the matrix. Note that \code{bubbles} accepts
#' an argument \code{bub.scale} to control the relative size of the bubbles.
#' Positive and negative values have separate colours.
#'
#' @name bubbles
#' @aliases bubbles bubbles-methods bubbles,formula,FLQuant-method
#' @docType methods
#' @section Generic function: bubbles(x, data)
#' @author The FLR Team
#' @seealso \link[lattice]{lattice}, \code{\linkS4class{FLQuant}},
#' \code{\linkS4class{FLQuants}},\code{\linkS4class{FLCohort}}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' bubbles(age~year, data=catch.n(ple4))
#' bubbles(age~year, data=catch.n(ple4), bub.scale=5)
#' bubbles(age~cohort, data=FLCohort(catch.n(ple4)), bub.scale=5)
#' 
#' qt01 <- log(catch.n(ple4)+1)
#' qt02 <- qt01+rnorm(length(qt01))
#' flqs <- FLQuants(qt01=qt01, qt02=qt02)
#' bubbles(age~year|qname, data=flqs, bub.scale=1)
#' 
#' qt03 <- FLQuant(rnorm(100),dimnames=list(age=as.character(1:10),
#'   year=as.character(1:10)))
#' bubbles(age~year, data=qt03, bub.scale=7, col=c("black","red"), pch=16)
#'
setMethod("bubbles", signature(x="formula", data ="FLQuant"),
  function(x, data, bub.scale=2.5, col=c("blue","red"), ...){
    dots <- list(...)
    data <- as.data.frame(data)
    dots$data <- data
    dots$cex <- bub.scale*(abs(data$data)/max(abs(data$data),na.rm=T))+bub.scale*0.1
    dots$col <- ifelse(data$data>0, col[1], col[2])
    dots$panel <- function(x, y, ..., cex, subscripts){
      panel.xyplot(x, y, cex=cex[subscripts], ...)
    }
    call.list <- c(x=x, dots)
    ans <- do.call("xyplot", call.list)
    return(ans)
  }
)

#' @rdname bubbles
setMethod("bubbles", signature(x="formula", data ="data.frame"),
function(x, data, bub.scale=2.5, col=c("blue","red"), ...){
dots <- list(...)
  datanm <- as.character(as.list(x)[[2]])
dots$data <- data
dots$cex <- bub.scale*(abs(data[,datanm])/max(abs(data[,datanm]),na.rm=T))+bub.scale*0.1
dots$col <- ifelse(data[,datanm]>0, col[1], col[2])
dots$panel <- function(x, y, ..., cex, subscripts){
panel.xyplot(x, y, cex=cex[subscripts], ...)
}
call.list <- c(x=x, dots)
ans <- do.call("xyplot", call.list)
ans
})

#' @rdname bubbles
setMethod("bubbles", signature(x="formula", data ="FLCohort"),
    function(x, data, bub.scale=2.5, ...){
      dots <- list(...)
      data <- as.data.frame(data)
      dots$data <- data
      dots$cex <- bub.scale*data$data/max(data$data, na.rm=TRUE)+0.1
      pfun <- function(x, y, ..., cex, subscripts){
        panel.xyplot(x, y, ..., cex = cex[subscripts])
    }
      call.list <- c(x = x, dots, panel=pfun)
      xyplot <- lattice::xyplot
      ans <- do.call("xyplot", call.list)
      ans$call <- match.call()
      ans
    }
)

#' @rdname bubbles
setMethod("bubbles", signature(x="formula", data ="FLQuants"),
  function(x, data, bub.scale=2.5, bub.col=gray(c(0.1, 0.1)), ...){
	# data
	dots <- list(...)
	dots$data <- as.data.frame(data)

	# def col & pch to plot negative values
	col <- as.numeric(dots$data$data>=0)
	coln <- vector(mode="character", length=length(col))
	pchn <- vector(mode="integer", length=length(col))

	# for negs
	coln[col==0] <- bub.col[1]
	pchn[col==0] <- 1

	# for pos
	coln[col==1] <- bub.col[2]
	pchn[col==1] <- 19

	# for NA
	coln[coln==""] <- NA
	pchn[coln==""] <- NA

	# rescale cex to make it prety (I hope)
	cex0 <- abs(dots$data$data)
	cex <- bub.scale*cex0/max(cex0, na.rm=TRUE)

	# panel
	pfun <- function(x,y,subscripts,...){
		panel.xyplot(x,y,col=coln[subscripts], pch=pchn[subscripts], cex=cex[subscripts]+bub.scale*0.1)
	}

	# legend

	akey <- bkey(list(data=dots$data$data, cex=cex, bub.col=bub.col, bub.scale=bub.scale), border=F, title="Scale", cex.title=0.8, padding.text=4)

	# call list
	dots$cex <- cex
	dots$pch <- pchn
	dots$col <- coln
	dots$panel <- pfun
	dots$key <- akey
	call.list <- c(x = x, dots)

	# plot
	ans <- do.call("xyplot", call.list)
	ans

})

# }}}

# wireframe {{{

#' Method wireframe
#'
#' 3D plot for FLQuant objects
#' 
#' Method to plot 3D representations of FLQuant objects
#'
#' @name wireframe
#' @aliases wireframe wireframe,FLQuant-method
#' @docType methods
#'
#' @param x a \code{formula} formula for lattice
#' @param data a \code{FLQuant} object with the values
#' @param ... Additional argument list to be passed to \code{wireframe}
#' @return a \code{wireframe} plot
#' @examples
#' 
#' data(ple4)
#' wireframe(data~age+year, data=harvest(ple4))
#'

setMethod("wireframe", c("formula","FLQuant"),
  function(x, data, ...) {
    args <- list(...)
    args$x <- x
    args$data <- as.data.frame(data)
    do.call("wireframe", args)
  }
) # }}}

# ccplot  {{{
setMethod("ccplot", signature(x="formula", data ="FLCohort"), function(x, data, ...){

    dots <- list(...)
  # define a suitable xlim based on years
  if(all.vars(x)[2]=="year"){
    ys <- dimnames(data)$cohort[dim(data)[1]]
      ye <- dimnames(data)$cohort[dim(data)[2]]
    xlim <- c(as.numeric(ys), as.numeric(ye)+2) 
      dots$xlim <- xlim
  }
  # now data coerce
    data <- as.data.frame(data)
  # some options
    data$year <- data$cohort + data$age
    dots$data <- data
    dots$groups <- data$cohort
  # call & run
    call.list <- c(x = x, dots)
    xyplot <- lattice::xyplot
    ans <- do.call("xyplot", call.list)
    ans

})  # }}}

# bkey   {{{
setMethod("bkey", signature("list"), function(object, ...){

	# test
	if(sum(names(object) %in% c("data", "cex", "bub.col", "bub.scale"))!=4){
		stop("The list passed to bkey must have components \"data\", \"cex\", \"bub.col\" and \"bub.scale\"","\n")
	}

	# get info
	dots <- list(...)
	data <- object$data
	adata <- abs(data)
	cex <- object$cex
	bub.col <- object$bub.col
	bub.scale <- object$bub.scale

	# vectors with cex, col, pch and text
	v <- ceiling(max(adata, na.rm=T))
	ktext <- format(round(seq(-v,v,l=9),2))
	v <- ceiling(max(cex, na.rm=T))
	kcex <- abs(seq(-v,v,l=9))+bub.scale*0.1
	kcol <- rep(bub.col,c(4,5))
	kpch <- rep(c(1,19),c(4,5))

	# the key
	akey <- simpleKey(text=ktext[9:1], points=T, lines=F, columns=1, space="right")
	akey$points$col=kcol[9:1]
	akey$points$pch=kpch[9:1]
	akey$points$cex=kcex
	akey$text$cex=0.8

	# merging with other arguments
	lst0 <- dots[names(dots) %in% names(akey)]
	lst1 <- dots[!(names(dots) %in% names(akey))]
	akey[match(names(lst0),names(akey),nomatch=0)] <- lst0
	akey <- c(akey,lst1)

	# output
	akey
})  # }}}
