# FLIndex.R - FLIndex class and methods
# FLCore/R/FLIndex.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Richard Hillary, Imperial College London
# $Id$


## Accesors {{{
invisible(createFLAccesors("FLIndex", exclude=c('name', 'desc', 'range', 'effort'))) # }}}

# FLIndex()   {{{
setMethod('FLIndex', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
    qobject <- quantSums(object)

    dims <- dims(object)

    res <- new("FLIndex",
      index=object, index.var=object, catch.n=object, catch.wt=object,
      effort=qobject, sel.pattern=object, index.q=object,
      range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear, startf=as.numeric(NA),
      endf=as.numeric(NA))))
      

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
  }
)

setMethod('FLIndex', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant()
    else
    {
      qslots <- slots[!slots %in% c('effort')]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots]]
    }
    return(FLIndex(object, ...))
  }
) # }}}

## is.FLIndex	{{{
is.FLIndex <- function(x)
    return(inherits(x, "FLIndex"))
# }}}

## as.FLIndex::FLFleet      {{{
setMethod("as.FLIndex", signature(object="FLFleet"),
    function(object, catchname="missing", catchtype="missing", ...) {
    
    # Check if valid fleet
    validObject(object)
    
    # If only one spp in @catch and spp=missing, take it
    if (missing(catchname) && length(object@catches)==1)

    indstock <- 1

    # If spp is character, look for it in @catch
    else if (is.character(catchname)) {
        if(length(object@catches) == 1) {
            if(object@catches[[1]]@name != catchname)
                stop(paste("Catchname ", catchname, "cannot be found in object"))
            else
                indstock <- 1
            } else {

            # get vector of spp in FLCatch
            indstock <- vector()
            for(i in seq(along=object@catches))
                if (object@catches[[i]]@name == catchname) indstock[i] <- i
                    indstock <- indstock[!is.na(indstock)]
                if (length(indstock)==0) stop(paste("Catchname ", catchname,
                    "cannot be found in fleet by as.FLIndex()"))
                if (length(indstock)>0)  stop(paste("More than 1 occurrence of ", catchname,
                    " found in fleet by as.FLIndex()"))
            }

        } else
            stop("stock must be a character string or a number")

        # Output FLIndex object
        # now, indstock contains the catches which need to be put in new Index object.
        # length of indstock==1, so simple copying of slots. If length >1 then calculations are needed

        if (length(indstock)==1){
            fli <- FLIndex(name=paste("catchtype ",
                ifelse((missing(catchtype)||catchtype == "catch"),"catch","landings"),
                "derived from FLfleet, with catchname " , catchname),
                iniFLQuant=object@catches[[indstock]]@landings.n)

            # range (20/5/2005; treated separately because plusgroup is missing in fleet@range
            fli@range["min"] <- object@catches[[indstock]]@range["min"]
            fli@range["max"] <- object@catches[[indstock]]@range["max"]
            fli@range["minyear"] <- object@catches[[indstock]]@range["minyear"]
            fli@range["maxyear"] <- object@catches[[indstock]]@range["maxyear"]
            fli@range["plusgroup"] <- object@catches[[indstock]]@range["max"]

            # q
            fli@index.q <- object@catches[[indstock]]@catchability

            # effort, catch
            fli@effort   <-  object@effort
            if (missing(catchtype) || catchtype == "catch") {
                fli@index    <-  object@catches[[indstock]]@catch.n
                fli@catch.wt <-  object@catches[[indstock]]@catch.wt
            } else if (catchtype == "landings") {
                fli@index    <-  object@catches[[indstock]]@landings.n
                fli@catch.wt <-  object@catches[[indstock]]@landings.wt
            } else
                stop(paste("Catchtype ", catchtype, " not recognized"))
        }
    return(fli)
    }
)   # }}}

## computeCatch (added by EJ)   {{{
setMethod(computeCatch, signature("FLIndex"), function(object){
	catch <- object@catch.n*object@catch.wt
	catch <- quantSums(catch)
	catch
})  # }}}

## trim     {{{
setMethod("trim", signature("FLIndex"), function(x, ...){

	args <- list(...)
  rng<-range(x)

  names <- getSlotNamesClass(x, 'FLArray')
	quant <- quant(slot(x, names[1]))
  c1 <- args[[quant]]
	c2 <- args[["year"]]

    # FLQuants with quant
    for (name in names)
	  {
		#if(name == 'effort')
		if(all(dimnames(slot(x,name))$age=="all"))
		{
			args <- args[names(args)!= quant]
			slot(x, name) <- do.call('trim', c(list(slot(x, name)), args))
		}
		else
			slot(x, name) <- trim(slot(x,name), ...)
	  }
            
  	if (length(c1) > 0) {
    	x@range["min"] <- c1[1]
	    x@range["max"] <- c1[length(c1)]
      if (rng["max"] != x@range["max"])
         x@range["plusgroup"] <- NA
	}
  	if (length(c2)>0 ) {
    	x@range["minyear"] <- as.numeric(c2[1])
	    x@range["maxyear"] <- as.numeric(c2[length(c2)])
  	}

	return(x)
}) # }}}

## dims {{{
setMethod("dims", signature(obj="FLIndex"),
    # Returns a list with different parameters
    function(obj, ...)
	{
    res <- callNextMethod()
    res[['startf']] <- obj@range[["startf"]]
    res[['endf']] <- obj@range[["endf"]]
    return(res)
    }
)    # }}}

# coerce  {{{
setAs("data.frame", "FLIndex",
  function(from)
  {
  lst <- list()
  qnames <- as.character(unique(from$slot))
  for (i in qnames)
    lst[[i]] <- as.FLQuant(from[from$slot==i,-1])
  do.call('FLIndex', lst)
  }
) # }}}


## effort		{{{
setMethod("effort", signature(object="FLIndex", metier="missing"),
	function(object)
    return(slot(object, "effort")))
setReplaceMethod("effort", signature(object="FLIndex", value="FLQuant"),
	function(object, value)
  {
		slot(object, "effort") <- value
    return(object)
  })
# }}}

# plot (FLIndex) {{{
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
)	# }}}

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
  require(grid)   #Addition of text to panels requires the grid package
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
   require(grid)   #Addition of text to panels requires the grid package

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
                panel.fill(col = rgb(colFn(panel.colour),max=255))   #Colour panel based on the coefficient of determination (r^2)
                panel.lmline(x.filtered,y.filtered,lwd=2)
              }                
          } else {  #If not marking significance, always fill panel and draw best fit line
              panel.fill(col = rgb(colFn(panel.colour),max=255))   #Colour panel based on the coefficient of determination (r^2)
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
                panel.fill(col = rgb(colFn(panel.colour),max=255))   #Colour panel based on the coefficient of determination (r^2)
              }                
          } else {  #If not marking significance, always fill panel 
              panel.fill(col = rgb(colFn(panel.colour),max=255))   #Colour panel based on the coefficient of determination (r^2)
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

# '['       {{{
setMethod('[', signature(x='FLIndex'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE)
  {
    qnames <- getSlotNamesClass(x, 'FLQuant')
    dims <- unlist(lapply(qapply(x, dim)[qnames], function(x) max(x[1])))
    slot <- names(dims[dims == max(dims)][1])
		dx <- dim(slot(x, slot))
    args <- list(drop=FALSE)

		if (!missing(i))
    {
      args <- c(args, list(i=i))
      x@range['plusgroup'] <- min(i[length(i)], x@range['plusgroup'])
    }
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
    
    for(q in qnames)
    {
      if(dims[[q]][1] == 1)
        slot(x, q) <- do.call('[', c(list(x=slot(x,q), i=1), args[names(args) != 'i']))
      else
        slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))
    }

    # range
    x@range['min'] <- dims(slot(x, slot))$min
    x@range['max'] <- dims(slot(x, slot))$max
    x@range['minyear'] <- dims(slot(x, slot))$minyear
    x@range['maxyear'] <- dims(slot(x, slot))$maxyear

    return(x)
    }
)   # }}}
