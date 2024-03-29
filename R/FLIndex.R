# FLIndex.R - FLIndex class and methods
# FLCore/R/FLIndex.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC D02

# Accesors {{{
invisible(createFLAccesors("FLIndex", exclude=c('name', 'desc', 'range', 'effort'))) # }}}

# FLIndex   {{{

#' @rdname FLIndex
#' @aliases FLIndex,FLQuant-method
setMethod('FLIndex', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
		units(object) <- 'NA'
    qobject <- quantSums(object)
    
    dims <- dims(object)

    res <- new("FLIndex",
      index=object, index.var=object, catch.n=object, catch.wt=object,
      effort=qobject, sel.pattern=object, index.q=object,
      range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear, startf=as.numeric(NA),
      endf=as.numeric(NA))))
      

    # Load given slots
  	for(i in names(args)) {
      if(i == "range")
        slot(res, i)[names(args[[i]])] <- args[[i]]
      else
  			slot(res, i) <- args[[i]]
    }

    return(res)
  }
)

#' @rdname FLIndex
#' @aliases FLIndex,missing-method
setMethod('FLIndex', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- unlist(lapply(args, is, 'FLQuant'))
    slots <- names(slots)[slots]
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

# is.FLIndex	{{{
is.FLIndex <- function(x)
    return(inherits(x, "FLIndex"))
# }}}

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

# expand  {{{
setMethod('expand', signature(x='FLIndex'),
  function(x, ...)
  {
    args <- list(...)
    quant <- dims(x)$quant

    # if 'quant' is to be expanded, need to consider no-quant slots
    if(quant %in% names(args))
    {

      # slots where age cannot be changed
      nquant <- c('effort')

      # full FLQuant(s)
      squant <- c('index', 'index.var', 'catch.n', 'catch.wt',
        'sel.pattern', 'index.q')

      # apply straight to all but nquant
      x <- qapply(x, expand, exclude=nquant, ...)

      # apply to nquant, but ignore first dim
      args <- args[!names(args)%in%quant]
      x <- do.call(qapply, c(list(X=x, FUN=expand, exclude=squant), args))

      # range
      range <- qapply(x, function(x) dimnames(x)[[1]])
      slot <- names(which.max(lapply(range, length)))
      dnames <- dimnames(slot(x, slot))
      range(x, c('min', 'max', 'minyear', 'maxyear')) <- c(as.numeric(dnames[[1]][1]),
        as.numeric(dnames[[1]][length(dnames[[1]])]), as.numeric(dnames[[2]][1]),
        as.numeric(dnames[[2]][length(dnames[[2]])]))

    }
    else
    {
      x <- qapply(x, expand, ...)

      if('year' %in% names(args))
      {
        years <- dimnames(slot(x, 'index'))[[2]]
        range(x, c('minyear', 'maxyear')) <- c(as.numeric(years[1]),
          as.numeric(years[length(years)]))
       }
    }
    return(x)
  }
) # }}}

# biomass {{{
setMethod("biomass", signature(x="FLIndex"),
  function(x) {
    quantSums(index(x) * catch.wt(x))
  }
)
# }}}
