# FLStock.R - FLStock class and methods
# FLCore/R/FLStock.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Rob Scott, CEFAS
# $Id$


# FLStock()   {{{
setMethod('FLStock', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA
    units(object) <- 'NA'
    qobject <- quantSums(object)

    dims <- dims(object)

    res <- new("FLStock", 
    catch=qobject, catch.n=object, catch.wt=object,
    landings=qobject, landings.n=object, landings.wt=object,
    discards=qobject, discards.n=object, discards.wt=object,
    stock=qobject, stock.n=object, stock.wt=object,
    harvest=object, harvest.spwn=object, m=object, m.spwn=object, mat=object, 
    range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear, minfbar=dims$min, maxfbar=dims$max)))

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
  }
)

setMethod('FLStock', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    argNms<- lapply(args, class)
    slots <- names(argNms)[argNms == 'FLQuant']

    flqs  <- names(argNms)[argNms == 'FLQuants']
    if(length(flqs) != 0)
       for (i in args[[flqs]])
          for (j in names(i))
             object <- i[[j]]

    if(length(slots) == 0)
      object <- FLQuant()
    else{
      qslots <- slots[!slots %in% c('catch','stock','landings','discards')]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots[1]]]}

    return(FLStock(object, ...))})	# }}}

# is.FLStock	{{{
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))	# }}}

## computeLandings	{{{
setMethod("computeLandings", signature(object="FLStock"),
	function(object, na.rm=TRUE) {
        res        <- quantSums(landings.n(object) * landings.wt(object), na.rm=na.rm)
        units(res) <- paste(units(landings.n(object)), "*",  units(landings.wt(object)))
        return(res)
 	} 
)	# }}}

## computeDiscards	{{{
setMethod("computeDiscards", signature(object="FLStock"),
	function(object, na.rm=TRUE) {
        res <- quantSums(discards.n(object)*discards.wt(object), na.rm=na.rm)
        units(res) <- paste(units(discards.n(object)), units(discards.wt(object)))
        return(res)
 	} 
)	# }}}

## computeCatch	{{{
setMethod("computeCatch", signature(object="FLStock"),
  function(object, slot="catch", na.rm=TRUE) {
    if(slot == "n"){
		# NA to 0
      res <- landings.n(object) + discards.n(object)
      if (units(discards.n(object)) == units(landings.n(object)))
			  units(res) <- units(discards.n(object))
    }
    else if(slot == "wt") {
      res <- (landings.wt(object) * landings.n(object) +
        discards.wt(object) * discards.n(object)) /
        (landings.n(object) + discards.n(object))
		  if (units(discards.wt(object)) == units(landings.wt(object)))
		    units(res) <- units(discards.wt(object))
    }
		else if (slot == "all") {
      ctch.n     <-computeCatch(object, slot="n")
      ctch.wt    <-computeCatch(object, slot="wt")
			ctch       <-quantSums(ctch.n*ctch.wt, na.rm=na.rm)
      units(ctch)<-paste(units(ctch.n), units(ctch.wt))

      res <- FLQuants(catch.wt=ctch.wt,
			  catch.n =ctch.n,
        catch   =ctch)
		}
    else {
		  res <- quantSums(catch.n(object) * catch.wt(object), na.rm=na.rm)
        units(res) <- paste(units(catch.n(object)), units(catch.wt(object)))
    }
		return(res)
    }
)	# }}}

## computeStock	{{{
setMethod("computeStock", signature(object="FLStock"),
	function(object, na.rm=TRUE) {
        res <- quantSums(stock.n(object) * stock.wt(object), na.rm=na.rm)
        units(res) <- paste(units(stock.n(object)), "*",  units(stock.wt(object)))
        return(res)
 	} 
)	# }}}

## plot  {{{
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
    obj <- rbind(obj, data.frame(as.data.frame(FLQuants(rec=apply(rec(x),
      c(1,2,6), sum))), panel='recruits'))

    # default options
    options <- list(scales=list(relation='free'), ylab="", xlab="",
      main=ifelse(length(name(x)) > 0, name(x), ""), col='black', lwd=2, cex=0.6,
      box.width=1)
    args <- list(...)
    options[names(args)] <- args

    ## pfun
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

## setPlusGroup function	{{{
#  changes the level of the plus group of the stock object
calc.pg <- function(s., i., k., r., pg., action, na.rm) {
	q.<-slot(s.,i.)

	minage <- s.@range["min"]

	#first check that object actually has ages
	a.<-dimnames(q.)[[quant(q.)]]

	if (any(a.=="all"))
	  return(q.)

	if (any(is.na(a.)) | !all(as.integer(a.)==sort(as.integer(a.))))
	  return(q.)

	pospg <- pg. - as.numeric(dimnames(slot(s., i.))[[1]][1]) + 1

	if (action == "sum"){
	  q.[pospg,,,,]<-apply(q.[r.,],2:6,sum, na.rm=na.rm)
	}
	else{
	  if(action == "wt.mean"){
  	sum.r <- apply(slot(s.,k.)[r.,],2:6,sum, na.rm=na.rm)
		q.[pospg,,,,]<- ifelse(sum.r@.Data == 0, 0, apply(q.[r.,]*slot(s.,k.)[r.,],2:6,sum,na.rm=na.rm)/sum.r)
	  }
	}
	a. <- dimnames(q.)
	q. <- q.[1:pospg,,,,]
	dimnames(q.)[[1]] <- minage:pg.
	dimnames(q.)[2:6] <- a.[2:6]

	return(q.)}

expandAgeFLStock<-function(object,maxage,keepPlusGroup=TRUE,...)
    {
    if (class(object)!="FLStock") stop('not a FLStock object')
    if (!validObject(object)) stop('object not a valid FLStock')

    res <-object
    if (maxage<=range(res,"max")) stop('maxage not valid')
    if (keepPlusGroup) range(res,c("max","plusgroup"))<-maxage else range(res,c("max","plusgroup"))<-c(maxage,NA)

    dmns     <-dimnames(m(res))
    oldMaxage<-dims(res)$max
    dmns$age <-as.numeric(dmns$age[1]):maxage

    Pdiscard<-discards.n(res)[ac(oldMaxage)]/catch.n(res)[ac(oldMaxage)]
    Planding<-landings.n(res)[ac(oldMaxage)]/catch.n(res)[ac(oldMaxage)]

    slts<-c("catch.n",
            "catch.wt",
            "discards.n",
            "discards.wt",
            "landings.n",
            "landings.wt",
            "stock.n",
            "stock.wt",
            "m",
            "mat",
            "harvest",
            "m.spwn",
            "harvest.spwn")

    ## create extra ages and fill with plusgroup
    for (i in slts) {
       dmns$iter  <-dimnames(slot(res,i))$iter
       slot(res,i)<-FLQuant(slot(res,i),dimnames=dmns)
       slot(res,i)[ac((oldMaxage+1):maxage)]<-0;
       slot(res,i)[ac((oldMaxage+1):maxage)]<-sweep(slot(res,i)[ac((oldMaxage+1):maxage)],2:6,slot(res,i)[ac(oldMaxage)],"+")
       }

    ## calc exp(-cum(Z)) i.e. the survivors
    n            <-FLQuant(exp(-apply((slot(res,"m")+slot(res,"harvest"))[ac(oldMaxage:maxage)]@.Data,2:6,cumsum)), quant='age')
    if (print(keepPlusGroup))
       n[ac(maxage)]<-n[ac(maxage)]*(-1.0/(exp(-harvest(res)[ac(maxage)]-m(res)[ac(maxage)])-1.0))
    n            <-sweep(n,2:6,apply(n,2:6,sum),"/")

    stock.n(res)[ac((oldMaxage):maxage)]<-sweep(stock.n(res)[ac((oldMaxage):maxage)],1:6,n,"*")

    z<-harvest(res)[ac(maxage)]+m(res)[ac(maxage)]

    catch.n(res)[   ac((oldMaxage):maxage)]<-sweep(stock.n(res)[ac((oldMaxage):maxage)],2:6,harvest(res)[ac(maxage)]/z*(1-exp(-z)),"*")
    catch.n(   res)[ac((oldMaxage):maxage)]<-sweep(stock.n(res)[ac((oldMaxage):maxage)],2:6,harvest(res)[ac(maxage)]/z*(1-exp(-z)),"*")
    if (dims(discards.n(res))$iter==1 & (dims(catch.n(res))$iter>1 | dims(Pdiscard)$iter>1))
       discards.n(res)<-propagate(discards.n(res),dims(res)$iter)
    if (dims(landings.n(res))$iter==1 & (dims(catch.n(res))$iter>1 | dims(Planding)$iter>1))
       landings.n(res)<-propagate(landings.n(res),dims(res)$iter)
    discards.n(res)[ac((oldMaxage):maxage)]<-sweep(catch.n(res)[ac((oldMaxage):maxage)],2:6,Pdiscard,"*")
    landings.n(res)[ac((oldMaxage):maxage)]<-sweep(catch.n(res)[ac((oldMaxage):maxage)],2:6,Planding,"*")

    ## replace any slots passed in (...)
    args <-names(list(...))
    slots<-getSlots("FLStock")
    for (i in args)
	    if (any(i==names(slots)))
        if (class(list(...)[[i]])==slots[i]) slot(res,i)<-list(...)[[i]]

     if (!validObject(res)) stop("Not valid")

    return(res)
    }

setMethod('setPlusGroup', signature(x='FLStock', plusgroup='numeric'),
  function(x, plusgroup, na.rm=FALSE, keepPlusGroup=TRUE)
	{
	if (!validObject(x)) stop("x not a valid FLStock object")
  if (!keepPlusGroup) range(x,"plusgroup")<-NA
	if (plusgroup>dims(x)$max) return(expandAgeFLStock(x, plusgroup, keepPlusGroup=keepPlusGroup))

	# FLQuants by operation
	pg.wt.mean <-c("catch.wt","landings.wt","discards.wt")
	pg.truncate<-c("harvest","m","mat","harvest.spwn","m.spwn")
	pg.sum	   <-c("catch.n", "landings.n", "discards.n")

  ## problem since you can't calculate plus group if one of these has difffert niters
  if (dims(x)$iter>1){
     PGpairs<-list(c("landings.n","landings.wt"), c("catch.n","catch.wt"),c("discards.n","discards.wt"))
     for (i in 1:length(PGpairs)){
        niters<-unlist(lapply(PGpairs[[i]], function(arg) dims(slot(x,arg))$iter))
        if (length(unique(niters))>1)
           slot(x,PGpairs[[i]][niters==1])<-propagate(slot(x,PGpairs[[i]][niters==1]),iter=dims(x)$iter)
        }

     ## stock.n exists and has niters
     if (sum(x@stock.n, na.rm=TRUE) > 1 && dims(x@stock.n)$iter>1){
        for (i in c(pg.truncate,"stock.wt"))
           if (dims(slot(x,i))$iter==1)
              slot(x,i)<-propagate(slot(x,i),iter=dims(x)$iter)
              }
     }

  # plusgroup calculations
	# xxxx.wt's etc. are condensed into the +gp using the catch if
	# stock.n not available
	# If stock.n available then these are used for f, m, mat & stockwt
	names <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
	maxage <-  dims(x@stock.n)["max"]
	minage <-  dims(x@stock.n)["min"]

	#check plusgroup valid
	if (!missing(plusgroup)){
	  if(plusgroup > maxage){
		 return("Error : new plus group greater than oldest age")
	  }
	  else{
		if(plusgroup < minage)
		  return("Error : new plus group less than youngest age")
	  }
	}
	else
	  return(stock)
	  
	# check fbar range is still valid with new plusgroup
  if (!missing(plusgroup)){
    if(plusgroup <= x@range["maxfbar"]){
      x@range["maxfbar"] <- plusgroup-1
      print("maxfbar has been changed to accomodate new plusgroup")
    }
    if(plusgroup <= x@range["minfbar"]){
      x@range["minfbar"] <- plusgroup-1
      print("minfbar has been changed to accomodate new plusgroup")
    }
  }
  
	#Are stock numbers present?
	stock.n.exist <- sum(x@stock.n, na.rm=TRUE) > 1
	if(stock.n.exist) {
		pg.wt.mean <- c(pg.wt.mean, pg.truncate, "stock.wt")
		pg.wt.by   <- c(pg.sum, rep("stock.n", 7))
		pg.sum	   <- c(pg.sum, "stock.n")
	} else {
		pg.wt.mean <- c(pg.wt.mean,  "stock.wt")
		pg.truncate<- c(pg.truncate, "stock.n")
		pg.wt.by   <- c(pg.sum,  rep("catch.n",7))
	}

	#Perform +grp calcs
	#there are three options wt by stock.n, catch.n or simply add up the values

  #  pg.range <- as.character(plusgroup:stock@range["max"])
	pg.range <- which((x@range[1] : x@range[2]) == plusgroup):length(
		(x@range[1] : x@range[2]))
	x@range["plusgroup"] <- plusgroup
	x@range["max"] <- plusgroup

	#do the weighted stuff first
	for (i in 1 : length(pg.wt.mean)) {
		j <- pg.wt.mean[i]
		k <- pg.wt.by[i]
		slot(x, j) <- calc.pg(x, j, k, pg.range, plusgroup, "wt.mean", na.rm)
	}

	#sum up stuff next
	for (i in pg.sum) {
	  slot(x, i) <- calc.pg(x, i, k, pg.range, plusgroup, "sum", na.rm)
	}

	#then truncate stuff
	if (!stock.n.exist) {
	  for (i in pg.truncate) {
		 slot(x, i) <- calc.pg(x, i, k, pg.range, plusgroup, "truncate", na.rm)
		}
	}
	return(x)
	}
)# }}}

## ssb		{{{
setMethod("ssb", signature(object="FLStock"),
	function(object, ...) {

	if(units(harvest(object)) == 'f')
	{
		res <- colSums(object@stock.n * exp(-object@harvest * object@harvest.spwn -
      object@m * object@m.spwn) * object@stock.wt * object@mat, na.rm=FALSE)
		dim(res) <- c(1, dim(res))
		dmns<-dimnames(object@stock)
		dmns$iter<-dimnames(res)$iter
		return(FLQuant(res, dimnames=dmns))
	} else if(units(harvest(object)) == 'hr')
  {
		res <- colSums(object@stock.n * (1 - object@harvest * object@harvest.spwn) *
      exp(-object@m * object@m.spwn) * object@harvest.spwn * object@mat * object@stock.wt)
		dim(res) <- c(1, dim(res))
		return(FLQuant(res, dimnames=dimnames(object@stock)))
  } else
		stop("Correct units (f or hr) not specified in the harvest slot")
	}
)	# }}}

## tsb		{{{
setMethod("tsb", signature(object="FLStock"),
	function(object, ...) {

	if(units(harvest(object)) == 'f')
	{
		res <- colSums(object@stock.n * exp(-object@harvest * object@harvest.spwn -
      object@m * object@m.spwn) * object@stock.wt, na.rm=FALSE)
		dim(res) <- c(1, dim(res))
		dmns<-dimnames(object@stock)
		dmns$iter<-dimnames(res)$iter
		return(FLQuant(res, dimnames=dmns))
	} else if(units(harvest(object)) == 'hr')
  {
		res <- colSums(object@stock.n * (1 - object@harvest * object@harvest.spwn) *
      exp(-object@m * object@m.spwn) * object@harvest.spwn * object@stock.wt)
		dim(res) <- c(1, dim(res))
		return(FLQuant(res, dimnames=dimnames(object@stock)))
  } else
		stop("Correct units (f or hr) not specified in the harvest slot")
	}
)	# }}}

## fbar		{{{
setMethod("fbar", signature(object="FLStock"),
 function(object, ...) {
  if (is.na(object@range["minfbar"])) object@range["minfbar"]<-object@range["min"]
  if (is.na(object@range["maxfbar"])) object@range["maxfbar"]<-object@range["max"]
  object@range["minfbar"]<-max(object@range["min"],min(object@range["max"],object@range["minfbar"]))
  object@range["maxfbar"]<-max(object@range["min"],min(object@range["max"],object@range["maxfbar"]))

  if(units(harvest(object)) == 'f' || units(harvest(object)) == 'hr')
	    {
		quantMeans(object@harvest[as.character(object@range["minfbar"]:object@range["maxfbar"]),])
		  } else
	stop("Correct units (f or hr) not specified in the harvest slot")
	}
)	# }}}

## sop	{{{
sop <- function(stock, slot="catch") {
	return(quantSums(slot(stock, paste(slot, ".n", sep="")) *
		slot(stock, paste(slot, ".wt", sep=""))) / slot(stock, slot))
}	# }}}

## harvest		{{{
setMethod("harvest", signature(object="FLStock", catch="missing"),
	function(object, index="f") {
		if (!missing(index) && units(slot(object, "harvest")) != index)
			stop("The units of harvest in the object do not match the specified index")
		return(slot(object, "harvest"))
	}
)

## harvest<-
setMethod("harvest<-", signature(object="FLStock", value="character"),
	function(object, value) {
		units(slot(object, "harvest")) <- value
		return(object)
	}
)
setMethod("harvest<-", signature(object="FLStock", value="FLQuant"),
	function(object, value) {
		slot(object, "harvest") <- value
    if(validObject(object))
      return(object)
    else
      stop("Object not valid")
	}
)
setMethod("harvest<-", signature(object="FLStock", value="numeric"),
	function(object, value) {
		slot(object, "harvest")[] <- value
		return(object)
	}
) # }}}

## catch<- FLQuants		{{{
setMethod("catch<-", signature(object="FLStock", value="FLQuants"),
	function(object, value) {
		catch(object)    <- value[['catch']]
		catch.n(object)  <- value[['catch.n']]
		catch.wt(object) <- value[['catch.wt']]
		return(object)
	}
) # }}}

# trim {{{
setMethod("trim", signature(x="FLStock"), function(x, ...){

	args <- list(...)

  rng<-range(x)
  
  c1 <- args[[quant(landings.n(x))]]
	c2 <- args[["year"]]
	c3 <- args[["unit"]]
	c4 <- args[["season"]]
	c5 <- args[["area"]]
	c6 <- args[["iter"]]

    # FLQuants with quant
	names <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])

    for (name in names) {
        if(name %in% c('stock', 'catch', 'landings', 'discards'))
            slot(x,name) <- trim(slot(x,name), year=c2, unit=c3, season=c4,
                area=c5, iter=c6)
        else
            slot(x,name) <- trim(slot(x,name), ...)
    }
            
  if (length(c1) > 0) {
    x@range["min"] <- c1[1]
    x@range["max"] <- c1[length(c1)]
    if (rng["max"] != x@range["max"])
        x@range["plusgroup"] <- NA
  }
  if (length(c2)>0 ) {
    x@range["minyear"] <- c2[1]
    x@range["maxyear"] <- c2[length(c2)]
  }

	return(x)}) # }}}

# ssbpurec  {{{
setMethod("ssbpurec",signature(object="FLStock"),
	function(object, start = "missing", end = "missing", type = "non-param", recs = "missing", spwns = "missing", plusgroup = TRUE, ...) {

		# checks and chose the range over which we calculate the SSB per unit recruit
		if((missing(start) && !missing(end)) | (!missing(start) && missing(end))) 
			stop("Error in ssbpurec: start and end must be supplied together if at all")

		if(missing(start) && missing(end)) 
			x  <- window(object,dims(object@m)[['minyear']],dims(object@m)[['minyear']])

		if(!missing(start) && !missing(end))
			x  <- window(object,start,end)

		if(missing(recs)) 
			recs  <- 1
		if(missing(spwns)) 
			spwns  <- 1 				

		ymin  <- dims(x@m)[['minyear']]
		ymax  <- dims(x@m)[['maxyear']]
		ns  <- dims(x@m)[['season']]
		amin  <- dims(x@m)[['min']]
		amax  <- dims(x@m)[['max']]
		pg  <- dim(x@m)[1]
		
		# if amin = 0 and recs < spwns !!!! cannot happen

		if(amin == 0 && recs < spwns)
			stop("Error: minimum age is zero and the recruitment occurs before spawning - not possible")
		
		if(type == 'non-param') {
			m  <- yearMeans(slot(x, "m"))
			mat  <- yearMeans(slot(x, "mat")) 
			wt  <- yearMeans(slot(x, "stock.wt"))
			n  <- FLQuant(m)
			
			# seasonal or non-seasonal options
			
			if(ns == 1) {
				
				# standard calculation : recruitment = 1, natural mort. sets the 
				# age-structure, with or without a plusgroup

				n[1,1,,1,]  <- 1
				for(a in 2:pg) 
					n[a,1,,1,]  <- n[a-1,1,,1,] * exp(-m[a-1,1,,1,])
				if(plusgroup)
					n[pg,1,,1,]  <- n[pg,1,,1,] / (1-exp(-m[pg,1,,1,]))
				
				rho  <- quantSums(n * mat * wt)

				# always set dimnames$year to be the minyear in the stock object

				dimnames(rho)$year  <- dims(object@m)[['minyear']]
				
			} else {

				# to come...
			}
			
		}

		return(rho)
	}
)# }}}

# '['       {{{
setMethod('[', signature(x='FLStock'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE)
  {
		dx <- dim(slot(x, 'stock.n'))
    args <- list(drop=FALSE)

		if (!missing(i))
    {
      args <- c(args, list(i=i))
      x@range['plusgroup'] <- min(as.numeric(i)[length(i)], x@range['plusgroup'])
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
 
	  quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		  "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
    	"m.spwn")
    for(q in quants)
      slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))

	  quants <- list("catch", "landings", "discards", "stock")
    args[['i']] <- 1
    for(q in quants)
      slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))
        
    # range
    x@range['min'] <- dims(slot(x, 'stock.n'))$min
    x@range['max'] <- dims(slot(x, 'stock.n'))$max
    x@range['minyear'] <- dims(slot(x, 'stock.n'))$minyear
    x@range['maxyear'] <- dims(slot(x, 'stock.n'))$maxyear

    return(x)
    }
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLStock", value="FLStock"),
	function(x, i, j, k, l, m, n, ..., value) {
    if (missing(i))
			i  <-  dimnames(x@stock.n)[1][[1]]
		if (missing(j))
			j  <-  dimnames(x@stock.n)[2][[1]]
 		if (missing(k))
   			k  <-  dimnames(x@stock.n)[3][[1]]
		if (missing(l))
			l  <-  dimnames(x@stock.n)[4][[1]]
		if (missing(m))
			m  <-  dimnames(x@stock.n)[5][[1]]
		if (missing(n))
			n  <-  dimnames(x@stock.n)[6][[1]]

	  quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		  "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
      "m.spwn")
    for(q in quants)
      slot(x, q)[i,j,k,l,m,n] <- slot(value, q)
	    
    quants <- list("catch", "landings", "discards", "stock")
    for(q in quants)
      slot(x, q)[1,j,k,l,m,n] <- slot(value,q)

    if(validObject(x))
      return(x)
    else
      stop("Object is not valid")
	}
)   # }}}

# coerce  {{{
setAs("data.frame", "FLStock",
  function(from)
  {
  lst <- list()
  qnames <- as.character(unique(from$slot))
  for (i in qnames)
    lst[[i]] <- as.FLQuant(from[from$slot==i,-1])
  do.call('FLStock', lst)
  }
) # }}}

# rec(FLStock)  {{{
setMethod('rec', signature(object='FLStock'),
  function(object, rec.age=as.character(object@range["min"]))
  {
    if(dims(object)$quant != 'age')
      stop("rec(FLStock) only defined for age-based objects")
    if(length(rec.age) > 1)
      stop("rec.age can only be of length 1")
    res <- stock.n(object)[rec.age,]
    return(res)
  }
) # }}}

# mergeFLStock {{{
mergeFLStock<-function(x, y)
    {
    if (!all(unlist(dims(x))==unlist(dims(y)))) stop("FLStock objects to combine have dim mismatch")

    res<-FLStock(stock     =stock(     x)   +stock(  y),
                 stock.n   =stock.n(   x)   +stock.n(y),
                 catch     =catch(     x)   +catch(  y),
                 catch.n   =catch.n(   x)   +catch.n(y),
                 landings  =landings(  x)+landings(  y),
                 landings.n=landings.n(x)+landings.n(y),
                 discards  =discards(  x)+discards(  y),
                 discards.n=discards.n(x)+discards.n(y))

    name(res) = paste(name(x),"merged with", name(y))
    desc(res) = paste(desc(x),"merged with", desc(y))
    res@range = x@range

    stock.wt(res)   <-(   stock.wt(x)*stock.n(x)      +stock.wt(y)*stock.n(y))/(stock.n(x)+stock.n(y))
    catch.wt(res)   <-(   catch.wt(x)*catch.n(x)      +catch.wt(y)*catch.n(y))/(catch.n(x)+catch.n(y))
    landings.wt(res)<-(landings.wt(x)*landings.n(x)+landings.wt(y)*landings.n(y))/(landings.n(x)+landings.n(y))
    discards.wt(res)<-(discards.wt(x)*discards.n(x)+discards.wt(y)*discards.n(y))/(discards.n(x)+discards.n(y))

#    args <- list(...)

    if (!is.null(args) && any(names(args) == "m"))
       m(res)<-args$m
    else
       {
       warning("adding m slots, this might not be want you want")
       m(res)<-(m(x)+m(y))/2
       }
       
    if (!is.null(args) && any(names(args) == "m.spwn"))
       m.spwn(res)<-args$m.spwn
    else
       {
       warning("adding m.spwn slots, this might not be want you want")
       m.spwn(res)      <-(harvest.spwn(x)+harvest.spwn(y))/2
       }
       
    if (!is.null(args) && any(names(args) == "harvest.spwn"))
       harvest.spwn(res)<-args$harvest.spwn
    else
       {
       warning("adding harvest.spwn slots, this might not be want you want")
       harvest.spwn(res)<-(harvest.spwn(x)+harvest.spwn(y))/2
       }

    mat(res)    <-(mat(x)*stock.n(x)+mat(y)*stock.n(y))/(stock.n(x)+stock.n(y))

    harvest(res)<-FLQuant(harvest(res),dimnames=dimnames(m(res)))

    #harvest(res)<-calcF(m(res),catch.n(res),stock.n(res))

    return(res)
    }
# }}}

# "+"      {{{
setMethod("+", signature(e1="FLStock", e2="FLStock"),
	function(e1, e2) {
    if(validObject(e1) & validObject(e2))
      return(mergeFLStock(e1, e2))
    else
      stop("Input objects are not valid: validObject == FALSE")
    }
) # }}}

# expand  {{{
setMethod('expand', signature(x='FLStock'),
  function(x, ...)
  {
    args <- list(...)
    quant <- dims(x)$quant

    # if 'quant' is to be expanded, need to consider no-quant slots
    if(quant %in% names(args))
    {

      # slots where age cannot be changed
      nquant <- c('catch', 'landings', 'discards', 'stock')
    
      # full FLQuant(s)
      squant <- c('catch.n', 'catch.wt', 'discards.n', 'discards.wt',
        'landings.n', 'landings.wt', 'stock.n', 'stock.wt', 'm', 'mat',
        'harvest', 'harvest.spwn', 'm.spwn')

      # apply straight to all but nquant
      x <- qapply(x, expand, exclude=nquant, ...)
    
      # apply to nquant, but ignore first dim
      args <- args[!names(args)%in%quant]
      x <- do.call(qapply, c(list(X=x, FUN=expand, exclude=squant), args))

      # warn about plusgroup
      if(!is.na(range(x, 'plusgroup')) && quant == 'age')
        warning("Consider calling setPlusGroup to extend along the 'age' dimension")

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
        years <- dimnames(slot(x, 'stock.n'))[[2]]
        range(x, c('minyear', 'maxyear')) <- c(as.numeric(years[1]),
          as.numeric(years[length(years)]))
       }
    }
    return(x)
  }
) # }}}

# dimnames {{{
setMethod('dimnames<-', signature(x='FLStock', value='list'),
  function(x, value)
  {
    slots <- getSlotNamesClass(x, 'FLQuant')
    aslots <- c('catch', 'landings', 'discards', 'stock')
    for(i in slots[!slots %in% aslots])
      dimnames(slot(x, i)) <- value

    # range
    vnames <- names(value)
    if('year' %in% vnames)
      range(x, c('minyear','maxyear')) <- value[['year']][c(1, length(value[['year']]))]
    if(dims(x)$quant %in% vnames)
      range(x, c('min','max')) <- value[[dims(x)$quant]][c(1,
        length(value[[dims(x)$quant]]))]

    value <- value[names(value) != dims(x)$quant]
    if(length(value) > 0)
    {
      for (i in aslots)
        dimnames(slot(x, i)) <- value
    }

    return(x)
  }
) # }}}

# fapex {{{
setMethod("fapex", signature(x="FLStock"),
  function(x, ...)
  {
    return(apply(harvest(x), 2:6, max))
  }
)
setMethod("fapex", signature(x="FLQuant"),
  function(x, ...)
    return(apply(x, 2:6, max)))
# }}}

# r {{{
setMethod("r", signature(m="FLStock", fec="missing"),
	function(m, by = 'year', method = 'el',...) {
    do.call('r', list(m=m(m), fec=mat(m), by=by, method=method))
	}
) # }}}

# survprob {{{
# estimate survival probabilities by year or cohort
setMethod("survprob", signature(object="FLStock"),
	function(object, by = 'year',...) {
		
		# estimate by year
		if(by == 'year')
      return(survprob(m(object)))
		
		# estimate by cohort
    else if(by == 'cohort')
      return(survprob(FLCohort(m(object))))

	}
) # }}}

# sp {{{
setMethod('sp', signature(stock='FLQuant', catch='FLQuant', harvest='missing'),
  function(stock, catch, rel=TRUE)
  {
    dmns <- dimnames(stock)$year
    rng1 <- dmns[-length(dmns)]
    rng2 <- dmns[-1]

    deltaB <- stock[,rng1] - stock[,rng2]

    res <- deltaB + catch[,rng1]
  
    if (rel)
      return(res/stock[,rng1])
     else
      return(res)
  }
)

setMethod('sp', signature(stock='FLStock', catch='missing', harvest='missing'),
	function(stock, rel=TRUE)
  {
    return(sp(stock(stock), catch(stock), rel=rel))
  }
) # }}}

# catchSel {{{
setMethod("catchSel", signature(object="FLStock"),
   function(object,fn="fapex")
    sweep(harvest(object),2:6,do.call(fn,list(object)),"/")
) # }}}


setGeneric("wt<-", function(object,...,value){
  standardGeneric("wt<-")})
setMethod("wt<-", signature(object="FLStock", value="FLQuant"),
  function(object, ..., value) {
    
    recycleFLQuantOverYrs<-function(object,flq){
      ### if averaged over years then expand years
      if (dim(flq)[2]==1 & dim(object)[2]>=1){
         object[]<-rep(c(flq),dim(object)[2])
         return(object)} else
         return(flq)}

		stock.wt(   object)<-recycleFLQuantOverYrs(stock.wt(   object),value)
		catch.wt(   object)<-recycleFLQuantOverYrs(catch.wt(   object),value)
		landings.wt(object)<-recycleFLQuantOverYrs(landings.wt(object),value)
		discards.wt(object)<-recycleFLQuantOverYrs(discards.wt(object),value)

		return(object)})

# z {{{
setMethod("z", "FLStock", function(object, ...) {
  f <- harvest(object)
  if(units(f) != 'f') {
    stop("Your exploitation rate is not defined as F, cannot be added to M")
  } else { 
    return(m(object) + f)
  }
}) # }}}
