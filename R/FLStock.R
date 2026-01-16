# FLStock.R - FLStock class and methods
# FLCore/R/FLStock.R

# Copyright 2003-2023 FLR Team. Distributed under the GPL 2 or later.
# Maintainer: Iago Mosqueira, WMR.

# FLStock()   {{{

#' @rdname FLStock
#' @aliases FLStock,FLQuant-method
setMethod('FLStock', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...) {

    args <- list(...)

    # empty object
    object[] <- NA
    units(object) <- 'NA'
    quant(object) <- 'age'
    qobject <- quantSums(object)
    dims <- dims(object)

    res <- new("FLStock",
    catch=qobject, catch.n=object, catch.wt=object,
    landings=qobject, landings.n=object, landings.wt=object,
    discards=qobject, discards.n=object, discards.wt=object,
    stock=qobject, stock.n=object, stock.wt=object,
    harvest=object, harvest.spwn=FLQuant(object, units=""),
		m=FLQuant(object, units='m'), m.spwn=FLQuant(object, units=""),
		mat=FLQuant(object, units=""),
		range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
		minyear=dims$minyear, maxyear=dims$maxyear, minfbar=dims$min,
    maxfbar=dims$max)))

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

		if(validObject(res))
			return(res)
		else
			stop("Invalid object created, check input FLQuant(s)")
  }
)

#' @rdname FLStock
#' @aliases FLStock,missing-method
setMethod('FLStock', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant

    slots <- unlist(lapply(args, is, 'FLQuant'))
    slots <- names(slots)[slots]
    
    flqs <- unlist(lapply(args, is, 'FLQuants'))
    flqs <- names(flqs)[flqs]

    if(length(flqs) != 0) {
       for (i in args[[flqs]])
          for (j in names(i))
             object <- i[[j]]
    }

    if(length(slots) == 0) {
      object <- FLQuant()
    } else{
      qslots <- slots[!slots %in% c('catch','stock','landings','discards')]
      if(length(qslots) > 0)
        object <- args[[qslots[1]]]
      else
        object <- args[[slots[1]]]
    }
    return(FLStock(iter(object, 1), ...))
  }
)

#' @rdname FLStock
#' @aliases FLStock,FLQuants-method
setMethod('FLStock', signature(object='FLQuants'),
  function(object, ...)
  {
    return(do.call('FLStock', object))
  }
)
# }}}

# is.FLStock	{{{
is.FLStock <- function(x)
	return(inherits(x, "FLStock"))	# }}}

# biomass metrics: ssb, tsb, vb, exb {{{

#' FLStodck biomass metrics
#'
#' @examples
#' data(ple4)
#' # SSB at spawning time
#' ssb(ple4)
#' # SSB at end of time step (year)
#' ssb_end(ple4)
#' # SSB at start of time step (year)
#' ssb_start(ple4)
#' # Vulnerable biomass at start of time step (year)
#' vb(ple4)
#' # Exploitable biomass at start of time step (year)
#' exb(ple4)

# .biomass
.biomass <- function(n, h, ph, m, pm, wt, sel, byage=FALSE) {

  # CALCULATE by harvest 'units'
  uns <- units(h)

  # COMPUTE start of harvest if < m.spwn
  # sh <- ifelse(th < tm, (th * tm) / (1 - th), 1 - th)
  # CALCULATE proportion of harvest to time
  # ph <- ifelse(time < sh, 0, sh + time * (1 - th))
 
  # SET tm to time
  # tm <- time

  # F
  if(uns == 'f') {
    res <- n * exp(-(h * ph + m * pm)) * wt * sel
  # HR
	} else if(uns == 'hr') {
    res <- n * (1 - h * ph) * exp(-m * pm) * wt * sel
  # else NA
  } else {
    res <- quantSums(n) %=% as.numeric(NA)
	}

  if(!byage)
    res <- quantSums(res)

  return(res)
}

# ssb
setMethod("ssb", signature(object="FLStock"),
	function(object, byage=FALSE, ...) {
    
    # PARSE extra arguments
    args <- list(...)
    for(i in names(args))
      slot(object, i)[] <- c(args[[i]])
    
    res <- .biomass(n=stock.n(object), h=harvest(object),
      ph=harvest.spwn(object), m=m(object), pm=m.spwn(object),
      wt=stock.wt(object), sel=mat(object), byage=byage)

    return(unitSums(res))
	}
)	

# ssb_end
ssb_end <- function(object, byage=FALSE, ...) {

  # PARSE extra arguments
  args <- list(...)
  for(i in names(args))
    slot(object, i)[] <- c(args[[i]])

  res <- .biomass(n=stock.n(object), h=harvest(object),
    ph=1, m=m(object), pm=1, wt=stock.wt(object),
    sel=mat(object), byage=byage)

  return(res)
}

# ssb_start
ssb_start <- function(object, byage=FALSE, ...) {

  # PARSE extra arguments
  args <- list(...)
  for(i in names(args))
    slot(object, i)[] <- c(args[[i]])

  res <- .biomass(n=stock.n(object), h=harvest(object),
    ph=0, m=m(object), pm=0, wt=stock.wt(object),
    sel=mat(object), byage=byage)

  return(res)
}

# vb
setMethod("vb", signature(x="FLStock", sel="ANY"),
	function(x, sel, byage=FALSE, ...) {

    # PARSE extra arguments
    args <- list(...)
    for(i in names(args))
      slot(x, i)[] <- c(args[[i]])

    # CALL .biomass with sel = catch.sel
    .biomass(n=stock.n(x), wt=stock.wt(x), h=harvest(x),
      ph=0, m=m(x), pm=0, sel=sel, byage=byage)
  }
)

setMethod("vb", signature(x="FLStock", sel="missing"),
	function(x, byage=FALSE, ...) {

    # PARSE extra arguments
    args <- list(...)
    for(i in names(args))
      slot(object, i)[] <- c(args[[i]])

    # CALL .biomass with sel = catch.sel
    .biomass(n=stock.n(x), wt=stock.wt(x), h=harvest(x),
      m=m(x), ph=0, pm=0, sel=catch.sel(x), byage=byage)
  }
)

# exb
setGeneric("exb", function(x, ...) standardGeneric("exb"))

setMethod("exb", signature(x="FLStock"),
	function(x, sel=catch.sel(x), wt=catch.wt(x), byage=FALSE, ...) {

    # PARSE extra arguments
    args <- list(...)
    for(i in names(args))
      slot(object, i)[] <- c(args[[i]])

    # CALL .biomass with sel = catch.sel
    .biomass(n=stock.n(x), wt=wt, h=harvest(x),
      m=m(x), ph=0, pm=0, sel=sel, byage=byage)
  }
)

# biomass_end
biomass_end <- function(object, byage=TRUE, ...) {

  # PARSE extra arguments
  args <- list(...)
  for(i in names(args))
    slot(object, i)[] <- c(args[[i]])

  # CALL .biomass with mat as sel
  .biomass(n=stock.n(object), wt=stock.wt(object), h=harvest(object),
    m=m(object), ph=1, pm=1, time=1, sel=1, byage=byage)
  }


# tsb
setMethod("tsb", signature(object="FLStock"),
	function(object, time=m.spwn(object), byage=FALSE, ...) {

    # PARSE extra arguments
    args <- list(...)
    for(i in names(args))
      slot(object, i)[] <- c(args[[i]])

    # CALL .biomass with sel = 1
    .biomass(n=stock.n(object), wt=stock.wt(object), h=harvest(object),
      m=m(object), ph=harvest.spwn(object), pm=m.spwn(object), 
      sel=1, byage=byage)
  }
)

# }}}

# setPlusGroup function	{{{
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

expandAgeFLStock<-function(object,maxage,keepPlusGroup=TRUE,...) {

    if (!inherits(object, "FLStock")) stop('not a FLStock object')
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

    # create extra ages and fill with plusgroup
    for (i in slts) {
       dmns$iter  <-dimnames(slot(res,i))$iter
       slot(res,i)<-FLQuant(slot(res,i),dimnames=dmns)
       slot(res,i)[ac((oldMaxage+1):maxage)]<-0;
       slot(res,i)[ac((oldMaxage+1):maxage)]<-sweep(slot(res,i)[ac((oldMaxage+1):maxage)],2:6,slot(res,i)[ac(oldMaxage)],"+")
       }

    # calc exp(-cum(Z)) i.e. the survivors
    n <- FLQuant(exp(-apply((slot(res,"m")+slot(res,"harvest"))[ac(oldMaxage:maxage)]@.Data,2:6,cumsum)), quant='age')
    if (keepPlusGroup)
    	n[ac(maxage)]<-n[ac(maxage)]*(-1.0/(exp(-harvest(res)[ac(maxage)]-m(res)[ac(maxage)])-1.0))
    n <-sweep(n,2:6,apply(n,2:6,sum),"/")

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

    # replace any slots passed in (...)
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

  # problem since you can't calculate plus group if one of these has difffert niters
  if (dims(x)$iter>1){
     PGpairs<-list(c("landings.n","landings.wt"), c("catch.n","catch.wt"),c("discards.n","discards.wt"))
     for (i in 1:length(PGpairs)){
        niters<-unlist(lapply(PGpairs[[i]], function(arg) dims(slot(x,arg))$iter))
        if (length(unique(niters))>1)
           slot(x,PGpairs[[i]][niters==1])<-propagate(slot(x,PGpairs[[i]][niters==1]),iter=dims(x)$iter)
        }

     # stock.n exists and has niters
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

# ssf		{{{

setMethod("ssf", signature(object="FLStock"),
	function(object, ...) {

		uns <- units(harvest(object))

		if(uns == 'f') {
			return(quantSums(stock.n(object) * exp(-(harvest(object) * harvest.spwn(object) +
				m(object) * m.spwn(object))) * mat(object)))

		} else if(uns == 'hr') {
			return(quantSums(stock.n(object) * mat(object) *
				(1 - harvest(object) * harvest.spwn(object)) *
				exp(-m(object) * m.spwn(object))))
  	} else {
		stop("Correct units (f or hr) not specified in the harvest slot")
		}
	}
)	# }}}

# fbar		{{{
setMethod("fbar", signature(object="FLStock"),
  function(object, min=range(object, 'minfbar'), max=range(object, 'maxfbar')) {

    if(is.na(min))
      min <- range(object, 'min')

    if(is.na(max))
       max <- range(object, 'max')

    if(units(harvest(object)) == 'f' || units(harvest(object)) == 'hr') {
	  	return(quantMeans(harvest(object)[as.character(seq(min, max)),]))
  	} else {
	  	return(quantMeans(harvest(object)[as.character(seq(min, max)),]))
#    	stop("Correct units (f or hr) not specified in the harvest slot")
    }
  }
)	# }}}

# zbar		{{{
setMethod("zbar", signature(object="FLStock"),
  function(object, min=range(object, 'minfbar'), max=range(object, 'maxfbar')) {

    if(is.na(min))
      min <- range(object, 'min')

    if(is.na(max))
       max <- range(object, 'max')

    if(units(harvest(object)) == 'f' || units(harvest(object)) == 'hr') {
	  	return(quantMeans(z(object)[as.character(seq(min, max)),]))
  	} else {
    	stop("Correct units (f or hr) not specified in the harvest slot")
    }
  }
)	# }}}

# hr {{{
setMethod("hr", signature(object="FLStock"),
 function(object, ...) {
	 
   rng <- range(object)

	 if (is.na(rng["minfbar"]))
		 rng["minfbar"] <- rng["min"]

	 if (is.na(rng["maxfbar"]))
		 rng["maxfbar"] <- rng["max"]

	 rng["minfbar"] <- max(rng["min"], min(rng["max"], rng["minfbar"]))
	 rng["maxfbar"] <- max(rng["min"], min(rng["max"], rng["maxfbar"]))

   rages <- as.character(seq(rng["minfbar"], rng["maxfbar"]))

   out <- (catch.n(object) * catch.wt(object)) /
      (stock.n(object) * stock.wt(object))

    units(out) <- "hr"

  return(quantMeans(out[rages, ]))
 }
)
# }}}

# mbar		{{{
setMethod("mbar", signature(object="FLStock"),
  function(object, min=range(object, 'minfbar'), max=range(object, 'maxfbar')) {

    if(is.na(min))
      min <- range(object, 'min')

    if(is.na(max))
       max <- range(object, 'max')

	  return(quantMeans(m(object)[as.character(seq(min, max)),]))
  }
)	# }}}

# meanage {{{

#' Calculate the mean age in the stock and catch
#'
#' Average age in the stock numbers or catch-at-age.
#'
#' @param object An age-structured FLStock object
#' @return An FLQuant object
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords ts
#' @examples
#' data(ple4)
#' meanage(ple4)
meanage <- function(object) {

  res <- quantSums(stock.n(object) * ages(object)) /
    quantSums(stock.n(object))
  units(res) <- ""
  return(res)
}

#' @rdname meanage
#' @examples
#' meanageCatch(ple4)
meanageCatch <- function(object) {

  res <- quantSums(catch.n(object) * ages(object)) /
    quantSums(catch.n(object))
  units(res) <- ""
  return(res)
}
# }}}

# meanwt {{{

#' Calculate the mean weight in stock and catch
#'
#' Average weight in the stock numbers or catch-at-age.
#'
#' @param object An age-structured FLStock object
#' @return An FLQuant object
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords ts
#' @examples
#' data(ple4)
#' meanwt(ple4)
meanwt <- function(object) {

  res <- quantSums(stock.n(object) * stock.wt(object)) /
    quantSums(stock.n(object))
  return(res)
}

#' @rdname meanwt
#' @examples
#' meanwtCatch(ple4)
meanwtCatch <- function(object) {

  res <- quantSums(catch.n(object) * stock.wt(object)) /
    quantSums(catch.n(object))
  return(res)
}
# }}}

# depletion {{{

#' @examples
#' data(ple4)
#' # Default uses first year as B0
#' depletion(ple4)
#' # B0 can be given
#' depletion(ple4, B0=1.74e6)
#' # and metric changed from 'ssb' default
#' depletion(ple4, metric=tsb)

# TODO: ADD season selection
setMethod("depletion", signature(x="FLStock"),
  function(x, B0=unitSums(do.call(index, list(x))[, 1]), index=ssb) {
    unitSums(do.call(index, list(x))) / c(B0)
  }
)
# }}}

# catchInmature / catchMature {{{

#' Proportion of mature and inmature fish in the catch
#'
#' The proportion in weight of mature and inmature fish in the catch can
#' be computed using catchMature and catchInmature.
#'
#' @param object An age-structured FLStock object
#' @return An FLQuant object
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords ts
#' @rdname catchMature
#' @examples
#' data(ple4)
#' catchInmature(ple4)
catchInmature <- function(object) {

  res <- quantSums(catch.n(object) * (1 - mat(object)) * catch.wt(object))

  return(res)
}

#' @rdname catchMature
#' @examples
#' catchMature(ple4)
catchMature <- function(object) {

  res <- quantSums(catch.n(object) * (mat(object)) * catch.wt(object))

  return(res)
}
# }}}

# sop	{{{
sop <- function(stock, slot="catch") {
	return(quantSums(slot(stock, paste(slot, ".n", sep="")) *
		slot(stock, paste(slot, ".wt", sep=""))) / slot(stock, slot))
}	# }}}

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
#' @rdname Extract
#' @aliases [,FLStock,ANY,ANY,ANY-method
setMethod('[', signature(x='FLStock'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE) {

		dx <- dim(slot(x, 'stock.n'))
    args <- list(drop=FALSE)

		if (!missing(i))
      args <- c(args, list(i=i))
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

    # SUBSET at-age slots
	  quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		  "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
    	"m.spwn")
    for(q in quants)
      slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))

    # SUBSET aggregated slots
	  quants <- list("catch", "landings", "discards", "stock")
    args[['i']] <- 1
    for(q in quants)
      slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))
    
    ds <- dims(x)
    
    # range for non-aggregated FLStock
    if(!is.na(dims(x)$min)) {

      # min
      x@range["min"] <- ds$min
      x@range["minyear"] <- ds$minyear

      # max
      x@range["max"] <- ds$max
      x@range["maxyear"] <- ds$maxyear

      # minfbar < min
      if(x@range["minfbar"] < ds$min)
        x@range["minfbar"] <- ds$min

      # maxfbar > max
      if(x@range["maxfbar"] > ds$max)
        x@range["maxfbar"] <- ds$max

      # plusgroup > max
      if(!is.na(x@range["plusgroup"]))
        if(x@range["plusgroup"] > ds$max)
          x@range["plusgroup"] <- ds$max

      }

    # RECOMPUTE aggregated slots
		if (!missing(i)) {
      stock(x) <- computeStock(x)
      catch(x) <- computeCatch(x, 'all')
    }

    # year
    x@range['minyear'] <- ds$minyear
    x@range['maxyear'] <- ds$maxyear

    return(x)
    }
)   # }}}

# '[<-'            {{{
#' @rdname Extract
#' @aliases [<-,FLStock,ANY,ANY,FLStock-method
setMethod("[<-", signature(x="FLStock", value="FLStock"),
	function(x, i, j, k, l, m, n, ..., value) {

    dx <- dim(x)

    if (missing(i))
      i <- seq(dx[1])
			# i <- dimnames(x@stock.n)[1][[1]]
		if (missing(j))
      j <- seq(dx[2])
			# j <- dimnames(x@stock.n)[2][[1]]
 		if (missing(k))
      k <- seq(dx[3])
   		# k <- dimnames(x@stock.n)[3][[1]]
		if (missing(l))
      l <- seq(dx[4])
			# l <- dimnames(x@stock.n)[4][[1]]
		if (missing(m)) {
      ms <- seq(dx[5])
      mc <- seq(dim(catch.n(x))[5])
    } else {
      ms <- mc <- m
    }
		  # m <- dimnames(x@stock.n)[5][[1]]
		if (missing(n))
      n <- seq(dx[6])
			# n <- dimnames(x@stock.n)[6][[1]]

    # ASSIGN at age quants
	  quants <- list("stock.n", "stock.wt", "m", "mat",
      "harvest", "harvest.spwn", "m.spwn")
    for(q in quants) {
      slot(x, q)[i,j,k,l,ms,n] <- slot(value, q)
    }

    # ASSIGN catch at age quants, may have fleets as areas
    quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", 
      "landings.n", "landings.wt")
    for(q in quants) {
      slot(x, q)[i,j,k,l,mc,n] <- slot(value, q)
    }

    quants <- list("catch", "landings", "discards", "stock")
    for(q in quants){
      slot(x, q)[1,j,k,l,m,n] <- slot(value,q)
    }

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

#' Extract and modify the recruitment time series
#'
#' Recruitment in number of fish is the first row of the 'stock.n' slot of
#' an age-structured 'FLStock'. These convenience functions allow a clearer
#' syntax when retrieving of altering the content of 'stock.n[rec.age,]', where
#' 'rec.age' is usually the first age in the object.
#'
#' @param object An object of class 'FLStock'
#' @param rec.age What age to extract, defaults to first one. As 'character' to select by name or as 'numeric' by position.
#'
#' @return RETURN Lorem ipsum dolor sit amet
#'
#' @name FUNCTION
#' @rdname FUNCTION
#' @aliases FUNCTION
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#' data(ple4)
#' rec(ple4)
#' # Multiple recruitment by a factor of 2
#' rec(ple4) <- rec(ple4) * 2

setMethod('rec', signature(object='FLStock'),
  function(object, rec.age=as.character(object@range["min"])) {

    if(dims(object)$quant != 'age')
      stop("rec(FLStock) only defined for age-based objects")

    if(length(rec.age) > 1)
      stop("rec.age can only be of length 1")

    # EXTRACT rec as stock.n[rec.age, ]
    res <- stock.n(object)[rec.age,]

    # ASSEMBLE rec vector if seasonal rec (nunits == nseasons > 2) ...
    if((dim(object)[3] > 1) & (dim(object)[3] == dim(object)[4])) {
      # .. AND sequential recruitment (unit 2 recs at season 2, ...)
      if(all(stock.n(object)[1,, 2, 1] <= 1e-6)) {
        res <- Reduce(sbind, lapply(seq(dim(object)[4]), function(i)
          unitSums(stock.n(object)[rec.age,, i, i])))
      }
    }
    return(res)
  }
)

setMethod("rec<-", signature(object="FLStock", value="FLQuant"),
  function(object, value) {

    if(dims(object)$quant != 'age')
      stop("rec(FLStock)<- only defined for age-based objects")

    if(dim(value)[1] > 1)
      stop("Object to assign as 'rec' must have a single 'age'.")
    
    stock.n(object)[1,] <- value

    return(object)
  }
)
# }}}

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
      args <- args[!names(args) %in% quant]
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

# dimnames<- {{{
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
      range(x)[c('minyear','maxyear')] <- value[['year']][c(1, length(value[['year']]))]
    if(dims(x)$quant %in% vnames) {
      range(x)[c('min','max', 'plusgroup')] <- suppressWarnings(as.numeric(
        value[[dims(x)$quant]][c(1, rep(length(value[[dims(x)$quant]])),2)]))
    }

    value <- value[names(value) != dims(x)$quant]

    if(length(value) > 0) {
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

    deltaB <- stock[, rng1] - stock[, rng2]

    res <- deltaB + catch[, rng1]

    if (rel)
      return(res / stock[, rng1])
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

# wt<- {{{
setMethod("wt<-", signature(object="FLStock", value="FLQuant"),
  function(object, ..., value) {
    warning("wt<-(FLStock) is defunct, please use individual methods")
    recycleFLQuantOverYrs<-function(object,flq){
      # if averaged over years then expand years
      if (dim(flq)[2]==1 & dim(object)[2]>=1){
         object[]<-rep(c(flq),dim(object)[2])
         return(object)} else
         return(flq)}

		stock.wt(   object)<-recycleFLQuantOverYrs(stock.wt(   object),value)
		catch.wt(   object)<-recycleFLQuantOverYrs(catch.wt(   object),value)
		landings.wt(object)<-recycleFLQuantOverYrs(landings.wt(object),value)
		discards.wt(object)<-recycleFLQuantOverYrs(discards.wt(object),value)

		return(object)}) # }}}

# sr {{{
setMethod("sr", signature(object="FLStock"),
	function(object, rec.age = dims(stock.n(object))$min, ...) {

	# check rec.age
  	if(rec.age < dims(stock.n(object))$min)
      stop("Supplied recruitment age less than minimum age class")

		# extract rec as stock.n at rec.age
    rec <- object@stock.n[as.character(rec.age),]
		# ssb
    ssb <- ssb(object)

    # now alter stock and recruitment to factor in the recruitement age
    if((dim(rec)[2]-1) <= rec.age)
      stop("FLStock recruitment data set too short")

    rec <- rec[,(1+rec.age):dim(rec)[2]]
    units(rec) <- units(slot(object, "stock.n"))
    ssb <- ssb[,1:(dim(ssb)[2] - rec.age)]

		return(FLQuants(rec=rec, ssb=ssb))
}) # }}}

# catch.sel {{{
setMethod("catch.sel", signature(object="FLStock"),
  function(object) {
    return(harvest(object) %/% (apply(harvest(object), 2:6, max) + 1e-32))
  }
) # }}}

# discards.ratio {{{
setMethod("discards.ratio", signature(object="FLStock"),
	function(object, wts=FALSE) {
    if(wts)
	  	return((discards.n(object) * discards.wt(object)) /
        ((landings.n(object) * landings.wt(object)) +
         (discards.n(object) * discards.wt(object))))
    else
		  return(discards.n(object) / (landings.n(object) + discards.n(object)))
	}
) 

setReplaceMethod("discards.ratio", signature(object="FLStock", value="ANY"),
	function(object, value) {
    landings.n(object) <- 1 - value
    discards.n(object) <- value
    return(object)
	}
) 
# }}}

# discards.sel {{{
setMethod("discards.sel", signature(object="FLStock"),
	function(object) {
		res <- catch.sel(object) * discards.ratio(object)
    res[is.na(res)] <- 0
		return(res %/% apply(res, 2:6, max, na.rm=TRUE))
	}
) # }}}

# landings.sel {{{
setMethod("landings.sel", signature(object="FLStock"),
	function(object) {
		res <- catch.sel(object) * (1 - discards.ratio(object))
    res[is.na(res)] <- 0
    return(res %/% apply(res, 2:6, max, na.rm=TRUE))
	}
) # }}}

# dim {{{
setMethod("dim", signature(x="FLStock"),
  function(x) {
    return(c(dim(x@m)[1:5], max(unlist(qapply(x, function(x) dim(x)[6])))))
  }
) # }}}

# nounit {{{

nounit <- function(stock) {

  # DIMS
  dis <- dim(stock)
  nun <- dis[3]

  # END if no units
  if(nun == 1)
    return(stock)
 
  # DIVISION vectors
  div <- rep(rep(seq(dis[3]), each=prod(dis[1:2])), prod(dis[4:6]))

  # CONVERT to vectors
  dat <- qapply(stock, c)

  # TODO: REPLICATE if length differs

  # SUBSET and rename
  stock <- stock[,,1]
  dimnames(stock) <- list(unit="unique")

  # sum: *.n
  stock.n(stock)[] <- Reduce('+', split(dat$stock.n, div))
  catch.n(stock)[] <- Reduce('+', split(dat$catch.n, div))
  landings.n(stock)[] <- Reduce('+', split(dat$landings.n, div))
  discards.n(stock)[] <- Reduce('+', split(dat$discards.n, div))

  # weighted mean: *.wt, m
  stock.wt(stock) <- Reduce('+', split(dat$stock.wt *
    (dat$stock.n + 1e-36), div))  / (c(stock.n(stock)) + 1e-36 * nun)
  catch.wt(stock) <- Reduce('+', split(dat$catch.wt *
    (dat$catch.n + 1e-36), div))  / (c(catch.n(stock)) + 1e-36 * nun)
  landings.wt(stock) <- Reduce('+', split(dat$landings.wt *
    (dat$landings.n + 1e-36), div))  / (c(landings.n(stock)) + 1e-36 * nun)
  discards.wt(stock) <- Reduce('+', split(dat$discards.wt *
    (dat$discards.n + 1e-36), div))  / (c(discards.n(stock)) + 1e-36 * nun)

  m(stock) <- Reduce('+', split(dat$m * dat$stock.n, div)) /
    c(stock.n(stock) + 1e-36)
 
  # COMPUTE

  catch(stock) <- computeCatch(stock)
  landings(stock) <- computeLandings(stock)
  discards(stock) <- computeDiscards(stock)
  stock(stock) <- computeStock(stock)

  return(stock)
}

# }}}

# weighted.mean {{{

#' @examples
#' data(ple4)
#' x <- FLQuants(landings.wt(ple4), discards.wt(ple4))
#' w <- FLQuants(landings.n(ple4), discards.n(ple4))
#' # Computes weighted mean of landings and discards weights-at-age
#' weighted.mean(x, w)

setMethod("weighted.mean", signature(x="FLQuants", w="FLQuants"),
  function(x, w) {
    Reduce('+', Map('*', x, w)) / Reduce('+', lapply(w, '+', 1e-36))
  })

# noseason {{{

noseason <- function(stock, spwn.season=1, rec.season=spwn.season, 
  weighted=FALSE) {

  # DIMS
  dis <- dim(stock)
  nse <- dis[4]
 
  # END if no seasons
  if(nse == 1)
    return(stock)
 
  # DIVISION vectors
  div <- rep(rep(seq(dis[4]), each=prod(dis[1:3])), prod(dis[5:6]))

  # CONVERT to vectors
  dat <- qapply(stock, c)

  # KEEP rec elements to assign back
  recm <- m(stock)[1,]
  recn <- stock.n(stock)[1,]

  # SUBSET and rename, n as in season 1
  stock <- stock[,,,1]
  dimnames(stock) <- list(season="all")

  # mat
  mat(stock)[] <- split(dat$mat, div)[[spwn.season]]

  # spwn
  harvest.spwn(stock)[] <- m.spwn(stock)[] <- ((spwn.season - 1) / nse)
  
  # means: wt
  if(weighted) {
#    stock.wt(stock) <- Reduce("+", Map("*", split(dat$stock.wt, div),
#      split(dat$stock.n, div))) / Reduce("+", split(dat$stock.n, div))
    catch.wt(stock) <- Reduce("+", Map("*", split(dat$catch.wt, div),
      split(dat$catch.n, div))) / Reduce("+", split(dat$catch.n, div))
    landings.wt(stock) <- Reduce("+", Map("*", split(dat$landings.wt, div),
      split(dat$landings.n, div))) / Reduce("+", split(dat$landings.n, div))
    discards.wt(stock) <- Reduce("+", Map("*", split(dat$discards.wt, div),
      split(dat$discards.n, div))) / Reduce("+", split(dat$discards.n, div))
  } else {
 #   stock.wt(stock) <- Reduce('+', split(dat$stock.wt, div)) / nse
    catch.wt(stock) <- Reduce('+', split(dat$catch.wt, div)) / nse
    landings.wt(stock) <- Reduce('+', split(dat$landings.wt, div)) / nse
    discards.wt(stock) <- Reduce('+', split(dat$discards.wt, div)) / nse
  }
  
  # sums: m, catch
  m(stock) <- Reduce('+', split(dat$m, div))

  # CORRECT Ns at spwn.season for age = 0
  if(dimnames(stock)$age[1] == "0" & rec.season > 1) {
    stock.n(stock)["0",,, 1] <- recn["0",,, rec.season]
    m(stock)["0",,, 1] <- seasonSums(recm["0",,, seq(dis[4]) >= rec.season])
  }

  catch.n(stock) <- Reduce('+', split(dat$catch.n, div))
  landings.n(stock) <- Reduce('+', split(dat$landings.n, div))
  discards.n(stock) <- Reduce('+', split(dat$discards.n, div))
  
  catch(stock) <- computeCatch(stock)
  landings(stock) <- computeLandings(stock)
  discards(stock) <- computeDiscards(stock)
  stock(stock) <- computeStock(stock)
  
  return(stock)

}
 # }}}

# noarea {{{

noarea <- function(stock) {

  old <- stock

  stock <- stock[,,,,1]
  dimnames(stock) <- list(area="unique")

  # sum: *.n

  stock.n(stock) <- areaSums(stock.n(old))
  catch.n(stock) <- areaSums(catch.n(old))
  landings.n(stock) <- areaSums(landings.n(old))
  discards.n(stock) <- areaSums(discards.n(old))

  # weighted mean: *.wt, m
  
  stock.wt(stock) <- areaSums(stock.wt(old) * stock.n(old)) /
    areaSums(stock.n(old))
  stock.wt(stock)[is.na(stock.wt(stock))] <-
    areaMeans(stock.wt(old))[is.na(stock.wt(stock))]

  catch.wt(stock) <- areaSums(catch.wt(old) * catch.n(old)) /
    areaSums(catch.n(old))
  catch.wt(stock)[is.na(catch.wt(stock))] <- 
    areaMeans(catch.wt(old))[is.na(catch.wt(stock))]

  landings.wt(stock) <- areaSums(landings.wt(old) * landings.n(old)) /
    areaSums(landings.n(old))
  landings.wt(stock)[is.na(landings.wt(stock))] <- 
    areaMeans(landings.wt(old))[is.na(landings.wt(stock))]

  discards.wt(stock) <- areaSums(discards.wt(old) * discards.n(old)) /
    areaSums(discards.n(old))
  discards.wt(stock)[is.na(discards.wt(stock))] <- 
    areaMeans(discards.wt(old))[is.na(discards.wt(stock))]

  # m
  m(stock) <- areaSums(m(old) * stock.n(old)) /
    areaSums(stock.n(old))
  m(stock)[is.na(m(stock))] <- areaMeans(m(old))[is.na(m(stock))]

  # mat
  mat(stock) <- areaSums(mat(old) * (stock.n(old) * stock.wt(old))) / 
    areaSums((stock.n(old) * stock.wt(old)))
  mat(stock)[is.na(mat(stock))] <- areaMeans(mat(old))[is.na(mat(stock))]

  # COMPUTE

  catch(stock) <- computeCatch(stock)
  landings(stock) <- computeLandings(stock)
  discards(stock) <- computeDiscards(stock)
  stock(stock) <- computeStock(stock)

  return(stock)
}

# }}}

# simplify {{{

#' @rdname simplify
#' @aliases simplify,FLStock-method

setMethod("simplify", signature(object="FLStock"),
  function(object, dims=c("unit", "season", "area")[dim(object)[3:5] > 1],
    spwn.season=1, rec.season=spwn.season, harvest=TRUE, weighted=FALSE) {
  
  # ORDER: season(unit(area))
  if(any(c("area", 5) %in% dims))
    object <- noarea(object)

  if(any(c("unit", 3) %in% dims))
    object <- nounit(object)

  if(any(c("season", 4) %in% dims))
    object <- noseason(object, spwn.season=spwn.season, rec.season=rec.season,
      weighted=weighted)

  # harvest
  if(harvest) {
    if(units(harvest(object)) == "f") {
      harvest(object) <- harvest(stock.n(object), catch.n(object), m(object))
    } else if (units(harvest(object)) == "hr") {
      harvest(object) <- catch.n(object) / stock.n(object)
      units(harvest(object)) <- "hr"
    }
  }
  return(object)
})
# }}}

# verify {{{

#' @details A set of rules has been defined for the *FLStock* class, available
#' by calling the ruleset method. The verify method for *FLStock* will by default
#' evaluate those rules, as well as any other defined in the call.
#'
#' @rdname verify
#' @param rules Basic set of rules for a given class, as returned by ruleset().
#' @seealso \code{\link{ruleset}}
#' @examples
#' data(ple4)
#' # verify for the standard set of rules for FLStock
#' verify(ple4)
#' # verify a single rule from set
#' verify(ple4, rules=ruleset(ple4, 'anyna'), report=FALSE)
#'
#' # add own rule to set
#' verify(ple4, m = ~m >=0)

setMethod("verify", signature(object="FLStock"),
  function(object, rules=ruleset(object), ..., report=TRUE) {
    do.call(callNextMethod,
      c(list(object), rules, list(...), list(report=report)))
  }
 ) # }}}

# ruleset {{{

#' @details A standard minimal set of rules to check FLStock objects against using the
#' verify method. The included rules are (with names in italics) check that:
#'
#' - there are no NAs in any slot, *anyna*.
#' - *catch.wt*, *landings.wt*, *discards.wt* and *stock.wt* > 0.
#' - *mat*, *m.spwn* and *harvest.spwn* values are between 0 and 1.
#' - *harvest* >= 0.
#' - *cohorts* in the stock.n slot contain decreasing numbers, except for the plusgroup age.
#' @rdname ruleset
#' @examples
#' data(ple4)
#' ruleset(ple4)
#' # Extract single rule by name
#' ruleset(ple4, 'anyna')

setMethod("ruleset", signature(object="FLStock"),
  function(object, ...) {

    rulelist <- list(
  
    # CHECK for NAs
    anyna=list(rule="!anyna", anyna=function(x)
      unlist(qapply(x, function(y) sum(is.na(y), na.rm=TRUE) > 0))),

    # CHECK catch.wt > 0
    catch.wt=~catch.wt > 0,
  
    # CHECK landings.wt > 0
    landings.wt=~landings.wt > 0,

    # CHECK discards.wt > 0
    discards.wt=~discards.wt > 0,

    # CHECK stock.wt > 0
    stock.wt=~stock.wt > 0,

    # CHECK 0 < mat < 1
    mat=~mat <= 1 & mat >= 0,

    # CHECK 0 < harvest.spwn < 1
    harvest.spwn=~harvest.spwn <= 1 & harvest.spwn >= 0,

    # CHECK 0 < m.spwn < 1
    m.spwn=~m.spwn <= 1 & m.spwn >= 0,
    
    # CHECK if m.spwn = 0, harvest.spwn = 0
    # TODO m.spwn=~m.spwn <= 1 & m.spwn >= 0,

    # CHECK harvest >= 0
    harvest=~harvest >= 0,

    # CHECK cohort numbers match (N_c,a > N_c,a+1)
    cohorts=list(rule=~ccohorts(stock.n),
      ccohorts=function(x) {
        if(dim(x)[2] < dim(x)[1])
          return(NA)
        #   DROP plusgroup, SELECT full cohorts
        x <- FLCohort(x)[-dim(x)[1], seq(dim(x)[1], dim(x)[2] - dim(x)[1])]
        # CHECK cohort change in abundances
        return((x[-1, ] / x[-dim(x)[1],]) < 1)
      }),
    
    # CHECK units
    uoms=list(rule="uoms", uoms=function(x)
      uomUnits(unlist(units(x))))
    )

    args <- list(...)
  
    if(length(args) == 0)
      return(rulelist)
    else
      return(rulelist[unlist(args)])
}) # }}}

# append {{{

#' @rdname append-methods
#' @details Attributes like dimnames and *units* will always be taken from the
#' first argument, unless the necessary chnages to dimnames$year
#'
#' @examples
#' # append(FLStock, FLStock)
#' data(ple4)
#' fs1 <- window(ple4, end=2001)
#' fs2 <- window(ple4, start=2002)
#' fs3 <- window(ple4, start=2005)
#'
#' # Appends by dimnames$year
#' stock.n(append(fs1, fs2))
#' 
#' # Appends by dimnames$year with gap (2011:2013)
#' stock.n(append(fs1, fs3))
#' 
#' # Appends inside x
#' stock.n(append(fs1, fs3, after=2000))
#' # Appends after end of x
#' stock.n(append(fs1, fs3, after=2005))

setMethod("append", signature(x="FLStock", values="FLStock"),
  function(x, values, after=dims(values)$minyear-1) {
    
    # EXTEND x if needed
    if(after + dims(values)$year > dims(x)$maxyear)
      x <- window(x, end=after + dims(values)$year)

    yrs <- ac(seq(after + 1, length=dims(values)$year))
    
  	quants <- c("catch.n", "catch.wt", "discards.n", "discards.wt",
      "landings.n", "landings.wt", "stock.n", "stock.wt", "m", "mat",
      "harvest", "harvest.spwn", "m.spwn", "catch", "landings", "discards",
      "stock")

    for(q in quants) {
      slot(x, q) <- append(slot(x, q), slot(values, q))
      # slot(x, q)[, yrs] <- slot(values, q)
    }

    return(x)
  }
) # }}}

# mohnMatrix {{{

#' Generate a matrix to compute Mohn's rho for a single metric
#'
#' A common measure of the strength of stock assessment retrospective
#' patterns is Mohn's rho. This function does not carry out the calculation
#' but returns a matrix with the metrics value for the n restrospective
#' runs, in columns, and n + 2 years, in rows.
#'
#' @param stocks An FLStocks object from a restrospective analysis
#' @param metric Metric to be computed, as a character vector or function
#'
#' @return A metrics of n + 2 x n, where n is the numbers of objects in stocks.

mohnMatrix <- function(stocks, metric="fbar", ...) {

  if(!is(stocks, "FLStocks"))
    stop("Input object must be of class 'FLStocks'")

  # LAST year in stocks
  syrs <- unname(unlist(lapply(stocks, function(x) dims(x)$maxyear)))
  yrs <- seq(max(syrs), by=-1, length=length(syrs))

  # IF no sequence from last, stop
  if(!all.equal(syrs, yrs))
    stop("maxyear of FLStock objects must be a sequence from last.")

  peels <- length(stocks) - 1
  
  end <- dims(stocks[[1]])$maxyear
  start <- dims(stocks[[peels + 1]])$maxyear - 2

  # EXTRACT metric for given years
  mm <- as.data.frame(lapply(stocks, function(x)
      c(window(do.call(metric, c(list(x), list(...))),
        start=start, end=end))))

  rownames(mm) <- seq(start, end)
  colnames(mm) <- c("base", as.character(-seq(1, length=peels)))

  return(mm)
} # }}}

# survivors {{{

#' Calculate the survivors of a stock to the next year.
#'
#' An FLStock object containing estimates of adundance at age ('stock.n') and
#' harvest level at age ('harvest'), is used to bring forward the population
#' by applying the total mortality at age ('z'). No calculation is made on
#' recruitment, so abundances for the first age will be set as 'NA', unless
#' a value is provided.
#'
#' @param object An FLStock with estimated harvest and abundances
#' @param rec Value for recruitment, first age abundance, 'numeric' or 'FLQuant'.'
#'
#' @return The abundances at age of the survivors, 'FLQuant'.
#'
#' @examples
#' data(ple4)
#' stock.n(ple4[, ac(2002:2006)])
#' survivors(ple4[, ac(2002:2006)])

survivors <- function(object, rec=NA) {

  dms <- dims(object)

  # MOVE to one year more
  res <- window(stock.n(object) %=% as.numeric(NA),
    start=dms$minyear + 1, end=dms$maxyear + 1)

  ages <- dimnames(res)$age
  
  # SURVIVORS at end of year
  survs <- stock.n(object) * exp(-z(object))

  # MOVE to age + 1, year + 1
  res[ages[-1], ] <- survs[ages[-length(ages)], ]

  # PLUSGROUP
  res[ages[length(ages)]] <- res[ages[length(ages)]] +
    survs[ages[length(ages)], ]

  res[1,] <- rec

  return(res)
} # }}}

# Funwanted, Fwanted {{{

#' Calculate the discards and landings-associated fishing mortalities
#'
#' Computes the fishing mortality at age (harvest) associated with either
#' landings (*Fwanted*) or discards (*Funwanted*) through the respective
#' proportions at age. The function names reflect the convention used in
#' ICES.
#'
#' @param x An FLStock object, with harvest
#' @param ages Ages over which the respective Fbar calculation applies
#'
#' @return An FLQuant
#'
#' @examples
#' data(ple4)
#' Fwanted(ple4, ages=2:6)
#' Funwanted(ple4, ages=1:3)

Funwanted <- function(x, ages=dimnames(x)$age) {
  quantMeans((discards.n(x)[ac(ages),] / catch.n(x)[ac(ages),]) *
    harvest(x)[ac(ages)])
}

Fwanted <- function(x, ages=dimnames(x)$age) {
quantMeans((landings.n(x)[ac(ages),] / catch.n(x)[ac(ages),]) *
    harvest(x)[ac(ages)])
} # }}}

# ssb_next {{{

#' Calculate next yera's SSB from survivors and Fbar
#'
#' The spawning stock biomass (SSB) of the stock gets calculated from the
#' survivors of the previous year. This provides a value for the first year
#' after the end of the object. Weights-at-age, maturity in this extra year are
#' calculated as averages over the last *wts.nyears*.
#'
#' For stocks spawning later in the year, a value for the average fishing
#' mortality, *fbar*, expected in that year can be provided. Mortality until
#' spawning is then calculated, with M and selectivity assumed in the extra year
#' to be an average of the last *fbar.nyears*.
#'
#' @param x An FLStock object containing estimates of abundance and harvesting.
#' @param fbar The Fbar rate assumed on the extra year. Defaults to 0.
#' @param wts.nyears Number of years in calculation of mean weight-at-age and maturity for the extra year.
#' @param fbar.nyears Number of years in calculation of mean selectivity, natural mortality and fraction of F abnd M before spawning for the extra year.
#'
#' @return An FLQuant.
#'
#' @examples
#' data(ple4)
#' ssb_next(ple4)
#' # Compare with ssb()
#' ssb(ple4)[, ac(2014:2017)] / ssb_next(ple4)[, ac(2014:2017)]

ssb_next <- function(x, fbar=0, wts.nyears=3, fbar.nyears=3) {

  my <- dims(x)$maxyear
  fages <- range(x, c("minfbar", "maxfbar"))
  
  # EXTEND slots and COMPUTE wts.nyears average for extra year

  # mat
  xmat <- window(mat(x)[,-1], end=my + 1)
  xmat[, ac(my + 1)] <- yearMeans(xmat[, ac(seq(my - wts.nyears, my))])

  # wt
  xwt <- window(stock.wt(x)[,-1], end=my + 1)
  xwt[, ac(my + 1)] <- yearMeans(xwt[, ac(seq(my - wts.nyears, my))])

  # SOLVE for fmultiplier, returns harvest from fbar and catch.sel

  if(fbar > 0) {

    f <- function(i) {
      abs(c(quantMeans(i * yearMeans(catch.sel(x)[ac(seq(fages[1], fages[2])),
        ac(seq(my - fbar.nyears, my))])) - c(fbar)))
    }

    fmu <- optimise(f, c(fbar / 5, fbar * 5))$minimum

  } else {

    fmu <- 0
  }
  
  # EXTEND slots and COMPUTE fbar.nyears average for extra year

  # harvest
  har <- window(harvest(x)[,-1], end=my + 1)
  cs <- yearMeans(catch.sel(x)[, ac(seq(my - fbar.nyears + 1, my))])
  har[, ac(my + 1)] <- cs %/% quantMeans(cs[ac(seq(fages[1], fages[2])),]) * fbar

  # DEBUG
  # har[, ac(my + 1)] <- yearMeans(catch.sel(x)[, ac(seq(my - fbar.nyears, my))]) * fmu
  
  # m
  mn <- window(m(x)[,-1], end=my + 1)
  mn[, ac(my + 1)] <- yearMeans(mn[, ac(seq(my - fbar.nyears, my))])

  # m.spawn
  ms <- window(m.spwn(x)[,-1], end=my + 1)
  ms[, ac(my + 1)] <- yearMeans(ms[, ac(seq(my - fbar.nyears, my))])

  # harvest.spawn
  hs <- window(harvest.spwn(x)[,-1], end=my + 1)
  hs[, ac(my + 1)] <- yearMeans(hs[, ac(seq(my - fbar.nyears, my))])

  return(quantSums(survivors(x) * exp(- (har * hs) - (mn * ms)) * xwt * xmat))

} # }}}

# targets {{{
biomass_end <- function(x) {
  m.spwn(x) <- 1
  harvest.spwn(x) <- 1
	return(quantSums(stock.n(x) * exp(-(harvest(x) *
    harvest.spwn(x) + m(x) * m.spwn(x))) * stock.wt(x)))
  }

biomass_spawn <- function(x) {
	return(quantSums(stock.n(x) * exp(-(harvest(x) *
    harvest.spwn(x) + m(x) * m.spwn(x))) * stock.wt(x)))
}

# }}}

# production {{{

#' @details Production can be calculated for an FLStock based on the spawning stock
#' biomass ("ssb"), total biomass ("biomass"), or exploitation ("exploitation").
#' @rdname production
#' @param what One of the production options: "ssb", "biomass", or "exploitation".
#' @examples
#' data(ple4)
#' # For SSB
#' production(ple4, "ssb")
#' # For total biomass
#' production(ple4, "biomass")

setMethod("production", signature(object="FLStock"),
  function(object, what="ssb", ...) {

    miny <- dims(object)$minyear
    maxy <- dims(object)$maxyear

    switch(tolower(substr(what, 1, 1)),
      # ssb
      s = computeCatch(object) + window(ssb(object), start=miny+1, end=maxy+1) -
        ssb(object),
      # biomass
      b = computeCatch(object) + window(stock(object), start=miny+1, end=maxy+1) -
        stock(object),
      # exploitation
      e = {
        biomass <- apply(catch.wt(object) * stock.n(object) * 
          catch.sel(object) %/% apply(catch.sel(object), 1, max), seq(6)[-1], sum)
        computeCatch(object) + window(biomass(object), start=miny+1, end=maxy+1) -
          biomass(object)
      })
  }
)

# }}}

# fwdWindow {{{

#' fwdWindow
#' @rdname fwdWindow
#' @details
#' For 'FLStock'
#' @examples
#' data(ple4)
#' # Use mean of last three years and extend until 2020
#' fut <- fwdWindow(ple4, end=2020)
#' # Check values on catch.wt
#' catch.wt(fut)[, ac(2015:2020)]
#' # Use mean of the 2010:2015 period
#' fut <- fwdWindow(ple4, end=2020, years=2010:2015)
#' # Use last three years mean, but last five for 'wt'
#' fut <- fwdWindow(ple4, end=2020, nsq=3, years=list(wt=5))
#' stock.wt(fut)[, ac(2013:2020)]
#' catch.sel(fut)[, ac(2013:2020)]
#' # Resample from last years for 'wt'
#' fut <- fwdWindow(ple4, end=2020, nsq=3, fun=c(wt='sample'))
#' # Years to resample can be different for 'catch.sel'
#' fut <- fwdWindow(ple4, end=2020, nsq=3,
#'   fun=c(wt='sample', catch.sel='sample'), years=c(wt=10, catch.sel=5))
#' # 'wt' slot has been resampled,
#' stock.wt(fut)[, ac(2015:2020)]
#' # while others have used a 3 year average
#' catch.sel(fut)[, ac(2015:2020)]

setMethod("fwdWindow", signature(x="FLStock", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3, fun=c("mean", "geomean", "sample"),
    years=list(wt=nsq, mat=nsq, m=nsq, spwn=nsq, discards.ratio=nsq,
    catch.sel=nsq)) {

    # DIMS
    dx <- dim(x)

    # PARSE years and add missing elements with defaults
    pyears <- eval(formals()$years)
    pyears[names(years)] <- as.list(years)

    # PARSE years
    pyears <- lapply(pyears, function(y) {
      if (length(y) == 1)
        dimnames(x)$year[seq(dx[2] - y + 1, dx[2])]
      else
        match(y, dimnames(x)$year)
    })

    # EXTEND x with window
    res <- window(x, end=end, extend=TRUE, frequency=1)

    # SET window years
    # BUG: zero
    wyrs <- seq(dx[2] + 1, dim(m(res))[2])

    # EXTRACT 'fun' names and find empty
    inms <- !nzchar(names(fun))

    # IF one argument, USE on all blocks
    if (length(fun) == 1 & !is.list(fun)) {
      funs <- setNames(rep(list(match.arg(fun)), 6), nm=names(pyears))
    # IF 2 or more
    } else {
      # ANY unnamed function? SET as default
      if (sum(inms) == 1) {
        funs <- setNames(rep(fun[inms], 6), nm=names(pyears))
      # ELSE set 'mean'
      } else {
        funs <- setNames(rep("mean", 6), nm=names(pyears))
      }

      # ASSIGN other fun values
      funs[names(fun[!inms])] <- fun[names(fun[!inms])]
    }
    
    # CREATE year sample (iters * no. window years)
    if('sample' %in% funs) {
      ysamp <- sample(pyears$wt, dx[6] * length(wyrs), replace=TRUE)
    }

    # SETUP functions
    funs <- lapply(funs, function(f) switch(f,
      "mean"=yearMeans,
      "geomean"=function(g) exp(yearMeans(log(g))),
      "sample"=function(g)
        return(c(aperm(g[, ysamp,,,,1]@.Data, c(1,3,4,5,6,2))))))

    # wt
    stock.wt(res)[, wyrs] <- funs$wt(stock.wt(res)[, pyears$wt])
    landings.wt(res)[, wyrs] <- funs$wt(landings.wt(res)[, pyears$wt])
    discards.wt(res)[, wyrs] <- funs$wt(discards.wt(res)[, pyears$wt])

    # n (ratios)
    landings.n(res)[, wyrs] <- funs$discards.ratio(
      landings.n(res)[, pyears$discards.ratio] /
      (catch.n(res)[, pyears$discards.ratio] + 1e-16))

    discards.n(res)[, wyrs] <- funs$discards.ratio(
      discards.n(res)[, pyears$discards.ratio] /
      (catch.n(res)[, pyears$discards.ratio] + 1e-16))

    # DO catch.wt from landings.wt and discards.wt to match fwd()
    catch.wt(res)[, wyrs] <- computeCatch(res, 'wt')[, wyrs]

    # m
    m(res)[, wyrs] <- funs$m(m(res)[, pyears$m])

    # mat
    mat(res)[, wyrs] <- funs$mat(mat(res)[, pyears$mat])

    # spwn
    m.spwn(res)[, wyrs] <- funs$spwn(m.spwn(res)[, pyears$spwn])
    harvest.spwn(res)[, wyrs] <- funs$spwn(harvest.spwn(res)[, pyears$spwn])

    # harvest
    if(!identical(pyears$catch.sel, pyears$wt)) {
      ysamp <- sample(pyears$catch.sel, dx[6] * length(wyrs), replace=TRUE)
    }
    harvest(res)[, wyrs] <- funs$catch.sel(harvest(res)[, pyears$catch.sel])

    # RESCALE selectivity, need it if F gone too low (F=0)
    harvest(res)[, wyrs] <- harvest(res)[, wyrs] %/%
      apply(harvest(res)[, wyrs], 2:6, max)

    return(res)
  }
) # }}}

# adjust {{{

#' Recalculate to adjust abundances to F and M
#' 
#' An FLStock object is projected forward using the initial abundances and
#' the total mortality-at-age per timestep. New values for the stock.n and
#' catch.n slots are calculated, assuming that harvest and m are correct.
#' This calculation provides a test of the internal consistency of the object.
#'
#' @param object an \code{FLStock} object
#' @return \code{FLStock} object
#'
#' @seealso \code{\link{harvest}}
#'
#' @docType methods
#' @examples
#' data(ple4)
#' test <- adjust(ple4)
#' # Difference in catch due to estimation error
#' plot(FLStocks(PLE=ple4, TEST=test))

# TODO ADD fbar input, ADD SRR 
# NEEDS stock.n, m, f year 1


setMethod("adjust", signature(object="FLStock"),
  function(object) {

    # DIMS
    dm <- dim(object)

    # EXTRACT slots
    sn <- stock.n(object)
    sm <- m(object)
    sf <- harvest(object)

    # PLUSGROUP
    pg <- sn[dm[1],,, dm[4]] * exp(-sf[dm[1],,, dm[4]] - sm[dm[1],,, dm[4]])
 
    # LOOP over years
    for (i in seq(dm[2] - 1)) {
      # and seasons
      for (j in seq(dm[4])) {
        # IF not last season
        if (j != dm[4]) {
          sn[, i,, j+1] <- sn[, i,, j] * exp(-sf[,i,,j] - sm[,i,,j])
        # IF last season
        } else {
          sn[-1, i+1,, 1] <- sn[-dm[1], i,, j] * exp(-sf[-dm[1], i,, j] - 
            sm[-dm[1], i,, j])
          sn[dm[1], i+1,, 1] <- sn[dm[1], i+1,, 1] + pg[, i,, 1]
        }
      }
    }

  # RECONSTRUCT n
  stock.n(object) <- sn
  
  # and catches/landings/discards

  catch.n(object) <- sn * sf / (sm + sf) * (1 - exp(-sf - sm))
  landings.n(object)[is.na(landings.n(object))] <- 0
  discards.n(object)[is.na(discards.n(object))] <- 0
  
  landings.n(object) <- catch.n(object) * landings.n(object) / 
    (discards.n(object) + landings.n(object))

  discards.n(object) <- catch.n(object) - landings.n(object)
  
  catch(object) <- computeCatch(object)  

  return(object)
  }
) # }}}

# qapply		{{{
setMethod('qapply', signature(X='FLStock', FUN='function'),
	function(X, FUN, ..., exclude=missing, simplify=FALSE) {
		
    FUN <- match.fun(FUN)
  
    slots <- c("catch", "catch.n", "catch.wt", "discards", "discards.n",
      "discards.wt", "landings", "landings.n", "landings.wt", "stock",
      "stock.n", "stock.wt", "m",  "mat", "harvest", "harvest.spwn", "m.spwn")

		if(!missing(exclude))
      slots <- slots[!slots %in% exclude]

    res <- setNames(as.list(slots), nm=slots)

    for(i in seq(slots)) {
      res[[i]] <- do.call(FUN, list(slot(X, slots[i]), ...))
    }
    # RETURN object class if FLQuant elements
    if(is(res[[1]], "FLQuant")) {
    
      for(i in slots) {
        slot(X, i) <- res[[i]]
      }

      dims <- dims(X)
      range <- c(min=dims$min, max=dims$max,
        plusgroup=min(dims$max, X@range['plusgroup']),
		    minyear=dims$minyear, maxyear=dims$maxyear, 
        minfbar=unname(range(X)['minfbar']),
        maxfbar=unname(range(X)['maxfbar']))

      range(X) <- range

      return(X)
    }

    if(simplify)
      res <- unlist(res)

		return(res)
	}
)   # }}}

# summary {{{
setMethod("summary", signature(object="FLStock"),
	function(object, ...){

    callNextMethod()

    # Values
    cat("Metrics: \n")

    metrics <- c("rec", "ssb", "catch", "fbar")

    for(i in metrics) {
      met <- try(iterMedians(do.call(i, list(object))), silent=TRUE)
      if(is(met, "FLQuant"))
        cat(" ", paste0(i, ":"),
          paste(format(range(met), trim=TRUE, digits=2), collapse=' - '),
          paste0(" (", units(met), ")"),
          "\n")
      else
        cat(" ", paste0(i, ": NA - NA (NA)\n"))
    }
  }
) # }}}

# aac {{{

#' @examples
#' acc(ple4)

setMethod("acc", signature(object="FLStock"),
  function(object, metric="catch.n", 
  ages=seq(range(object, 'minfbar'), range(object, 'plusgroup') - 1)) {

  inp <- do.call(metric, list(object))[ages]

  res <- acc(inp)

  units(res) <- 'z'

  return(res)
})
# }}}

# ages {{{
setMethod("ages", signature(object="FLStock"),
  function(object) {
  res <- FLQuant(an(dimnames(object)$age), dimnames=dimnames(object),
    units="")
  return(res)
  }
)
# }}}

# ffwd {{{ 

#' Project forward an FLStock for a fbar target
#'
#' Projection of an FLStock object for a fishing mortality target does not
#' always require the features of fwd().Fast-forward an FLStock object for a fishing mortality yearly target only.
#' 
#' @param object An *FLStock*
#' @param sr A stock-recruit relationship, *FLSR* or *predictModel*.
#' @param fbar Yearly target for average fishing mortality, *FLQuant*.
#' @param control Yearly target for average fishing mortality, *fwdControl*.
#' @param deviances Deviances for the strock-recruit relationsip, *FLQuant*.
#'
#' @return The projected *FLStock* object.
#'
#' @author Iago MOSQUEIRA (MWR), Henning WINKEL (JRC).
#' @seealso \link{fwd}
#' @keywords models
#' @examples
#' data(ple4)
#' sr <- predictModel(model=bevholt, params=FLPar(a=140.4e4, b=1.448e5))
#' # Project for fixed Fbar=0.21
#' run <- ffwd(ple4, sr=sr, fbar=FLQuant(0.21, dimnames=list(year=1958:2017)))
#' plot(run)

ffwd <- function(object, sr, fbar=control, control=fbar, deviances="missing") {

    # HANDLE fwdControl
    if(is(fbar, "fwdControl")) {
      # CHECK single target per year & no max/min
      if(length(fbar$year) != length(unique(fbar$year)))
        stop("ffwd() can only project for yearly targets, try calling FLasher::fwd().")

      # CHECK no max/min
      # TODO: BETTER check
      if(any(is.na(iters(fbar)[, "value",])))
        stop("ffwd() can only handle targets and not min/max limits, try calling FLasher::fwd().")
      
      # CHECK target is fbar/f
      if(!all(fbar$quant %in% c("f", "fbar")))
        stop("ffwd() can only project for f/fbar targets, try calling FLasher::fwd().")
      fbar <- m(object)[1, ac(fbar$year)] %=% fbar$value
    }

    # EXTRACT projection years
    yrs <- match(dimnames(fbar)$year, dimnames(object)$year)

    # PROPAGATE if needed
    nit <- max(c(dim(object)[6], dim(sr)[6], dim(fbar)[6]))
    object <- propagate(object, nit)

    # SET recruitment age
    recage <- dims(object)$min
    nry <- seq(recage)

    # SUBSET for projection years
    obj <- object[, seq(yrs[1] - recage, yrs[length(yrs)])]

    # DIMS
    dm <- dim(obj)
    dms <- dims(obj)

    # EXTRACT slots
    naa <- stock.n(obj)
    maa <- m(obj)
    faa <- harvest(obj)
    sel <- catch.sel(obj)

    # DEVIANCES
    if(missing(deviances)) {
      deviances <- rec(obj) %=% 1
    }

    # PARSE sr
    if(is(sr, "FLQuant")) {
      sr <- predictModel(model=rec~a, params=FLPar(c(sr),
        dimnames=list(params="a", year=dimnames(sr)$year, 
        iter=dimnames(sr)$iter)))
    }
 
    # SUBSET and EXPAND (JIC) if unit > 1
    deviances <- expand(window(deviances, start=dms$minyear, end=dms$maxyear),
      unit=dimnames(obj)$unit)

    # COMPUTE harvest
    fages <- range(object, c("minfbar", "maxfbar"))

    faa[, -nry] <- (sel[, -nry] %/%
      quantMeans(sel[ac(seq(fages[1], fages[2])), -nry])) %*% fbar

    faa[is.na(faa)] <- 0
    
    # COMPUTE SRP multiplier
    waa <- stock.wt(obj)
    mat <- mat(obj)
    msp <- m.spwn(obj)
    fsp <- harvest.spwn(obj)
    srp <- exp(-(faa * fsp) - (maa * msp)) * waa * mat

    # DEAL with potential covars
    covars <- NULL
    if(is(sr, 'FLSR')) {
      if (length(sr@covar) > 0)
        covars <- window(sr@covar, start=dms$minyear, end=dms$maxyear)
    }

    # CHECK for mat > 0 if recage is 0
    if(recage == 0 & any(mat(object)[ac(recage),] > 0))
      warning("Recruitment age in object is '0' and maturity for that age is set greater than 0. Contribution of age 0 SSB to recruitment dynamics is being ignored.")

    # LOOP over obj years (i is new year)
    for (i in seq(dm[2])[-nry]) {

      # n
      naa[-1, i] <- naa[-dm[1], i-1] * exp(-faa[-dm[1], i-1] - maa[-dm[1], i-1])

      # pg
      naa[dm[1], i] <- naa[dm[1], i] +
        naa[dm[1], i-1] * exp(-faa[dm[1], i-1] - maa[dm[1], i-1])

      # rec * deviances
       naa[1, i] <- rep(c(eval(sr@model[[3]],
        c(as(sr@params, 'list'), list(
        ssb=c(colSums(naa[, i - recage, 1] * srp[, i - recage, 1],
          na.rm=TRUE))), lapply(covars, '[', 1, i - recage)))) / dm[3], each=dm[3]) *
        c(deviances[, i])
    }

  # UPDATE stock.n & harvest
  stock.n(object)[, yrs] <- naa[, -nry]
  harvest(object)[, yrs] <- faa[, -nry]
  
  # UPDATE stock,
  stock(object) <- computeStock(object)
  
  # and catch.n
  catch.n(object)[, yrs] <- (naa * faa / (maa + faa) *
    (1 - exp(-faa - maa)))[, -nry]

  # SET landings.n & discards.n to 0 if NA
  landings.n(object)[, yrs][is.na(landings.n(object)[, yrs])] <- 0
  discards.n(object)[, yrs][is.na(discards.n(object)[, yrs])] <- 0
  
  # CALCULATE landings.n from catch.n and ratio
  landings.n(object)[, yrs] <- (catch.n(object) * (landings.n(object) / 
    (discards.n(object) + landings.n(object))))[, yrs]

  # CALCULATE discards
  discards.n(object)[, yrs] <- (catch.n(object) - landings.n(object))[, yrs]

  # COMPUTE average catch.wt
  catch.wt(object)[, yrs] <- weighted.mean(
    FLQuants(L=landings.wt(object), D=discards.wt(object)),
    FLQuants(L=landings.n(object), D=discards.n(object)))[, yrs]

  # COMPUTE catch
  catch(object)[, yrs] <- quantSums(catch.n(object) * catch.wt(object))[, yrs]

  return(object)
}
# }}}

# ageopt {{{

#' Age at which a cohort reaches its maximum biomass, calculated by year
#'
#' The optimal (or critical) age is the transition point when a cohort achieves
#' its maximum biomass in the absemce of fishing, i.e. losses due to natural
#' mortality are now greater than gains due to increase in individual biomass.
#'
#' @param object An object of class 'FLStock'
#'
#' @return The age at which maximum biomass is reached, an 'FLQuant'.
#'
#' @name ageopt
#' @rdname ageopt
#'
#' @author The FLR Team
#' @seealso \link{FLStock}
#' @keywords methods
#' @examples
#' data(ple4)
#' ageopt(ple4)

setMethod("ageopt", signature(object="FLStock"),
  function(object) {
  
    # SET future Fbar to zero
    fbar <- fbar(object)[, -1] %=% 0

    # REMOVE last year
    object <- window(object, start=dims(object)$minyear - 1)
  
    # SET NaA to 1 in first age
    stock.n(object)[1] <- 1

    # PROJECT for fbar target and rec=1
    object <- ffwd(object, fbar=fbar,
      sr=predictModel(model="geomean", params=FLPar(a=1)))[, -1]
  
    # COMPUTE biomass
    res <- stock.wt(object)[, -1] * stock.n(object)[, -1]
  
    # COMPUTE age with max biomass
    if (is.na(range(object, "plusgroup"))) {
      res <- apply(res, c(2:6), function(x) as.numeric(names(x)[x == max(x)]))
    } else {
      res <- apply(res[-dim(res)[1]], c(2:6), function(x)
        as.numeric(names(x)[x==max(x)]))
    }

    units(res) <- ""

    return(res)
  }
)
# }}}

# update(FLStock, ...) {{{

setMethod("update", signature(object="FLStock"),
  function(object, ...) {

    res <- callNextMethod()

    slots <- names(list(...))

    # RECALCULATE aggregates
    if(!"landings" %in% slots)
      landings(res) <- computeLandings(res)
    if(!"discards" %in% slots)
      discards(res) <- computeDiscards(res)
    if(!"catch" %in% slots)
      catch(res) <- computeCatch(res)
    if(!"stock" %in% slots)
      stock(res) <- computeStock(res)

    return(res)
  }
)
# }}}

# computeHarvest, recomputeHarvest {{{

#' Computes fishing mortality from abundances, catches and natural mortality
#'
#' Objects or class 'FLStock' already contain a 'harvest' slot to store
#' estimates of fishing mortality at age, for example those obtained from
#' a stock assessment method. Fishing mortality at age can be recalculated
#' using two methods: 
#'
#' @param x An object of class 'FLStock'.
#' @param units Harvest to be computed as 'f' or 'hr', 'character'.
#'
#' @return An 'FLQuant' with the calculated fishing mortalities at age.
#'
#' @author The FLR Team
#' @seealso [FLStock-class] [harvest()] [FLQuant-class]
#' @keywords manip
#' @examples
#' data(ple4)
#' # Compute 'f' from stock.n and Baranov
#' computeHarvest(ple4)
#' # Recomputes all F at age by solving catch Baranov
#' recomputeHarvest(ple4)

setMethod("computeHarvest", signature(object="FLStock", catch="missing"),
  function(object, units=NULL) {

  # AVOIDS recursive default argument 
  if(is.null(units))
    units <- units(harvest(object))

  res <- switch(match.arg(units, choices=c("f", "hr", "NA")),
    "f"=harvest(stock.n(object), catch.n(object), m(object), recompute=FALSE),
    "NA"=harvest(stock.n(object), catch.n(object), m(object), recompute=FALSE),
    "hr"=catch.n(object) / stock.n(object))

  res[is.na(res)] <- 0

  units(res) <- units

  return(res)
  }
)

recomputeHarvest <- function(x) {
  harvest(stock.n(x), catch.n(x), m(x), recompute=TRUE)
}
# }}}

# discardsRatio {{{

#' Compute the ratio of discards to total catch in numbers or weight
#'
#' A calculation is made of the proportion of discards over total catch at age, 
#' either as numbers (value = 'numbers') or weight (value = 'weight'), or for
#' the total discards and catch in biomass (value = 'total').
#'
#' @param object An object of class 'FLStock'
#' @param value One of 'numbers' (default), 'weight' or 'total'.
#'
#' @return The discards ratio (between 0 and 1), 'FLQuant'
#'
#' @name discardsRatio
#' @rdname discardsRatio
#'
#' @author The FLR Team
#' @seealso [FLStock-class]
#' @keywords arith
#' @examples
#' data(ple4)
#' # Discards ratio at age in numbers
#' discardsRatio(ple4)
#' # Total proportion of discards by year
#' discardsRatio(ple4, value="total")

discardsRatio <- function(object, value=c("numbers", "weight", "total")) {

  # SWITCH over value
  switch(match.arg(value),
  # Numbers at age
  numbers=(discards.n(object) / (catch.n(object))),
  # Weight at age
  weight=(discards.n(object) * discards.wt(object)) /
    (catch.n(object) * catch.wt(object)),
  # Total biomass
  total=(discards(object) / (landings(object) + discards(object)))
  )
} 
# }}}

# fec {{{
setMethod("fec", signature(object="FLStock"),
  function(object) {

    res <- stock.n(object) * exp(-(harvest(object) * 
      (harvest.spwn(object)) + m(object) * (m.spwn(object)))) *
      stock.wt(object) * mat(object)

    units(res) <- ""

    return(res)
  }
)
# }}}

# leslie {{{

#' @examples
#' data(ple4)
#' leslie(ple4)
#' # Subset for a single year matrix
#' leslie(ple4[,'2000'])

setMethod("leslie", signature(object="FLStock", fec="missing"),
  function(object) {

    return(leslie(object, fec(object)))
  }
)

setMethod("leslie", signature(object="FLStock", fec="FLQuant"),
  function(object, fec) {

    # Numbers at age at spawning time
    survivors <- stock.n(object) * exp(-(harvest(object) * 
      (harvest.spwn(object)) + m(object) * (m.spwn(object))))

    return(leslie(survivors, fec))
  }
)
# }}}
