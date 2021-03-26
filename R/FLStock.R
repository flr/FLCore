# FLStock.R - FLStock class and methods
# FLCore/R/FLStock.R

# Copyright 2003-2018 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLStock()   {{{

#' @rdname FLStock
#' @aliases FLStock,FLQuant-method
setMethod('FLStock', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...) {

    args <- list(...)

    # empty object
    # object <- iter(FLQuant(object), 1)
    object[] <- NA
    units(object) <- 'NA'
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

# ssb		{{{

#' @rdname ssb
#' @aliases ssb-FLStock,method
#' @details
#' For an object of class \code{\link{FLStock}}, the calculation of SSB depends
#' on the value of the 'units' attribute in the \code{harvest} slot. If this is
#' in terms of fishing mortality (\code{units(harvest(object)) == 'f'}), and
#' assuming an object structured by age, then SSB is calculated as
#' \deqn{SSB_{y} = \sum\nolimits_{a} N_{a,y} \cdot e^{-(F_{a,y} \cdot Hs_{a,y} + M_{a,y} \cdot Ms_{a,y})} \cdot W_{a,y} \cdot T_{a,y} }{SSB_y = sum_a(N_ay * exp(-(F_ay * Hs_ay + M_ay * Ms_ay)) * W_ay * T_ay)}
#' where \eqn{N_{a,y}}{N_ay} is the abundance in numbers (\code{stock.n}) by
#' age (a) and year (y), \eqn{F_{a,y}}{F_ay} is the fishing mortality (\code{harvest}), 
#' \eqn{Hs_{a,y}}{Hs_ay} is the proportion of fishing mortality before spawning
#' (\code{harvest.spwn}),
#' \eqn{M_{a,y}}{M_ay} is the natural mortality (\code{m}), 
#' \eqn{Ms_{a,y}}{Ms_ay} is the proportion of natural mortality before spawning
#' (\code{m.spwn}),
#' \eqn{W_{a,y}}{W_ay} is the mean weight at age in the stock (\code{m}), and
#' \eqn{T_{a,y}}{T_ay} is the proportion mature at age in the stock (\code{mat}).
#' For \code{\link{FLStock}} objects with other dimensions (\code{area},
#' \code{unit}), the calculation is carried out along those dimensions too. To
#' obtain a global value please use the corresponding summing method.
#'
#' If the harvest slot contains estimates in terms of harvest rates
#' (\code{units(harvest(object)) == "hr"}), SSB will be computed as
#' \deqn{SSB_{y} = \sum\nolimits_{a} N_{a,y} \cdot (1 - H_{a,y} \cdot Hs_{a,y}) \cdot e^{-(M_{a,y} \cdot Ms_{a,y})} \cdot W_{a,y} \cdot T_{a,y} }{SSB_y = sum_a(N_ay * (1 - H_ay * Hs_ay) * exp(-(M_ay * Ms_ay)) * W_ay * T_ay)}
#' where \eqn{H_{a,y}}{H_ay} is the harvest rate (proportion of catch in weight
#' over total biomass).
#'
#' @seealso \code{\link{areaSums}}
#' @examples
#'
#' data(ple4)
#' ssb(ple4)
setMethod("ssb", signature(object="FLStock"),
	function(object, ...) {

		uns <- units(harvest(object))

		if(uns == 'f') {
			return(quantSums(stock.n(object) * exp(-(harvest(object) *
        harvest.spwn(object) + m(object) * m.spwn(object))) *
        stock.wt(object) * mat(object)))

		} else if(uns == 'hr') {
			return(quantSums(stock.n(object) * stock.wt(object) * mat(object) *
				(1 - harvest(object) * harvest.spwn(object)) *
				exp(-m(object) * m.spwn(object))))

  	} else {
		stop("Correct units (f or hr) not specified in the harvest slot")
		}
	}
)	# }}}

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

# tsb		{{{
setMethod("tsb", signature(object="FLStock"),
	function(object, ...) {

		uns <- units(harvest(object))

		if(uns == 'f') {
			return(quantSums(stock.n(object) * exp(-(harvest(object) * harvest.spwn(object) +
				m(object) * m.spwn(object))) * stock.wt(object)))

		} else if(uns == 'hr') {
			stock.n(object) * (1 - harvest(object) * harvest.spwn(object)) *
				exp(-m(object) * m.spwn(object)) * harvest.spwn(object) * stock.wt(object)
  	} else {
		stop("Correct units (f or hr) not specified in the harvest slot")
		}
	}
)	# }}}

# fbar		{{{
setMethod("fbar", signature(object="FLStock"),
 function(object, ...) {

	 rng <- range(object)

	 if (is.na(rng["minfbar"]))
		 rng["minfbar"] <- rng["min"]

	 if (is.na(rng["maxfbar"]))
		 rng["maxfbar"] <- rng["max"]

	 rng["minfbar"] <- max(rng["min"], min(rng["max"], rng["minfbar"]))
	 rng["maxfbar"] <- max(rng["min"], min(rng["max"], rng["maxfbar"]))

	 if(units(harvest(object)) == 'f' || units(harvest(object)) == 'hr')
	    {
		return(quantMeans(harvest(object)[as.character(rng["minfbar"]:rng["maxfbar"]),]))
		  } else
	stop("Correct units (f or hr) not specified in the harvest slot")
	}
)	# }}}

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

      # max
      x@range["max"] <- ds$max

      # minfbar < min
      if(x@range["minfbar"] < ds$min)
        x@range["minfbar"] <- ds$min

      # maxfbar > max
      if(x@range["maxfbar"] < ds$max)
        x@range["maxfbar"] <- ds$max

      # plusgroup > max
      if(x@range["plusgroup"] < ds$max)
        x@range["plusgroup"] <- ds$max

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

    if (missing(i))
			i <- dimnames(x@stock.n)[1][[1]]
		if (missing(j))
			j <- dimnames(x@stock.n)[2][[1]]
 		if (missing(k))
   		k <- dimnames(x@stock.n)[3][[1]]
		if (missing(l))
			l <- dimnames(x@stock.n)[4][[1]]
		if (missing(m))
		  m <- dimnames(x@stock.n)[5][[1]]
		if (missing(n))
			n <- dimnames(x@stock.n)[6][[1]]

	  quants <- list("catch.n", "catch.wt", "discards.n", "discards.wt", "landings.n",
		  "landings.wt", "stock.n", "stock.wt", "m", "mat", "harvest", "harvest.spwn",
      "m.spwn")
    for(q in quants) {
      slot(x, q)[i,j,k,l,m,n] <- slot(value, q)
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

# dim {{{
setMethod("dim", signature(x="FLStock"),
  function(x) {
    return(dim(x@m))
  }
) # }}}

# vb = vulnerable biomass {{{

setMethod("vb", signature(x="FLStock", sel="missing"),
  function(x) {
    
    har <- harvest(x)
    minx <- apply(har, 2:6, min)
    maxx <- apply(har, 2:6, max)
    vn <- stock.n(x) * ((har %-% minx) %/% (maxx-minx))
    vb <- quantSums(vn * stock.wt(x))
    
    return(vb)
  }
)

setMethod("vb", signature(x="FLStock", sel="FLQuant"),
  function(x, sel) {
    
    vb <- quantSums(stock.n(x) * sel * stock.wt(x))
    
    return(vb)
  }
)

# }}}

# simplify {{{

#' @rdname simplify
#' @aliases simplify,FLStock-method

setMethod("simplify", signature(object="FLStock"),
  function(object, dims=c("unit", "season", "area")[dim(object)[3:5] > 1],
    spwn.season=1, stock.season=1, calcF=TRUE) {
    
    # TODO: check spwn.season vs. mat
    
    # DIMS to operate on, inverse of dims
    dms <- seq(1,6)[-(match(dims, c("unit", "season", "area")) + 2)]
  	dmns <- list(season="all", unit="unique", area="unique")[dims]

    last.season <- dims(object)$season
  
    # sum along dms
    foo <- function(x, dims=dms, FUN=sum) {
      return(apply(x, dims, function(x){
      z <- x[!is.na(x)]; ifelse(length(z), FUN(z, na.rm=TRUE), NA)}))
    }

    # SUMS for catch.n, landings.n and discards.n
    can <- foo(catch.n(object), dims=dms)
    lan <- foo(landings.n(object), dims=dms)
    din <- foo(discards.n(object), dims=dms)

    if("season" %in% dims) {
      
      # GET stock.n at stock.season
    	stn <- foo(stock.n(object)[,,, stock.season], dims=dms)
	    dimnames(stn) <- list(season="all")
      
      # TODO ADD mean weighted by abundance
      cawt <- foo(catch.wt(object), dims=dms, FUN=mean)
      lawt <- foo(landings.wt(object), dims=dms, FUN=mean)
      diwt <- foo(discards.wt(object), dims=dms, FUN=mean)
      stwt <- foo(stock.wt(object)[,,,stock.season], dims=dms, FUN=mean)

      mat <- mat(object)[,,,spwn.season]

    } else {

      # SUM stock.n
      stn <- foo(stock.n(object))
      
      cawt <- foo(catch.wt(object), dims=dms, FUN=mean)
      lawt <- foo(landings.wt(object), dims=dms, FUN=mean)
      diwt <- foo(discards.wt(object), dims=dms, FUN=mean)
      stwt <- foo(stock.wt(object), dims=dms, FUN=mean)
    }
    
    # AVERAGE mat
    if("unit" %in% dims & identical(dimnames(mat(object))$unit, c("F", "M"))) {
      if("season" %in% dims) {
        mat <- foo(mat(object)[,,'F', spwn.season], dims=dms, FUN=mean)
      } else {
      mat <- foo(mat(object)[,,'F'], dims=dms, FUN=mean)
      }
    } else {
      mat <- foo(mat(object), dims=dms, FUN=mean)
    }

    # SET new dimnames
    dimnames(cawt) <- dmns
    dimnames(lawt) <- dmns
    dimnames(diwt) <- dmns
    dimnames(stwt) <- dmns
    dimnames(mat) <- dmns
  	
    if("area" %in% dims)
    	mat <- areaMeans(mat)
  	
    # M: weighted mean?
    m <- m(object)
    if("unit" %in% dims)
      m <- unitMeans(m)
    if("season" %in% dims)
      m <- seasonSums(m)
    if("area" %in% dims)
      m <- areaMeans(m)
    
    dimnames(m) <- dmns
  
    # harvest  
    har <- harvest(stn, can, m)
    har[stn == 0] <- 0
    har[can == 0] <- 0
	
    # harvest.spwn & m.spwn
    harvest.spwn <- m.spwn <- m
    units(harvest.spwn) <- units(m.spwn) <- ""
    
    # TODO CHECK all options
    if("unit" %in% dims) {
      harvest.spwn[] <- seasonSums(unitSums(catch(object)[,,,seq(1,spwn.season-1)])) %/%
        seasonSums(unitSums(catch(object)))
    } else if("season" %in% dims) {
      harvest.spwn[] <- seasonSums(catch(object)[,,,seq(1,spwn.season-1)]) %/%
        seasonSums(catch(object))
    } else {
      harvest.spwn <- areaMeans(harvest.spwn(object))
    }

    m.spwn[] <- ((spwn.season - 1) / last.season)
    harvest.spwn[] <- ((spwn.season - 1) / last.season)
  
    res <- FLStock(name=name(object), desc=desc(object), range=range(object),
      catch.n=can, catch.wt=cawt, landings.n=lan, landings.wt=lawt,
      discards.n=din, discards.wt=diwt, stock.n=stn, stock.wt=stwt,
      mat=mat, m=m, m.spwn=m.spwn, harvest=har, harvest.spwn=harvest.spwn)

    landings(res) <- computeLandings(res)
    discards(res) <- computeDiscards(res)
    catch(res) <- computeCatch(res)
    stock(res) <- computeStock(res)

    return(res)
  } 
) # }}}

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
      slot(x, q)[, yrs] <- slot(values, q)
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
#' @param stocks An FLstocks object from a restrospective analysis
#' @param metric Metric to be computed, as a character vector or function
#'
#' @return A metrics of n + 2 x n, where n is the numbers of objects in stocks.

mohnMatrix <- function(stocks, metric="fbar") {

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

  # EXTRACT quantity for given years
  mm <- as.data.frame(lapply(stocks,
    function(x) c(window(do.call(metric, list(x)), start=start, end=end))))
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
#' recruitment, so abundances for the first age will be set as 'NA'.
#'
#' @param object An FLStock with estimated harvest and abundances
#'
#' @return The abundances at age of the survivors, 'FLQuant'.
#'
#' @examples
#' data(ple4)
#' stock.n(ple4[, ac(2002:2006)])
#' survivors(ple4[, ac(2002:2006)])

survivors <- function(object) {

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
#' calculated as everages over the last *wts.nyears*.
#'
#' For stocks spawning later in the year, a value for the avreage fishing
#' mortality, *fbar*, expected in that year can be provided. Mortality until
#' spawning is then calculated with M and selectivity assumed in the extra year
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

  return(list(ssb=quantSums(survivors(x) * exp(- (har * hs) - (mn * ms)) * xwt * xmat),
    n=survivors(x), har=har, hs=hs, mn=mn, ms=ms, xwt=xwt, xmat=xmat))

  return(quantSums(survivors(x) * exp(- (har * hs) - (mn * ms)) * xwt * xmat))

} # }}}

# targets {{{
ssb_end <- function(x) {
  m.spwn(x) <- 1
  harvest.spwn(x) <- 1
  return(ssb(x))
}

ssb_start <- function(x) {
  m.spwn(x) <- 0
  harvest.spwn(x) <- 0
  return(ssb(x))
}

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

biomass <- function(x) {
  stock(x)
}

# }}}
