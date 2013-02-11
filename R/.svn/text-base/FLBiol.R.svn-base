# FLBiol - class for representing a natural population
# FLCore/R/FLBiol.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, CEFAS
# $Id$


# FLBiol()   {{{
setMethod('FLBiol', signature(object='FLQuant'),
  function(object, plusgroup=dims(object)$max, ...)
  {
    args <- list(...)

    # empty object
    object[] <- NA

    dims <- dims(object)

    res <- new("FLBiol", 
    n=object, m=object, wt=object, fec=object, spwn=object,
    range = unlist(list(min=dims$min, max=dims$max, plusgroup=plusgroup,
			minyear=dims$minyear, maxyear=dims$maxyear)))

    # Load given slots
  	for(i in names(args))
			slot(res, i) <- args[[i]]

    return(res)
  }
)

setMethod('FLBiol', signature(object='missing'),
  function(...)
  {
    args <- list(...)

    # if no FLQuant argument given, then use empty FLQuant
    slots <- lapply(args, class)
    slots <- names(slots)[slots == 'FLQuant']
    if(length(slots) == 0)
      object <- FLQuant()
    else
      object <- args[[slots[1]]]
    return(FLBiol(object, ...))
  }
) # }}}

## is.FLBiol {{{
# Test if an object is of FLBiol class
is.FLBiol <- function(x)
	return(inherits(x, "FLBiol"))
# }}}

## mean.lifespan {{{
setMethod("mean.lifespan", signature(x="FLBiol"),
	function(x, ref.age = 'missing',...) {
		
		# checks
		if(missing(ref.age))
			ref.age <- dims(m(x))$min 

		if(ref.age >= dims(m(x))$max)
			stop("Error in mean.lifespan: reference age greater than last true age")			
		mm <- trim(m(x),age=ref.age:dims(m(x))$max)
		mm <- yearMeans(mm)
		mm <- seasonSums(mm)

		# assuming last true age's M is the future M
		# apply the actuarial formula for mean lifspan
		# ::
		# function m.lf to be applied to unit, seas

		m.lf <- function(x) {
			xx <- array(rep(NA,1000))
			xx[1:length(x)] <- x[]
			xx[(length(x)+1):1000] <- x[length(x)]
			lf <- 0
			for(i in 1:1000) 
					lf <- lf + prod(exp(-xx[1:i]))
			return(lf)
		}
		
		mm <- apply(mm,2:6,m.lf)

		# return the FLQuant age/year aggregated but with unit, area and iter 
		# specific values of the mean lifespan
		
		return(mm)
	}
)# }}}

## as.FLBiol {{{
setMethod("as.FLBiol", signature(object="FLBiol"),

  function(object, unit  =1:dim(object@n)[3],
                   season=1:dim(object@n)[4],
                   area  =1:dim(object@n)[5]) {

    slotnames <- names(getSlots("FLBiol")[getSlots("FLBiol")=="FLQuant"])
    for(slotname in slotnames){
      s.d <- dim(slot(object, slotname))
      slot(object, slotname) <- slot(object, slotname)[,,pmin(unit,s.d[3]),
                                                         pmin(season,s.d[4]),
                                                         pmin(area,s.d[5])]
    }
    return(object)
  }
)

setMethod("as.FLBiol", signature(object="FLStock"), function(object,...){
	flb <- new("FLBiol")
	flb@name <- object@name
	flb@desc <- object@desc
	flb@range <- object@range
	flb@n <- object@stock.n
	flb@m <- object@m
	flb@wt <- object@stock.wt
	flb@fec <- object@mat
	flb@spwn <- object@m.spwn
    return(flb)
  }
) # }}}

# plot {{{
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

## ssb  {{{
setMethod("ssb", signature(object="FLBiol"),
	function(object, ...)
  {
		res <- quantSums(n(object) * wt(object) * fec(object) * exp(-spwn(object) * 
      m(object)), na.rm=FALSE)
    units(res) <- paste(units(n(object)), units(wt(object)), sep=' * ')
    return(res)
  }
)	# }}}

## tsb  {{{
setMethod("tsb", signature(object="FLBiol"),
	function(object, ...)
  {
		res <- quantSums(n(object) * wt(object) * exp(-spwn(object) * 
      m(object)), na.rm=FALSE)
    units(res) <- paste(units(n(object)), units(wt(object)), sep=' * ')
    return(res)
  }
)	# }}}

## computeStock  {{{
setMethod("computeStock", signature(object="FLBiol"),
	function(object, ...)
		return(quantSums(n(object) * wt(object) , ...))
)	# }}}

## ssn  {{{
setMethod("ssn", signature(object="FLBiol"),
	function(object, ...)
		return(quantSums(n(object) * fec(object) * exp(-spwn(object) * m(object)), ...))
)	# }}}

# harvest {{{
setMethod('harvest', signature(object='FLBiol', catch='missing'),
  function(object, fratio=1)
    {
    now <- object@n
    dims <- dim(now)
    res <- now
    res[1:(dims[1]-1), 1:(dims[2]-1)] <- now[2:dims[1], 2:dims[2]]

    res <- log(now/res)

    # last age as previous
    res[dims[1],] <- res[dims[1]-1,]

    # trim out last year
    res <- res[,1:(dims[2]-1)]-m(object)[,1:(dims[2]-1)]

  ##Plusgroup stuff
  pgF<-function(object, hrvst, a=1)
  {

    #deriv(y~n1*exp(-f-m2)+n2*exp(-f*a-m2)-n3,"f")
    d.<-function(f,n1,n2,n3,m1,m2,a=1){
            .expr1 <- -f
            .expr4 <- n1 * exp(.expr1 - m2)
            .expr7 <- exp(.expr1 * a - m2)
            .value <- .expr4 + n2 * .expr7 - n3
            .grad <- array(0, c(length(.value), 1L), list(NULL, c("f")))
            .grad[, "f"] <- -(n2 * (.expr7 * a) + .expr4)
            attr(.value, "gradient") <- .grad

            return(.value)
        }

    for (i in 1:(dims(hrvst)$year)){
      n1<-c(n(object)[ac(range(object,"plusgroup")-1),i])
      n2<-c(n(object)[ac(range(object,"plusgroup"))  ,i])
      n3<-c(n(object)[ac(range(object,"plusgroup"))  ,i+1])

      m1<-c(m(object)[ac(range(object,"plusgroup")-1),i])
      m2<-c(m(object)[ac(range(object,"plusgroup"))  ,i])

      x    <-0.1
      f.   <-10
      Iters<-0
      while (abs(f.) >= 10e-10 && Iters <= 50)
        {
        Iters<-Iters+1
        res<-d.(x,n1,n2,n3,m1,m2,a)

        f.   = res[[1]]
        dfdx = attr(res,"gradient")

        x = (x - f./dfdx)
        }

      hrvst[ac(range(object,"plusgroup"))  ,i]<-x
      hrvst[ac(range(object,"plusgroup")-1),i]<-x*a
      }

    return(hrvst)
    }
    
    if (("plusgroup" %in% names(range(object)) && !is.na(range(object,"plusgroup"))))
     res<-pgF(object, res, a=fratio)

    units(res) <- 'f'

    return(res)
  }
) # }}}

# leslie {{{
# this method applies the Leslie Matrix-type model to an FLBiol object
# ::
# this is just for the year and age version as tweeks will be needed for 
# sexually dimorphic and seasonal models
setMethod("leslie", signature(object="FLBiol"),
	function(object, plusgroup = FALSE, ...) {
		
		# create arrays with no dimnames to speed things up
		
		xx <- object
		dms.n <- c(dim(n(xx)))
		n <- array(dim=dms.n)
		pm <- array(dim=dms.n)
		m.spwn <- array(dim=dms.n)
		fec <- array(dim=dms.n)

		n[] <- n(xx)@.Data[]
		pm[] <- exp(-m(xx)@.Data[])
		m.spwn[] <- spwn(xx)@.Data[]
		fec[] <- fec(xx)@.Data[]

		amax <- dms.n[1]
		ymax <- dms.n[2]

		if(is.na(n[1,1,1,1,1,])) 
			stop("Error in leslie: initial population number is missing")

		# first set the eqm levels of n based on R0 and the survival schedule

		for(a in 2:amax) 
			n[a,1,1,1,1,] <- n[a-1,1,1,1,1,] * pm[a-1,1,1,1,1,]

		if(plusgroup)
			n[amax,1,1,1,1,] <- n[amax,1,1,1,1,]/(1-pm[amax,1,1,1,1,])

		for(y in 2:ymax) {
			
			# standard Leslie matrix dynamics

			n[-c(1),y,1,1,1,] <- n[-c(amax),y-1,1,1,1,] * pm[-c(amax),y-1,1,1,1,]

			if(plusgroup)
				n[amax,y,1,1,1,] <- n[amax,y-1,1,1,1,] * pm[amax,y-1,1,1,1,]

			# now recruits given by usual equations
	
			tmp <- FLQuant(n * fec * (1 - m.spwn * (1 - pm)))
			n[1,y,1,1,1,] <- quantSums(tmp)@.Data[,y-1,,,,]
		}

		# OK turn the n back into an FLQuant and send it back in the FLBiol object

		n <- FLQuant(quant='age',n,dimnames = dimnames(n(xx)))
		n(xx) <- n
		return(xx)
	}
)
# }}}

# r {{{
# calculates the intrinsic rate of increase from the Leslie-transition matrix
# or the Euler-Lotka equation by year or by cohort.
setMethod("r", signature(m="FLQuant", fec="FLQuant"),
	function(m, fec, by = 'year', method = 'el',...)
  {
    # checks
		if(by != 'year' && by != 'cohort')
			stop("Error in r: direction of estimation is neither year nor cohort")

		if(method != 'leslie' && method != 'el')
			stop("Error in r: method used is neither Leslie matrix or Euler-Lotka")

		# estimate by year
		if(by == 'year')
    {
			dmf <- dim(fec)
			dmm <- dim(m)
			age <- as.numeric(dimnames(fec)$age)

			# solve Euler-Lotka equation
			if(method == 'el')
      {
        m <- survprob(m)

				r.func <- function(ff, p, age)
        {
					# solve Euler-Lotka using optimise
					elfn <- function(x)
						return((sum(exp(-x[1] * age) * p * ff) - 1) ^ 2)
				
					res.r <- optimise(elfn, interval=c(-10,10))[[1]]
					return(res.r)
				}

				if(dmf[6] > 1 && dmm[6] > 1 && (dmf[6] != dmm[6]))
					stop("Error in r: iteration dimensions are not the same for fec and m")

        nits <- max(dmf[6], dmm[6])

				if(dmf[6] > 1 && dmm[6] == 1)
        {		
					tmp <- m
					ps <- fec
					ps[] <- tmp[]
					rm(tmp)
					nits <- dmf[6]
				} 

				if(dmf[6] == 1 && dmm[6] > 1)
        {		
					tmp <- fec
					f <- m
					f[] <- tmp[]
					rm(tmp)
					nits <- dmm[6]
				} 

				r.ret <- FLQuant(dim=c(1,dmf[2],1,1,1,nits), 
          dimnames=dimnames(quantMeans(fec))[1:5])

				# define required variables for the estimation
				for(y in 1:dmf[2])
        {  	
					# loop over the iterations
					for(i in 1:nits)
          {	
						ff <- as.vector(fec[,y,,,,i])
						p <- as.vector(m[,y,,,,i])						
						
						r.ret[,y,,,,i] <- r.func(ff, p, age)
					}
				}
			}

			# use Leslie matrix lead eigenvalues
      else if(method == 'leslie')
      {
				m <- exp(-m)

				# define function to construct leslie matrix and calculate r 

				r.func <- function(ff, p) {

					# construct the leslie matrix 
					lesm <- matrix(ncol=length(ff),nrow=length(ff))
					
					lesm[,] <- 0
					lesm[1,] <- ff[]
					na <- length(ff)
					for(a in 1:(na-1))
						lesm[a+1,a] <- p[a+1]
					
					# calculate log of real part of the lead eigenvalue of the leslie matrix
					res.r <- log(max(Re(eigen(lesm)[['values']])))

					return(res.r)
				}

				if(dmf[6] > 1 && dmm[6] > 1 && (dmf[6] != dmm[6]))
					stop("Error in r: iteration dimensions are not the same for fec and m")

        nits <- max(dmf[6], dmm[6])

				if(dmf[6] > 1 && dmm[6] == 1)
        {		
					tmp <- m
					ps <- fec
					ps[] <- tmp[]
					rm(tmp)
					nits <- dmf[6]
				} 

				if(dmf[6] == 1 && dmm[6] > 1)
        {		
					tmp <- fec
					f <- m
					f[] <- tmp[]
					rm(tmp)
					nits <- dmm[6]
				} 

				r.ret <- FLQuant(dim=c(1,dmf[2],1,1,1,nits), 
          dimnames=dimnames(quantMeans(fec))[1:5])

				for(y in 1:dmf[2])
        {  	
					# loop over the iterations
					for(i in 1:nits)
          {	
						ff <- as.vector(fec[,y,,,,i])
						p <- as.vector(m[,y,,,,i])						
						r.ret[,y,,,,i] <- r.func(ff,p)
					}
				} 
			}
		}
	
		# estimate by cohort

    else if(by == 'cohort') {
      stop("not implemented yet")
		}

		return(r.ret)
	}
) 

setMethod("r", signature(m="FLBiol", fec="missing"),
	function(m, by = 'year', method = 'el',...)
  {
    r(m(m), fec(m), by=by, method=method,)
  }
) # }}}

# survprob {{{
# estimate survival probabilities by year or cohort
setMethod("survprob", signature(object="FLBiol"),
	function(object, by = 'year',...) {
		
		# estimate by year
		if(by == 'year')
      return(survprob(m(object)))
		
		# estimate by cohort
    else if(by == 'cohort')
      return(survprob(FLCohort(m(object))))

	}
) # }}}

# setPlusGroup {{{
setMethod('setPlusGroup', signature(x='FLBiol', plusgroup='numeric'),
s.<-	function(x, plusgroup, na.rm=FALSE)
	{
	pg.wt.mean <-c("wt","m","fec","spwn")

	#check plusgroup valid
	if (!missing(plusgroup))
     x@range["plusgroup"]<-plusgroup
  if(x@range["plusgroup"] > x@range["max"])
		 return("Error : plus group greater than oldest age")

	#Perform +grp calcs
  pg.range <- as.character(x@range["max"]:x@range["plusgroup"])

	#do the weighted stuff first
	for (i in pg.wt.mean){
	   if (dims(n(x))$iter!=dims(slot(x,i))$iter) 
         slot(x,i)<-propagate(slot(x,i),dims(n(x))$iter) 
     slot(x,i)[as.character(x@range["plusgroup"])]<-quantSums(slot(x,i)[pg.range]*x@n[pg.range])/quantSums(x@n[pg.range])
     }
  x@n[as.character(x@range["plusgroup"])]<-quantSums(x@n[pg.range])

  x<-x[as.character(x@range["min"]:x@range["plusgroup"])]

  x@range["max"]<-x@range["plusgroup"]

	return(x)
	}
)# }}}

# rec(FLBiol)  {{{
setMethod('rec', signature(object='FLBiol'),
#  function(object, rec.age=ac(dims(object)$min))
  function(object, rec.age=dims(object)$min)
  {
    if(dims(object)$quant == 'age')
      n(object)[ac(rec.age),]
    else
      stop("rec(FLBiol) only defined for age-based objects")
  }
) # }}}

# fbar {{{
setMethod("fbar", signature(object="FLBiol"),
 function(object, ...)
 {
  if (!("minfbar" %in% names(range(object))))
    range(object,"minfbar") <- dims(object)$min+1

  if (!("maxfbar" %in% names(range(object))))
    range(object,"maxfbar")<-dims(object)$max-1

  if (is.na(object@range["minfbar"]))
    object@range["minfbar"]<-object@range["min"]+1

  if (is.na(object@range["maxfbar"]))
    object@range["maxfbar"]<-object@range["max"]-1

  fbarRng<-range(object,"minfbar"):(range(object,"maxfbar")-1)

  res <- log(n(object)[ac(fbarRng),-dims(object)$year]/n(object)[ac(fbarRng+
    1),-1])-m(object)[ac(fbarRng),-dims(object)$year]

  res<-apply(res,c(2:6),mean)

  return(res)

  } 
) # }}}

# catch.n {{{
setMethod("catch.n", signature(object="FLBiol"),
  function(object)
  {
    hrvst<-harvest(object)
    z <- hrvst+m(object)[,-dims(n(object))$year]
    res <- n(object)[,-dims(n(object))$year]*hrvst/z*(1-exp(-z))
    return(res)
   }
) # }}}
