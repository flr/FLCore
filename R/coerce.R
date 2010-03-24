# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# FLBiol	{{{
setAs('FLBiol', 'FLStock',
	function(from)
	{
		FLStock(stock.n=from@n, stock.wt=from@wt, m=from@m,
			name=from@name, desc=from@desc, mat=from@fec,
			m.spwn=from@spwn,harvest.spwn=from@spwn, range=from@range)
	}
)
setAs('FLStock', 'FLBiol',
	function(from)
	{
		FLBiol(n=from@stock.n, wt=from@stock.wt, m=from@m,
			name=from@name, desc=from@desc, fec=from@mat,
			spwn=from@m.spwn, range=from@range)
	}
)

setAs('FLBiol', 'FLIndex',
  function(from)
	{
    dmns<-dimnames(from@n)
    dmns$age<-"all"
    
		res<-FLIndex(index      =from@n,
                 index.var  =FLQuant(NA, dimnames=dimnames(from@n)),
                 catch.n    =from@n,
                 catch.wt   =from@wt,
                 effort     =FLQuant(1,  dimnames=dmns),
                 sel.pattern=FLQuant(NA, dimnames=dimnames(from@n)),
                 index.q    =FLQuant(1,  dimnames=dimnames(from@n)),
                 range      =from@range,
                 type="number",
			           name=from@name, desc=paste("Coerced from FLBiol:",from@desc))
			           
    units(res@index)   <-units(from@n)
    units(res@catch.n) <-units(from@n)
    units(res@catch.wt)<-units(from@wt)
    
    res@range<-c(res@range,startf=0.0,endf=0.01)

  return(res)
	})

setAs('FLStock', 'FLIndex',
	function(from)
	{
    dmns<-dimnames(from@catch.n)
    dmns$age<-"all"

		res<-FLIndex(index       =from@stock.n,
                 catch.n     =from@catch.n,
                 catch.wt    =from@catch.wt,
                 effort      =FLQuant(1,dimnames=dmns),
                 index.q     =FLQuant(1,dimnames=dmns),
                 index.var   =FLQuant(NA, dimnames=dimnames(from@stock.n)),
                 sel.pattern =sweep(from@harvest,2:6,fbar(from),"/"),
                 range       =from@range,
                 type        ="number",
			           name        =from@name,
                 desc        =paste("Coerced from FLBiol:",from@desc))

    units(res@index)   <-units(from@stock.n)
    units(res@catch.n) <-units(from@catch.n)
    units(res@catch.wt)<-units(from@catch.wt)

  return(res)
	})
# }}}

# FLCatch	{{{
setAs('FLStock', 'FLCatch',
	function(from)
	{
		# FLCatch from FLStock
		res <- FLCatch(landings.n=landings.n(from), landings.wt=landings.wt(from),
		landings=landings(from), discards.n=discards.n(from),
		discards.wt=discards.wt(from), discards=discards(from),
    name=name(from), range=from@range, catch.q=FLQuant(1, dimnames=dimnames(catch(from))))

		# selectivities
    fbarAges <-as.character(from@range["minfbar"]:from@range["maxfbar"])
    catch.sel <- sweep(harvest(from), 2:6, apply(harvest(from)[fbarAges], 2:6, mean), "/")
    catch.ratio <- landings.n(from)/(landings.n(from)+discards.n(from))
    landings.sel(res) <- catch.sel * catch.ratio
   	discards.sel(res) <- catch.sel * (1-catch.ratio)

		return(res)
	}
)	# }}}

# FLStock	{{{
setAs('FLCatch', 'FLStock',
	function(from)
	{
		# FLCatch from FLStock
		res <- FLStock(landings.n=landings.n(from), landings.wt=landings.wt(from),
		landings=landings(from), discards.n=discards.n(from), discards.wt=discards.wt(from),
    discards=discards(from), catch=catch(from), catch.wt=catch.wt(from),
    catch.n=catch.n(from), name=name(from), range=from@range)

		return(res)
	}
)	# }}}

# FLFleet	{{{
# from FLStock
setAs('FLStock', 'FLFleet',
	function(from)
	  {
		# FLCatch from FLStock
		res <- as(from, 'FLCatch')
		return(FLFleet(res, range=from@range, effort=apply(harvest(from)[ac(range(from,"minfbar"):range(from,"maxfbar")),],c(2,4:6),mean)))
  	}
) # }}}

# FLMetier	{{{
setAs('FLStock', 'FLMetier',
	function(from)
	{
		# FLCatch from FLStock
		res <- as(from, 'FLCatch')
		return(FLMetier(res, range=from@range))
	}
)	

setAs('FLCatch', 'FLMetier',
  function(from)
  {
    return(FLMetier(from, range=from@range))
  }
)

setAs('FLCatch', 'FLMetiers',
  function(from)
  {
    return(FLMetiers(FLMetier(from)))
  }
)
setAs('FLCatch', 'FLIndex',
  function(from)
	{
    dmns    <-dimnames(from@landings.n)
    dmns$age<-"all"
    
		res<-FLIndex(catch.n    =catch.n(from),
                 index      =FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 index.var  =FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 catch.wt   =catch.wt(from),
                 effort     =FLQuant(NA, dimnames=dmns),
                 sel.pattern=FLQuant(NA, dimnames=dimnames(from@landings.n)),
                 index.q    =FLQuant(1,  dimnames=dimnames(from@landings.n)),
                 range      =from@range,
                 type="number",
			           name=from@name, desc=paste("Coerced from FLCatch:",from@desc))
			           
    units(res@index)   <-"NA"
    units(res@catch.n) <-units(from@landings.n)
    units(res@catch.wt)<-units(from@landings.wt)
    
    res@range<-c(res@range,startf=0.0,endf=0.01)

  return(res)
	}
)
# }}}

# FLArray	{{{
setAs('FLArray', 'data.frame',
	function(from)
	{
		# to avoid warnings when NA have to be added
		options(warn=-1)
        if(any(is.na(suppressWarnings(as.numeric(dimnames(from)[[1]])))))
            quant <- as.character(dimnames(from)[[1]])
        else
            quant <- as.numeric(dimnames(from)[[1]])
    dnames <- dimnames(from)
		df <- data.frame(expand.grid(quant=quant,
			year=as.numeric(dnames[[2]]),
			unit=factor(dnames[[3]], levels=dnames[[3]]),
			season=factor(dnames[[4]], levels=dnames[[4]]),
			area=factor(dnames[[5]], levels=dnames[[5]]),
			iter=factor(dnames[[6]], levels=dnames[[6]])),
			data=as.vector(from))
		names(df)[1:2] <- names(dnames)[1:2]
		attributes(df)$units <- units(from)
		options(warn=0)
		return(df)
	}
)	# }}}

# FLPar	{{{
setAs('FLPar', 'list',
	function(from)
	{
    res <- list()
    for(i in dimnames(from)[['params']])
      res[[i]] <- c(from[i,])
    return(res)
	}
)	# }}}
