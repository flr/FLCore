# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: coerce.R 1789 2012-12-10 10:34:22Z imosqueira $

# From FLBiol {{{
setAs('FLBiol', 'FLStock',
	function(from)
	{
		FLStock(stock.n=from@n, stock.wt=from@wt, m=from@m,
			name=from@name, desc=from@desc, mat=from@fec,
			m.spwn=from@spwn,harvest.spwn=from@spwn, range=from@range)
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
	}
) # }}}

# From FLStock  {{{
setAs('FLStock', 'FLBiol',
	function(from)
	{
		FLBiol(n=from@stock.n, wt=from@stock.wt, m=from@m,
			name=from@name, desc=from@desc, fec=from@mat,
			spwn=from@m.spwn, range=from@range)
	}
)

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
                 range       =c(from@range, startf=0, endf=1),
                 type        ="number",
			           name        =from@name,
                 desc        =paste("Coerced from FLStock:",from@desc))

    if(units(harvest(from)) == 'f')
      sel.pattern(res) <- sweep(from@harvest,2:6,fbar(from),"/")

    units(res@index)   <-units(from@stock.n)
    units(res@catch.n) <-units(from@catch.n)
    units(res@catch.wt)<-units(from@catch.wt)

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
    dnames <- dimnames(from)
        if(!any(is.na(suppressWarnings(as.numeric(dnames[[1]])))))
            quant <- as.numeric(dimnames(from)[[1]])
        else
			      quant <- factor(dnames[[1]], levels=dnames[[1]])
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
) 

setAs('FLPar', 'data.frame',
  function(from)
  {
	  return(data.frame(expand.grid(dimnames(from)), data=as.vector(from@.Data)))
  }
) 

setAs('data.frame', 'FLPar',
  function(from) {
    
		# iter names from df
		if("iter" %in% names(from))
			iters <- from$iter
		# or from rownames, if present
		else
    	iters <- rownames(from, do.NULL=TRUE, prefix="")

    # param named columns
		pnames <- names(from)[!names(from) %in% c("data", "iter")]
		
		pnames <- lapply(as.list(as.list(subset(from, select=pnames))), unique)
		pnames <- lapply(pnames, as.character)

	  dmns <- c(pnames, list(iter=unique(iters)))
    
		return(FLPar(from$data, dimnames=dmns, units="NA"))
  }
)

# }}}
