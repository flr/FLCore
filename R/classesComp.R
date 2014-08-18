# classesComp.R - 
# FLCore/R/classesComp.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# FLComp   {{{
validFLComp <- function(object){

	# range must be named ...
	nms <- names(range(object))
	# with non/empty strings
	if(any(nchar(nms) == 0))
		return("names in range cannot be empty")

	# Any FLArray?
	slots <- getSlots(class(object))
	
	if(any("FLArray" %in% slots) | any("FLQuant" %in% slots)) {

		# FLQuant slots must have either 1 or n iter
  	dims <- unlist(qapply(object, function(x) dims(x)$iter))
	  test <- dims != max(dims) & dims != 1
		if (any(test))
			stop(paste("All slots must have iters equal to 1 or 'n': error in",
				paste(names(test[!test]), collapse=', ')))
	
	  # and dimname for iter[1] should be '1'
  	dimnms <- qapply(object, function(x) dimnames(x)$iter)
		test <- unlist(dimnms[dims == 1])
		if(!all(test==test))
			stop(paste("Incorrect names on the iter dimension in ",
				paste(names(test[!test]), collapse=', ')))

	  # all 'quant' should be equal
  	quants <- unlist(qapply(object, quant))
	  if(any(quants != quants[1]))
  	  stop("Not all 'quant' names are the same. Check using qapply(x, quant)")

	}
	
	return(TRUE)
}

#' Class FLComp
#' 
#' A virtual class that forms the basis for most FLR classes composed of slots
#' of class \code{\linkS4class{FLQuant}}. No objects of this class can be
#' constructed.
#'
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' 
#' @name FLComp
#' @aliases FLComp FLComp-class
#' @docType class
#' @section Slots: \describe{
#'    \item{name}{A character vector for the object name.}
#'    \item{desc}{A textual description of the object contents.}
#'    \item{range}{A named numeric vector with various values of quant and year ranges, plusgroup, fishing mortality ranges, etc.}
#' }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame},
#' \link{iter}, \link{propagate}, \link{qapply}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link{units,FLComp-method},
#' \link{units<-,FLComp,list-method}, \link[stats]{window}
#' @keywords classes
setClass("FLComp",
	representation(
		name="character",
		desc="character",
		range="numeric",
		"VIRTUAL"), 
	prototype(
		name=character(1),
		desc=character(0),
  	range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1))), 
  validity=validFLComp)

invisible(createFLAccesors('FLComp', include=c('name', 'desc')))
#  }}}

# FLS			{{{
validFLS <- function(object) {

	# TODO
	return(TRUE)

	names <- names(getSlots('FLS')[getSlots('FLS')=="FLQuant"])
	for(i in names){
		# all dimnames 2:5 are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in object@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in object', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in object'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}

setClass("FLS",
	representation(
	"FLComp",
	catch	    	="FLQuant",
	catch.n	    ="FLQuant",
	catch.wt		="FLQuant",
	discards		="FLQuant",
	discards.n  ="FLQuant",
	discards.wt ="FLQuant",
	landings		="FLQuant",
	landings.n  ="FLQuant",
	landings.wt ="FLQuant",
	stock	    	="FLQuant",
	stock.n	    ="FLQuant",
	stock.wt		="FLQuant",
	m						="FLQuant",
	mat		    	="FLQuant",
	harvest	    ="FLQuant",
	harvest.spwn="FLQuant",
	m.spwn	    ="FLQuant",
	"VIRTUAL"
	),
	prototype=prototype(
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(units='m'),
		mat		 = FLQuant(units='prop'),
		harvest	 = FLQuant(units='f'),
		harvest.spwn = FLQuant(units='prop'),
		m.spwn	 = FLQuant(units='prop')
	),
  validity=validFLS
)
remove(validFLS)

invisible(createFLAccesors("FLS", exclude=c('name', 'desc', 'range', 'harvest')))	# }}}

# FLStock			{{{
validFLStock <- function(object) {
	
	names <- names(getSlots('FLStock')[getSlots('FLStock')=="FLQuant"])
	for(i in names){
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLStock@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLStock', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLStock'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}

#' Class FLStock
#' 
#' A class for modelling a fish stock. 
#'
#' The \code{FLStock} object contains a representation of a fish stock
#' This includes information on removals (i.e. catches, landings and discards), 
#' maturity, natural mortality and the results of an analytical assessment (i.e.
#' estimates of abundance and removal rates).
#' 
#' @name FLStock
#' @template FLStock-aliases
#'
#' @docType class
#' @template FLStock_slots
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#'     \item{Totals}{The length of the quant dimension for the totals slots (catch, landings and discards) must be equal to 1.}
#' }
#' @template Accessors
#' @template Constructors
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link{as.FLBiol}, \link{as.FLSR},
#' \link{catch}, \link{catch<-}, \link{catch.n}, \link{catch.n<-},
#' \link{catch.wt}, \link{catch.wt<-}, \link[methods]{coerce},
#' \link{computeCatch}, \link{computeDiscards}, \link{computeLandings},
#' \link{discards}, \link{discards<-}, \link{discards.n}, \link{discards.n<-},
#' \link{discards.wt}, \link{discards.wt<-}, \link{harvest}, \link{harvest<-},
#' \link{harvest.spwn}, \link{landings}, \link{landings<-}, \link{landings.n},
#' \link{landings.n<-}, \link{landings.wt}, \link{landings.wt<-}, \link{m},
#' \link{m<-}, \link{mat}, \link{m.spwn}, \link[graphics]{plot}, \link{ssb},
#' \link{ssbpurec}, \link{stock}, \link{stock.n}, \link{stock.wt}, \link{trim},
#' \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' 
#' landings(ple4) #get the landings slot
#' landings(ple4) <- apply(landings.n(ple4)*landings.wt(ple4),2,sum)   # assign values to the landings slot
#' 
#' discards(ple4) <- computeDiscards(ple4)
#' 
#' harvest(ple4) <- 'f' # set the units of the harvest slot of an FLStock object
#' 
#' catch(ple4) <- computeCatch(ple4)
#' catch(ple4) <- computeCatch(ple4, slot="all")
#' 
#' ple4[,1] # subset the FLStock
#' trim(ple4, age=2:6, year=1980:1990) #trim the FLStock
#' 
#' ssb(ple4) # calculate SSB
#' ssbpurec(ple4) # calculate SSB per recruit
#' 
#' biol <- as(ple4, "FLBiol")  # coerce an FLStock to an FLBiol
#' flsr <- as.FLSR(ple4)       # initialise an FLSR object from an FLStock
#' 
#' 
#' 
setClass("FLStock",
	representation(
	"FLS"
	),
	prototype=prototype(
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(),
		mat		 = FLQuant(),
		harvest	 = FLQuant(units="f"),
		harvest.spwn = FLQuant(),
		m.spwn	 = FLQuant()
	),
  validity=validFLStock
)
remove(validFLStock)

#invisible(createFLAccesors("FLStock", exclude=c('name', 'desc', 'range', 'harvest')))	# }}}

# FLStockLen			{{{

#' Class FLStockLen
#' 
#' A class for modelling a length structured fish stock.
#'
#' The \code{FLStockLen} object contains a length based representation of a fish stock
#' This includes information on removals (i.e. catches, landings and discards), 
#' maturity, natural mortality and the results of an analytical assessment (i.e.
#' estimates of abundance and removal rates).
#'
#' @name FLStockLen
#' @template FLStockLen-aliases
#' @docType class
#' @section Slots:
#'     \describe{
#'     \item{halfwidth}{The middle of the length bins (\code{numeric}).}
#'     \item{catch}{Total catch weight (\code{FLQuant}).}
#'     \item{catch.n}{Catch numbers (\code{FLQuant}).}
#'     \item{catch.wt}{Mean catch weights (\code{FLQuant}).}
#'     \item{discards}{Total discards weight (\code{FLQuant}).}
#'     \item{discards.n}{Discard numbers (\code{FLQuant}).}
#'     \item{discards.wt}{Mean discard weights (\code{FLQuant}).}
#'     \item{landings}{Total landings weight (\code{FLQuant}).}
#'     \item{landings.n}{Landing numbers (\code{FLQuant}).}
#'     \item{landings.wt}{Landing weights (\code{FLQuant}).}
#'     \item{stock}{Total stock weight (\code{FLQuant}).}
#'     \item{stock.n}{Stock numbers (\code{FLQuant}).}
#'     \item{stock.wt}{Mean stock weights (\code{FLQuant}).}
#'     \item{m}{Natural mortality (\code{FLQuant}).}
#'     \item{mat}{Proportion mature (\code{FLQuant}).}
#'     \item{harvest}{Harvest rate or fishing mortality. The units of the FLQuant should be set to 'harvest' or 'f' accordingly (\code{FLQuant}).} 
#'     \item{harvest.spwn}{Proportion of harvest/fishing mortality before spawning (\code{FLQuant}).}
#'     \item{m.spwn}{Proportion of natural mortality before spawning (\code{FLQuant}).}
#'     \item{name}{Name of the stock (\code{character}).}
#'     \item{desc}{Description of stock (\code{character}).}
#'     \item{range}{Named numeric vector containing the quant and year ranges, the plusgroup and the quant range that the average fishing mortality is calculated over (\code{numeric}).}
#' }
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#'     \item{Totals}{The length of the quant dimension for the totals slots (catch, landings and discards) must be equal to 1.}
#' }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link{as.FLBiol}, \link{as.FLSR},
#' \link{computeCatch}, \link{computeDiscards}, \link{computeLandings}, \link[graphics]{plot}, \link{ssb},
#' \link{ssbpurec}, \link{trim}, \link{FLComp}
#' @keywords classes
setClass("FLStockLen",
	representation(
	"FLS",
	halfwidth = "numeric"
	),
	prototype=prototype(
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		halfwidth = as.numeric(NA),
		catch	= FLQuant(dimnames=list(len=as.numeric(NA))),
		catch.n	= FLQuant(dimnames=list(len=as.numeric(NA))),
		catch.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
		discards= FLQuant(dimnames=list(len=as.numeric(NA))),
		discards.n = FLQuant(dimnames=list(len=as.numeric(NA))),
		discards.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
		landings   = FLQuant(dimnames=list(len=as.numeric(NA))),
		landings.n = FLQuant(dimnames=list(len=as.numeric(NA))),
		landings.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
		stock	   = FLQuant(dimnames=list(len=as.numeric(NA))),
		stock.n	 = FLQuant(dimnames=list(len=as.numeric(NA))),
		stock.wt = FLQuant(dimnames=list(len=as.numeric(NA))),
		m		 = FLQuant(dimnames=list(len=as.numeric(NA))),
		mat		 = FLQuant(dimnames=list(len=as.numeric(NA))),
		harvest	 = FLQuant(dimnames=list(len=as.numeric(NA))),
		harvest.spwn = FLQuant(dimnames=list(len=as.numeric(NA))),
		m.spwn	 = FLQuant(dimnames=list(len=as.numeric(NA)))
	),
  validity=function(object) {

	# TODO
	return(TRUE)
	
	names <- names(getSlots('FLStock')[getSlots('FLStock')=="FLQuant"])
	for(i in names){
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLStock@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLStock', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLStock'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}
) # }}}

# FLBiol {{{
validFLBiol <- function(object){

   ## All FLQuant objects must have same dimensions
   Dim <- dim(object@n)[-6]

   s.  <-list("n","wt","fec","spwn","m")

   for (i. in s.)	{
	   t. <- slot(object,i.)
	   if (is.FLQuant(t.) & !all(dim(t.)[-6] == Dim))
	      return(paste("FLQuant dimensions wrong for ", i.))
	   }

   # Verify that bounds are correct and correspond to first slot
	.t  <-getSlots(class(object))
	.t  <-.t[.t=="FLQuant"]

   if (length(.t)> 0) {

      Par <- dims(.s<-slot(object,names(.t[1])))

	   min <- object@range["min"]
      if (!is.na(min) && (min < Par$min || min > Par$max))
   		return("min quant is outside range in FLQuant slots")
	   max <- object@range["max"]
	   if (!is.na(max) && (max < Par$min || max > Par$max))
	   	return("max quant is outside range in FLQuant slots")
	   if (!is.na(min) && !is.na(max) && max < min)
		   return("max quant is lower than min")
	   plusgroup <- object@range["plusgroup"]
	   if (!is.na(plusgroup) && (plusgroup < Par$min || plusgroup > Par$max))
		   return("plusgroup is outside [min, max] quant range in FLQuant slots")
	   minyear <- object@range["minyear"]
	   if (!is.na(minyear) && (minyear < Par$minyear || minyear > Par$maxyear))
		   return("minyear is outside years range in FLQuant slots")
	   maxyear <- object@range["maxyear"]
	   if (!is.na(maxyear) && (maxyear < Par$minyear || maxyear > Par$maxyear))
		   return("maxyear is outside years range in FLQuant slots")
	   if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
		   return("maxyear is lower than minyear")
	   }

   # Everything is fine
   return(TRUE)
   }

#' Class FLBiol
#' 
#' A class for modelling age / length or biomass structured populations.   
#' 
#' The \code{FLBiol} class is a representation of a biological fish population. 
#' This includes information on abundances, natural mortlity and fecundity.
#' 
#' @name FLBiol
#' @template FLBiol-aliases
#' @docType class
#' @section Slots: \describe{
#'     \item{n}{Numbers in the population. \code{FLQuant}.}
#'     \item{m}{Mortality rate of the population. \code{FLQuant}.}
#'     \item{wt}{Mean weight of an individual. \code{FLQuant}.}
#'     \item{fec}{Fecundity/maturity/per capita birth rate. \code{FLQuant}.}
#'     \item{spwn}{Proportion of mortality before spawning/birth. \code{FLQuant}.}
#'     \item{name}{Name of the object. \code{character}.}
#'     \item{desc}{Brief description of the object. \code{character}.}
#'     \item{range}{Named numeric vector describing the range of the object. \code{numeric}.} }
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author The FLR Team
#' @seealso \link{as.FLBiol}, \link{as.FLSR}, \link[methods]{coerce}, \link[graphics]{plot}, \link{ssb} \link{catch.n,FLBiol-method}
#' @keywords classes
#' @examples
#' 
#' # An FLBiol example dataset
#' data(ple4.biol)
#' 
#' summary(ple4.biol)
#' 
setClass("FLBiol",
	representation(
		"FLComp",
    n        ="FLQuant",
		m        ="FLQuant",
		wt       ="FLQuant",
		fec      ="FLQuant",
		spwn     ="FLQuant"
      ),
	prototype=prototype(
		range    =unlist(list(min=NA, max=NA, plusgroup=NA, minyear=1, maxyear=1)),
    n        = FLQuant(),
		m        = FLQuant(),
		wt       = FLQuant(),
		fec      = FLQuant(),
		spwn     = FLQuant()),
	validity=validFLBiol
)

setValidity("FLBiol", validFLBiol)
remove(validFLBiol)	# We do not need this function any more
invisible(createFLAccesors("FLBiol", exclude=c('name', 'desc', 'range'))) # }}}

# FLI    {{{


#' Class FLI
#' 
#' A VIRTUAL class that holds data and parameters related to abundance indices.
#' 
#' @name FLI
#' @docType class
#' @section Slots: \describe{
#'     \item{type}{Type of index (\code{character}).}
#'     \item{distribution}{Statistical distribution of the index values (\code{character}).}
#'     \item{index}{Index values (\code{FLQuant}).}
#'     \item{index.var}{Variance of the index (\code{FLQuant}).}
#'     \item{catch.n}{Catch numbers used to create the index (\code{FLQuant}).}
#'     \item{catch.wt}{Catch weight of the index (\code{FLQuant}).}
#'     \item{effort}{Effort used to create the index (\code{FLQuant}).}
#'     \item{sel.pattern}{Selection pattern for the index (\code{FLQuant}).}
#'     \item{index.q}{Catchability of the index (\code{FLQuant}).}
#'     \item{name}{Name of the stock (\code{character}).}
#'     \item{desc}{General description of the object (\code{character}).}
#'     \item{range}{Range of the object (\code{numeric})}
#' }
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author The FLR Team
#' @seealso \link{computeCatch}, \link{dims},
#' \link{iter}, \link[graphics]{plot}, \link{propagate}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp}
#' @keywords classes
setClass("FLI",
    representation(
		"FLComp",
    distribution = "character",
    index        = "FLQuant",
    index.var    = "FLQuant",
    catch.n      = "FLQuant",
		catch.wt     = "FLQuant",
		effort       = "FLQuant",
		sel.pattern  = "FLQuant",
		index.q      = "FLQuant",
		"VIRTUAL"),
    prototype=prototype(
        range        = unlist(list(min=0, max=0, plusgroup=NA,
			minyear=1, maxyear=1, startf=NA, endf=NA)),
        distribution = character(0),
        index        = new("FLQuant"),
        index.var    = new("FLQuant"),
		catch.n      = new("FLQuant"),
		catch.wt     = new("FLQuant"),
		effort       = new("FLQuant"),
		sel.pattern  = new("FLQuant"),
		index.q      = new("FLQuant")),
    validity = function(object) {

  dimnms <- qapply(object, function(x) dimnames(x))

  # iters are 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("Iters in FLI can only be of length 1 or n")

  # quant is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
     stop("quant dimension in FLI can only be 'all' or n")

  # iter is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("iter dimension in FLI can only be '1' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    if(!all.equal(dimnms[[i]][c(-1,-6)], dimnms[[1]][c(-1,-6)]))
      stop(cat("Mismatch in dims for", i))
      
  # first dim equal for all index.* slots
  #for(i in grep('index', names(dimnms), value=TRUE))
  #  if(!all.equal(dimnms[[i]][1], dimnms[[1]][1]))
  #    stop(cat("Mismatch in dims for", i))

  # effort should have quant='all'
  if (!(dims(slot(object,"effort"))[1] == 1))
     stop("Effort can only have quant = 'all'")

  # min / max
  dims <- dims(object@catch.n)
  min <- object@range["min"]
  max <- object@range["max"]

  if (!is.na(min) && !is.na(max) && max < min)
    stop(paste("max quant is lower than min quant in FLQuant slot", i))

  # plusgroup
  plusgroup <- object@range["plusgroup"]
  if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
     stop("plusgroup is outside [min, max] range in FLQuant slots")

  # minyear / maxyear
  dims <- dims(object@index)
  minyear <- object@range["minyear"]
  if (!is.na(minyear) && (minyear < dims$minyear || minyear > dims$maxyear))
     stop(paste("minyear is outside years range in FLQuant slot", i))
  maxyear <- object@range["maxyear"]
  if (!is.na(maxyear) && (maxyear < dims$minyear || maxyear > dims$maxyear))
     stop(paste("maxyear is outside years range in FLQuant slot", i))
  if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
     stop(paste("maxyear is lower than minyear in FLQuant slot", i))

  # Everything is fine
  return(TRUE)
  }
) #   }}}

# FLIndex    {{{

#' Class FLIndex
#' 
#' A class that holds data and parameters related to abundance indices.
#' 
#' @name FLIndex
#' @template FLIndex-aliases
#' @docType class
#' @section Slots: \describe{
#'     \item{type}{Type of index (\code{character}).}
#'     \item{distribution}{Statistical distribution of the index values (\code{character}).}
#'     \item{index}{Index values (\code{FLQuant}).}
#'     \item{index.var}{Variance of the index (\code{FLQuant}).}
#'     \item{catch.n}{Catch numbers used to create the index (\code{FLQuant}).}
#'     \item{catch.wt}{Catch weight of the index (\code{FLQuant}).}
#'     \item{effort}{Effort used to create the index (\code{FLQuant}).}
#'     \item{sel.pattern}{Selection pattern for the index (\code{FLQuant}).}
#'     \item{index.q}{Catchability of the index (\code{FLQuant}).}
#'     \item{name}{Name of the stock (\code{character}).}
#'     \item{desc}{General description of the object (\code{character}).}
#'     \item{range}{Range of the object (\code{numeric})}
#' }
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author The FLR Team
#' @seealso \link{computeCatch}, \link{dims},
#' \link{iter}, \link[graphics]{plot}, \link{propagate}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' fli <- FLIndex(index=FLQuant(rnorm(8), dim=c(1,8)), name="myTestFLindex")
#' summary(fli)
#' index(fli)
#' 
setClass("FLIndex",
    representation(
		"FLI",
		type         = "character"),
    prototype=prototype(
    type         = character(0)),
    validity=function(object) {

  # min / max
  dims <- dims(object@catch.n)
  min <- object@range["min"]
  max <- object@range["max"]

  if (!is.na(min) && (min < dims(object@catch.n)$min || min > dims(object@catch.n)$max))
     stop(paste("min is outside quant range in FLQuant slot", i))

  if(!is.na(max) && (max < dims(object@catch.n)$min || max > dims(object@catch.n)$max))
    stop(paste("max is outside quant range in FLQuant slot", i))

  # Everything is fine
  return(TRUE)
  }
) #   }}}

# FLIndexBiomass    {{{

#' Class FLIndexBiomass
#' 
#' A class that holds data and parameters related to biomass abundance indices.
#' 
#' @name FLIndexBiomass
#' @template FLIndex-aliases
#' @docType class
#' @section Slots: \describe{
#'     \item{distribution}{Statistical distribution of the index values (\code{character}).}
#'     \item{index}{Index values (\code{FLQuant}).}
#'     \item{index.var}{Variance of the index (\code{FLQuant}).}
#'     \item{catch.n}{Catch numbers used to create the index (\code{FLQuant}).}
#'     \item{catch.wt}{Catch weight of the index (\code{FLQuant}).}
#'     \item{effort}{Effort used to create the index (\code{FLQuant}).}
#'     \item{sel.pattern}{Selection pattern for the index (\code{FLQuant}).}
#'     \item{index.q}{Catchability of the index (\code{FLQuant}).}
#'     \item{name}{Name of the stock (\code{character}).}
#'     \item{desc}{General description of the object (\code{character}).}
#'     \item{range}{Range of the object (\code{numeric})}
#' }
#' @template Accessors
#' @template Constructors
#' @section Validity: \describe{
#'     \item{Dimensions}{All FLQuant slots must have iters equal to 1 or 'n'.}
#'     \item{Iters}{The dimname for iter[1] should be '1'.}
#'     \item{Dimnames}{The name of the quant dimension must be the same for all FLQuant slots.}
#' }
#' @author The FLR Team
#' @seealso \link{computeCatch}, \link{dims},
#' \link{iter}, \link[graphics]{plot}, \link{propagate}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' idx <- FLIndexBiomass(index=FLQuant(1:10, quant='age'))
#' 
#' data(ple4)
#' ida <- FLIndexBiomass(index=ssb(ple4),
#'   catch.n=catch.n(ple4))
#'
setClass("FLIndexBiomass",
	representation(
		"FLI"),
  prototype=prototype(
    index=FLQuant(dimnames=list(age='all'))
	),
  validity=function(object) {

		dims <- dims(object)

		# age='all'
		if(dims$quant != 'age')
			return("quant in FLIndexBiomass must be 'age'")

		if(dimnames(object@index)['age'] != 'all')
			return("quant dimnames in FLIndexBiomass must be 'all'")

		# slots with no ages
		dimq <- unlist(qapply(object, function(x) dim(x)[1]))

		# slots with no ages
		noq <- c('index', 'index.var', 'index.q')
		if(any(!noq %in% names(dimq)[dimq == 1]))
			return("slots index, index.var and index.q must have age='all'")

		# others must have equal age
		dimq <- dimq[names(dimq) %in% c('catch.n', 'catch.wt', 'sel.pattern')]
		if(any(dimq =! dimq[1]))
			return("Slots with age data must have the same dimensions")

		# Everything is fine
	  return(TRUE)
  }
) #   }}}

# FLModel  {{{
validFLModel <- function(object)
{
  # All FLArray slots are of the same exact class
  flarr <- getSlotNamesClass(object, 'FLArray')
  class <- class(slot(object, flarr[1]))
  for(i in flarr[-1])
    if(class(slot(object, i)) != class)
      return(paste('FLQuant/FLCohort slots in object should all be of the same class: ',
        i))
  
  # initial returns an FLPar
  init <- do.call(initial(object), lapply(formals(initial(object)), function(x) x<-0.1))
  if(!is.null(init) & !is(init, 'FLPar'))
    return("initial function must return an 'FLPar'")

  return(TRUE)
}
setClass('FLModel',
  representation('FLComp',
    model='formula',
    logl='function',
    gr='function',
    distribution='factor',
    initial='function',
    params='FLPar',
    logLik='logLik',
    vcov='array',
    hessian='array',
    details='list',
    residuals='FLArray',
    fitted='FLArray'),
  prototype(
    range=unlist(list(min=NA, max=NA, minyear=1, maxyear=1)),
    model=formula(NULL),
    distribution=factor(levels=c('beta', 'dbinom', 'cauchy', 'chisq', 'exp',
        'f', 'gamma', 'geom', 'hyper', 'lnorm', 'multinom', 'nbinom', 'norm',
        'pois', 't', 'unif', 'weibull')),
    fitted=FLQuant(),
    residuals=FLQuant())
)
invisible(createFLAccesors("FLModel", exclude=c('name', 'desc', 'range', 'params', 'distribution')))  # }}}

# FLlst class{{{
vFLl <- function(object){

	# Make sure the list contains all items of the same class
	cls <- unlist(lapply(object, class))
  if(any(cls != cls[1]))
	  return("Components must be of the same class!")	

  # All elements in the list are validObjects themselves
  if(!all(unlist(lapply(object, validObject))))
	  return("Components must be valid objects themselves (validObject == TRUE)")	

	# Everything is fine
	return(TRUE)
}

# class
setClass("FLlst", contains="list",
  representation(names="character", desc="character", lock="logical"),
	prototype(lock=FALSE),
	validity=vFLl
) # }}}

# FLQuants {{{
# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLQuant")) stop("Components must be FLQuant")	
	}
	# Everything is fine
	return(TRUE)
}

#' Class FLQuants
#' 
#' \code{FLQuants} is a \code{list} of \code{FLQuant} objects.
#' It is very similar to the standard \code{list} class.
#' It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#' The elements of \code{FLQuants} must all be of class  \code{FLQuant}. 
#' 
#' @name FLQuants
#' @aliases FLQuants-class FLQuants FLQuants-methods FLQuants,ANY-method
#' FLQuants,missing-method FLQuants,list-method FLQuants,FLQuants-method
#' @docType class
#' @section Slots: \describe{
#'     \item{.Data}{The data. \code{list}.}
#'     \item{names}{Names of the list elements. \code{character}.}
#'     \item{desc}{Description of the object. \code{character}.}
#'     \item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}.}
#' }
#' @template FLlst-accessors
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes
# class
setClass("FLQuants", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLQuants", function(object, ...){
	standardGeneric("FLQuants")
	}
)

setMethod("FLQuants", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLQuants", lst)
})


setMethod("FLQuants", signature(object="FLComp"),
	function(object, ...) {

		args <- list(...)

		# SPLIT into list if a character vector 
		if(length(args) == 1 & length(args[[1]]) > 1)
			args <- as.list(args[[1]])

		# CHECK args are char or function
		chr <- unlist(lapply(args, function(x) is(x, 'character')))
		fun <- unlist(lapply(args, function(x) is(x, 'function')))
		
		if(sum(chr + fun) != length(args))
			stop("Arguments in ... must be of class 'character' or 'function'")

		# CHECK function elements have names
		if(any(names(args[fun]) == ""))
			stop("Function calls must be named, e.g. catch=catch")

		# GET names
		nms <- names(args)
		nms[chr] <- unlist(args[chr])

		# DO.CALL list elements
		res <- lapply(args, function(x) do.call(x, args=list(object)))

		# ASSIGN names
		names(res) <- nms

		return(new("FLQuants", res))
})

setMethod("FLQuants", "missing", function(...){
	if(missing(...)){
		new("FLQuants")
	} else { 
		lst <- list(...)
		new("FLQuants", lst)
	}
})

setMethod("FLQuants", "list", function(object){
	new("FLQuants", object)
})

setMethod("FLQuants", "FLQuants", function(object){
	return(object)
}) # }}}

# FLSR  {{{
validFLSR <- function(object)
{
	# params must have dims equal to quants
	return(TRUE)
}

#' Class FLSR
#' 
#' Class for stock-recruitment models.
#' 
#' A series of commonly-used stock-recruitment models are already available,
#' including the corresponding likelihood functions and calculation of initial
#' values. See \code{\link{SRModels}} for more details and the exact
#' formulation implemented for each of them.
#' 
#' @name FLSR
#' @aliases FLSR-class FLSR FLSR-methods FLSR,ANY-method FLSR,missing-method
#' @docType class
#' @section Slots: \describe{
#'     \item{name}{Name of the object (\code{character}).}
#'     \item{desc}{Description of the object (\code{character}).}
#'     \item{range}{Range (\code{numeric}).}
#'     \item{rec}{Recruitment series (\code{FLQuant}).}
#'     \item{ssb}{Index of reproductive potential, e.g. SSB or egg oor egg production (\code{FLQuant}).}
#'     \item{fitted}{Estimated values for rec (\code{FLQuant}).}
#'     \item{residuals}{Residuals obtained from the model fit (\code{FLArray}).}
#'     \item{covar}{Covariates for SR model (\code{FLQuants}).}
#'     \item{model}{Model formula (\code{formula}).}
#'     \item{gr}{Function returning the gradient of the likelihood (\code{function}).}
#'     \item{logl}{Log-likelihood function (\code{function}).}
#'     \item{initial}{Function returning initial parameter values for the optimizer (\code{function}).}
#'     \item{params}{Estimated parameter values (\code{FLPar}).}
#'     \item{logLik}{Value of the log-likelihood (\code{logLik}).}
#'     \item{vcov}{Variance-covariance matrix (\code{array}).}
#'     \item{details}{Extra information on the model fit procedure (\code{list}).}
#'     \item{logerror}{Is the error on a log scale (\code{logical}).}
#'     \item{distribution}{(\code{factor}).}
#'     \item{hessian}{Resulting Hessian matrix from the fit (\code{array}).}
#' }
#' @author The FLR Team
#' @seealso \link{FLModel}, \link{FLComp}
#' @keywords classes
#' @examples
#' 
#'     # Create an empty FLSR object.
#'     sr1 <- FLSR()
#'     
#'     # Create an  FLSR object using the existing SR models. 
#'     sr2 <- FLSR(model = 'ricker')
#'     sr2@@model
#'     sr2@@initial
#'     sr2@@logl
#'     
#'     sr3 <- FLSR(model = 'bevholt')
#'     sr3@@model
#'     sr3@@initial
#'     sr3@@logl
#'     
#'     # Create an FLSR using a function.
#'     mysr1 <- function(){
#'         model <- rec ~ a*ssb^b
#'         return(list(model = model))}
#'     
#'     sr4 <- FLSR(model = mysr1)
#' 
#'     # Create an FLSR using a function and check that it works.
#'     mysr2 <- function(){
#'         formula <- rec ~ a+ssb*b
#'         
#'         logl <- function(a, b, sigma, rec, ssb) sum(dnorm(rec, 
#'             a + ssb*b, sqrt(sigma), TRUE))
#'         
#'        initial <- structure(function(rec, ssb) {
#'             a     <- mean(rec)
#'             b     <- 1
#'             sigma <- sqrt(var(rec))
#'             
#'             return(list(a= a, b = b, sigma = sigma))}, lower = c(0, 1e-04, 1e-04), upper = rep(Inf, 3))
#'         
#'        return(list(model = formula, initial = initial, logl = logl))
#'     }
#'       
#'     ssb <- FLQuant(runif(10, 10000, 100000))
#'     rec <- 10000 + 2*ssb + rnorm(10,0,1)  
#'     sr5 <- FLSR(model = mysr2, ssb = ssb, rec = rec)
#'     
#'     sr5.mle <- fmle(sr5)
#'     sr5.nls <- nls(sr5)
#'     
#' # NS Herring stock-recruitment dataset
#' data(nsher)
#' 
#' # already fitted with a Ricker SR model
#' summary(nsher)
#' 
#' plot(nsher)
#' 
#' # change model
#' model(nsher) <- bevholt()
#' 
#' # fit through MLE
#' nsher <- fmle(nsher)
#' 
#' plot(nsher)
#' 
setClass('FLSR',
  representation(
	  'FLModel',
  	rec='FLQuant',
	  ssb='FLQuant',
  	covar='FLQuants',
    logerror='logical'),
  prototype(residuals=FLQuant(), fitted=FLQuant(), logerror=TRUE, covar=new('FLQuants')),
	validity=validFLSR)
remove(validFLSR)

invisible(createFLAccesors("FLSR", include=c('rec', 'ssb', 'covar'))) # }}}

# FLCohorts {{{

# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCohort")) stop("Components must be FLCohort")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLCohorts", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLCohorts", function(object, ...){
	standardGeneric("FLCohorts")
	}
)

setMethod("FLCohorts", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLCohorts", lst)
})

setMethod("FLCohorts", "missing", function(...){
	if(missing(...)){
		new("FLCohorts")
	} else { 
		lst <- list(...)
		new("FLCohorts", lst)
	}
})

setMethod("FLCohorts", "list", function(object){
	new("FLCohorts", object)
})

setMethod("FLCohorts", "FLCohorts", function(object){
	return(object)
}) # }}}

# FLComps {{{
vFLCs <- function(object) {

  # all elements inherit from class FLComp
  if(!all(unlist(lapply(object, is, 'FLComp'))))
    return("All elements must be of a class that inherits from FLComp")

  return(TRUE)
}

setClass("FLComps", contains=c("FLlst"), validity=vFLCs)
# }}}

# FLStocks {{{
vFLSs <- function(object){
	
  # All items are FLStock
  if(!all(unlist(lapply(object, is, 'FLStock'))))
      return("Components must be FLStock")	
	
	return(TRUE)
}

# class
setClass("FLStocks", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLStocks", signature(object="FLStock"), function(object, ...) {
    lst <- c(object, list(...))
    FLStocks(lst)
})

setMethod("FLStocks", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLStocks")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLStocks',  c(list(object=object), args))
	  }
  }
)

setMethod("FLStocks", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLStocks", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLIndices {{{
vFLSs <- function(object){
	
  # All items are FLIndex
  if(!all(unlist(lapply(object, is, 'FLIndex'))))
      return("Components must be FLIndex")	
	
	return(TRUE)
}

# class
setClass("FLIndices", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLIndices", signature(object="FLIndex"), function(object, ...) {
    lst <- c(object, list(...))
    FLIndices(lst)
})

setMethod("FLIndices", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLIndices")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLIndices',  c(list(object=object), args))
	  }
  }
)

setMethod("FLIndices", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLIndices", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLBiols {{{
vFLSs <- function(object){
	
  # All items are FLBiol
  if(!all(unlist(lapply(object, is, 'FLBiol'))))
      return("Components must be FLBiol")	
	
	return(TRUE)
}

# class
setClass("FLBiols", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLBiols", signature(object="FLBiol"), function(object, ...) {
    lst <- c(object, list(...))
    FLBiols(lst)
})

setMethod("FLBiols", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLBiols")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLBiols',  c(list(object=object), args))
	  }
  }
)

setMethod("FLBiols", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLBiols", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLSRs {{{
vFLSRs <- setClass("FLSRs", contains="FLComps",
	validity=function(object) {
    # All items are FLSR
    if(!all(unlist(lapply(object, is, 'FLSR'))))
      return("Components must be FLSR")	
	
	  return(TRUE)
  }
)

# constructor
setMethod("FLSRs", signature(object="FLSR"), function(object, ...) {
    lst <- c(object, list(...))
    FLSRs(lst)
})

setMethod("FLSRs", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLSRs")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLSRs',  c(list(object=object), args))
	  }
  }
)

setMethod("FLSRs", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)
    
    # names in args, ... 
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLSRs", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLPars {{{
# validity
vFLPs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLPar")) stop("Components must be FLPar")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLPars", contains="FLlst",
	validity=vFLPs
)

# constructor
setGeneric("FLPars", function(object, ...){
	standardGeneric("FLPars")
	}
)

setMethod("FLPars", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLPars", lst)
})

setMethod("FLPars", "missing", function(...){
	if(missing(...)){
		new("FLPars")
	} else { 
		lst <- list(...)
		new("FLPars", lst)
	}
})

setMethod("FLPars", "list", function(object){
	new("FLPars", object)
})

setMethod("FLPars", "FLPars", function(object){
	return(object)
}) # }}}

# FLModelSims {{{
# validity
vFLMs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLModelSim")) stop("Components must be FLModelSim")	
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLModelSims", contains="FLlst",
	validity=vFLMs
)

# constructor
setGeneric("FLModelSims", function(object, ...){
	standardGeneric("FLModelSims")
	}
)

setMethod("FLModelSims", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLModelSims", lst)
})

setMethod("FLModelSims", "missing", function(...){
	if(missing(...)){
		new("FLModelSims")
	} else { 
		lst <- list(...)
		new("FLModelSims", lst)
	}
})

setMethod("FLModelSims", "list", function(object){
	new("FLModelSims", object)
})

setMethod("FLModelSims", "FLModelSims", function(object){
	return(object)
}) # }}}
