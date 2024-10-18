# classesComp.R -
# FLCore/R/classesComp.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLComp   {{{

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
#' @aliases name,FLComp-method name<-,FLComp,character-method
#' @aliases desc,FLComp-method desc<-,FLComp,character-method
#' @aliases range,FLComp-method range<-,FLComp,numeric-method
#' @docType class
#' @section Slots: \describe{
#'    \item{name}{A character vector for the object name.}
#'    \item{desc}{A textual description of the object contents.}
#'    \item{range}{A named numeric vector with various values of quant and year ranges, plusgroup, fishing mortality ranges, etc. Elements are specific to each child class.}
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
    desc=character(1),
    range  = unlist(list(min=0, max=0, minyear=1, maxyear=1))),

  validity=function(object){

  # desc and name must have length 1
  if(length(desc(object)) > 1)
    return("desc can only be of length 1")

  if(length(name(object)) > 1)
    return("name can only be of length 1")

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
  }

  return(TRUE)
}

  )

invisible(createFLAccesors('FLComp', include=c('name', 'desc')))
#  }}}

# FLS      {{{

#' Class FLS
#'
#' A virtual class that forms the basis for the \code{\linkS4class{FLStock}}
#' and \code{\linkS4class{FLStockLen}} classes. No objects of this class can be
#' constructed.
#'
#' @section Validity: \describe{
#'     \item{None}{No particular validity checks}
#' }
#'
#' @name FLS
#' @aliases FLS FLS-class
#' @docType class
#' @section Slots:
#' \describe{
#'  \item{catch}{Total catch weight (\code{FLQuant}).}
#'  \item{catch.n}{Catch numbers (\code{FLQuant}).}
#'  \item{catch.wt}{Mean catch weights (\code{FLQuant}).}
#'  \item{desc}{Description of the stock (\code{character}).}
#'  \item{discards}{Total discards weight (\code{FLQuant}).}
#'  \item{discards.n}{Discard numbers (\code{FLQuant}).}
#'  \item{discards.wt}{Mean discard weights (\code{FLQuant}).}
#'  \item{landings}{Total landings weight (\code{FLQuant}).}
#'  \item{landings.n}{Landing numbers (\code{FLQuant}).}
#'  \item{landings.wt}{Landing weights (\code{FLQuant}).}
#'  \item{stock}{Total stock weight (\code{FLQuant}).}
#'  \item{stock.n}{Stock numbers (\code{FLQuant}).}
#'  \item{stock.wt}{Mean stock weights (\code{FLQuant}).}
#'  \item{m}{Natural mortality (\code{FLQuant}).}
#'  \item{m.spwn}{Proportion of natural mortality before spawning (\code{FLQuant}).}
#'  \item{mat}{Proportion mature (\code{FLQuant}).}
#'  \item{harvest}{Harvest rate or fishing mortality. The units of this slot
#'   should be set to 'harvest' or 'f' accordingly (\code{FLQuant}).}
#'  \item{harvest.spwn}{Proportion of harvest/fishing mortality before
#'   spawning (\code{FLQuant}).}
#'  \item{name}{Name of the stock (\code{character}).}
#'  \item{range}{Named numeric vector containing the quant and year ranges,
#'   the plusgroup and the quant range that the average fishing mortality should
#'   be calculated over (\code{numeric}).} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{as.data.frame},
#' \link{iter}, \link{propagate}, \link{qapply}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link{units,FLComp-method},
#' \link{units<-,FLComp,list-method}, \link[stats]{window}
#' @keywords classes

setClass("FLS",
  representation(
  "FLComp",
  catch        ="FLQuant",
  catch.n      ="FLQuant",
  catch.wt    ="FLQuant",
  discards    ="FLQuant",
  discards.n  ="FLQuant",
  discards.wt ="FLQuant",
  landings    ="FLQuant",
  landings.n  ="FLQuant",
  landings.wt ="FLQuant",
  stock        ="FLQuant",
  stock.n      ="FLQuant",
  stock.wt    ="FLQuant",
  m            ="FLQuant",
  mat          ="FLQuant",
  harvest      ="FLQuant",
  harvest.spwn="FLQuant",
  m.spwn      ="FLQuant",
  "VIRTUAL"
  ),
  prototype=prototype(
    range  = unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
    catch  = FLQuant(),
    catch.n  = FLQuant(),
    catch.wt= FLQuant(),
    discards= FLQuant(),
    discards.n = FLQuant(),
    discards.wt= FLQuant(),
    landings   = FLQuant(),
    landings.n = FLQuant(),
    landings.wt= FLQuant(),
    stock     = FLQuant(),
    stock.n   = FLQuant(),
    stock.wt = FLQuant(),
    m     = FLQuant(units="m"),
    mat     = FLQuant(units=""),
    harvest   = FLQuant(units="f"),
    harvest.spwn = FLQuant(units=""),
    m.spwn   = FLQuant(units="")
  ),
  validity=function(object) {

    ran <- range(object)
    dms <- dims(object)
    
    # CHECK year range
	  if(any(range(object, c("minyear", "maxyear")) != dms[c("minyear", "maxyear")]))
      return('Years in range do not match object dimensions')

  return(TRUE)}
)

invisible(createFLAccesors("FLS", exclude=c('name', 'desc', 'range', 'harvest')))  # }}}

# FLStock			{{{

#' Class FLStock
#'
#' A class for modelling a fish stock.
#'
#' The \code{FLStock} object contains a representation of a fish stock as
#' constructed for the purposes of scientific analysis and advice. This includes
#' information on removals (i.e. catches, landings and discards), maturity,
#' natural mortality and the results of an analytical assessment (i.e. estimates
#' of abundance and removal rates) .
#'
#' @name FLStock
#' @aliases FLStock-class catch,FLStock-method catch.n,FLStock-method
#' catch.wt,FLStock-method desc,FLStock-method discards,FLStock-method
#' discards.n,FLStock-method discards.wt,FLStock-method
#' harvest,FLStock-method harvest.spwn,FLStock-method
#' landings,FLStock-method landings.n,FLStock-method
#' landings.wt,FLStock-method mat,FLStock-method m,FLStock-method
#' m.spwn,FLStock-method name,FLStock-method range,FLStock-method
#' stock,FLStock-method stock.n,FLStock-method stock.wt,FLStock-method
#' catch<-,FLStock,FLQuant-method catch<-,FLStock,numeric-method
#' catch<-,FLStock,FLQuants-method catch.n<-,FLStock,FLQuant-method
#' catch.n<-,FLStock,numeric-method catch.wt<-,FLStock,FLQuant-method
#' catch.wt<-,FLStock,numeric-method desc<-,FLStock,character-method
#' discards<-,FLStock,FLQuant-method discards<-,FLStock,numeric-method
#' discards.n<-,FLStock,FLQuant-method discards.n<-,FLStock,numeric-method
#' discards.wt<-,FLStock,FLQuant-method
#' discards.wt<-,FLStock,numeric-method harvest<-,FLStock,character-method
#' harvest<-,FLStock,FLQuant-method harvest<-,FLStock,numeric-method
#' harvest.spwn<-,FLStock,FLQuant-method
#' harvest.spwn<-,FLStock,numeric-method landings<-,FLStock,FLQuant-method
#' landings<-,FLStock,numeric-method landings.n<-,FLStock,FLQuant-method
#' landings.n<-,FLStock,numeric-method
#' landings.wt<-,FLStock,FLQuant-method
#' landings.wt<-,FLStock,numeric-method mat<-,FLStock,FLQuant-method
#' mat<-,FLStock,numeric-method m<-,FLStock,FLQuant-method
#' m<-,FLStock,numeric-method m.spwn<-,FLStock,FLQuant-method
#' m.spwn<-,FLStock,numeric-method name<-,FLStock,character-method
#' range<-,FLStock,numeric-method stock<-,FLStock,FLQuant-method
#' stock<-,FLStock,numeric-method stock.n<-,FLStock,FLQuant-method
#' stock.n<-,FLStock,numeric-method stock.wt<-,FLStock,FLQuant-method
#' stock.wt<-,FLStock,numeric-method
#' @docType class
#' @section Slots:
#' \describe{
#'  \item{catch}{Total catch weight (\code{FLQuant}).}
#'  \item{catch.n}{Catch numbers (\code{FLQuant}).}
#'  \item{catch.wt}{Mean catch weights (\code{FLQuant}).}
#'  \item{discards}{Total discards weight (\code{FLQuant}).}
#'  \item{discards.n}{Discard numbers (\code{FLQuant}).}
#'  \item{discards.wt}{Mean discard weights (\code{FLQuant}).}
#'  \item{landings}{Total landings weight (\code{FLQuant}).}
#'  \item{landings.n}{Landing numbers (\code{FLQuant}).}
#'  \item{landings.wt}{Landing weights (\code{FLQuant}).}
#'  \item{stock}{Total stock weight (\code{FLQuant}).}
#'  \item{stock.n}{Stock numbers (\code{FLQuant}).}
#'  \item{stock.wt}{Mean stock weights (\code{FLQuant}).}
#'  \item{m}{Natural mortality (\code{FLQuant}).}
#'  \item{mat}{Proportion mature (\code{FLQuant}).}
#'  \item{harvest}{Harvest rate or fishing mortality. The units of this slot
#'   should be set to 'hr' or 'f' accordingly (\code{FLQuant}).}
#'  \item{harvest.spwn}{Proportion of harvest/fishing mortality before
#'   spawning (\code{FLQuant}).}
#'  \item{m.spwn}{Proportion of natural mortality before spawning
#'   (\code{FLQuant}).}
#'  \item{name}{Name of the stock (\code{character}).}
#'  \item{desc}{Description of the stock (\code{character}).}
#'  \item{range}{Named numeric vector containing the quant and year ranges,
#'   the plusgroup and the quant range that the average fishing mortality should
#'   be calculated over (\code{numeric}).} }
#' @template Accessors
#' @template Constructors
#' @param plusgroup Plusgroup age, to be stored in range
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
#' summary(ple4)
#'
#' # get the landings slot and assign values to it
#'   landings(ple4)
#'   landings(ple4) <- apply(landings.n(ple4)*landings.wt(ple4),2,sum)
#'
#' # perform similar calculation as the preceding apply function
#'   discards(ple4) <- computeDiscards(ple4)
#'   catch(ple4) <- computeCatch(ple4)
#'   catch(ple4) <- computeCatch(ple4, slot="all")
#'
#' # set the units of the harvest slot of an FLStock object
#'   harvest(ple4) <- 'f'
#'
#' # subset and trim the FLStock
#'   ple4[,1]
#'   trim(ple4, age=2:6, year=1980:1990)
#'
#' # Calculate SSB, and SSB per recruit at zero fishing mortality
#'   ssb(ple4)
#'   ssbpurec(ple4)
#'
#' # Coerce an FLStock to an FLBiol
#'   biol <- as(ple4, "FLBiol")
#'
#' # Initialise an FLSR object from an FLStock
#'   flsr <- as.FLSR(ple4)
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
  validity=function(object) {

	names <- names(getSlots('FLStock')[getSlots('FLStock')=="FLQuant"])
	for(i in names){
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[c(2,3,4)]),
			unlist(dimnames(slot(object, i))[c(2,3,4)])))
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
	
    ran <- range(object)
    dms <- dims(object)

    # CHECK minfbar, maxfbar and plusgroup match object dimensions
    if(!is.na(dms$min) & ran["minfbar"] < dms$min)
      return("minfbar is lower than first age")
    if(!is.na(dms$max) & ran["maxfbar"] > dms$max)
      return("maxfbar is higher than last age")
    if(!is.na(ran["plusgroup"]) & ran["plusgroup"] > dms$max)
      return("plusgroup is higher than last age")

	return(TRUE)}
) # }}}

# FLStockLen      {{{

#' Class FLStockLen
#'
#' A class for modelling a length-structured fish stock.
#'
#' The \code{FLStockLen} object contains a length based representation of a
#' fish stock. This includes information on removals (i.e. catches, landings and
#' discards), maturity, natural mortality and the results of an analytical
#' assessment (i.e. estimates of abundance and removal rates).
#'
#' @name FLStockLen
#' @docType class
#' @rdname FLStockLen
#' @aliases FLStockLen-class catch,FLStockLen-method catch.n,FLStockLen-method
#' catch.wt,FLStockLen-method desc,FLStockLen-method
#' discards,FLStockLen-method discards.n,FLStockLen-method
#' discards.wt,FLStockLen-method halfwidth,FLStockLen-method
#' harvest,FLStockLen-method harvest.spwn,FLStockLen-method
#' landings,FLStockLen-method landings.n,FLStockLen-method
#' landings.wt,FLStockLen-method mat,FLStockLen-method m,FLStockLen-method
#' m.spwn,FLStockLen-method name,FLStockLen-method range,FLStockLen-method
#' stock,FLStockLen-method stock.n,FLStockLen-method
#' stock.wt,FLStockLen-method
#' catch<-,FLStockLen,FLQuant-method catch<-,FLStockLen,numeric-method
#' catch.n<-,FLStockLen,FLQuant-method catch.n<-,FLStockLen,numeric-method
#' catch.wt<-,FLStockLen,FLQuant-method
#' catch.wt<-,FLStockLen,numeric-method desc<-,FLStockLen,character-method
#' discards<-,FLStockLen,FLQuant-method
#' discards<-,FLStockLen,numeric-method
#' discards.n<-,FLStockLen,FLQuant-method
#' discards.n<-,FLStockLen,numeric-method
#' discards.wt<-,FLStockLen,FLQuant-method
#' discards.wt<-,FLStockLen,numeric-method halfwidth<-,FLStockLen,-method
#' harvest<-,FLStockLen,character-method
#' harvest<-,FLStockLen,FLQuant-method harvest<-,FLStockLen,numeric-method
#' harvest.spwn<-,FLStockLen,FLQuant-method
#' harvest.spwn<-,FLStockLen,numeric-method
#' landings<-,FLStockLen,FLQuant-method
#' landings<-,FLStockLen,numeric-method
#' landings.n<-,FLStockLen,FLQuant-method
#' landings.n<-,FLStockLen,numeric-method
#' landings.wt<-,FLStockLen,FLQuant-method
#' landings.wt<-,FLStockLen,numeric-method mat<-,FLStockLen,FLQuant-method
#' mat<-,FLStockLen,numeric-method m<-,FLStockLen,FLQuant-method
#' m<-,FLStockLen,numeric-method m.spwn<-,FLStockLen,FLQuant-method
#' m.spwn<-,FLStockLen,numeric-method name<-,FLStockLen,character-method
#' range<-,FLStockLen,numeric-method stock<-,FLStockLen,FLQuant-method
#' stock<-,FLStockLen,numeric-method stock.n<-,FLStockLen,FLQuant-method
#' stock.n<-,FLStockLen,numeric-method
#' stock.wt<-,FLStockLen,FLQuant-method
#' stock.wt<-,FLStockLen,numeric-method
#' @section Slots:
#' \describe{
#'  \item{halfwidth}{The middle of the length bins (\code{numeric}).}
#'  \item{catch}{Total catch weight (\code{FLQuant}).}
#'  \item{catch.n}{Catch numbers (\code{FLQuant}).}
#'  \item{catch.wt}{Mean catch weights (\code{FLQuant}).}
#'  \item{discards}{Total discards weight (\code{FLQuant}).}
#'  \item{discards.n}{Discard numbers (\code{FLQuant}).}
#'  \item{discards.wt}{Mean discard weights (\code{FLQuant}).}
#'  \item{landings}{Total landings weight (\code{FLQuant}).}
#'  \item{landings.n}{Landing numbers (\code{FLQuant}).}
#'  \item{landings.wt}{Landing weights (\code{FLQuant}).}
#'  \item{stock}{Total stock weight (\code{FLQuant}).}
#'  \item{stock.n}{Stock numbers (\code{FLQuant}).}
#'  \item{stock.wt}{Mean stock weights (\code{FLQuant}).}
#'  \item{m}{Natural mortality (\code{FLQuant}).}
#'  \item{mat}{Proportion mature (\code{FLQuant}).}
#'  \item{harvest}{Harvest rate or fishing mortality. The units of this slot
#'   should be set to 'harvest' or 'f' accordingly (\code{FLQuant}).}
#'  \item{harvest.spwn}{Proportion of harvest/fishing mortality before
#'   spawning (\code{FLQuant}).}
#'  \item{m.spwn}{Proportion of natural mortality before spawning
#'   (\code{FLQuant}).}
#'  \item{name}{Name of the stock (\code{character}).}
#'  \item{desc}{Description of the stock (\code{character}).}
#'  \item{range}{Named numeric vector containing the quant and year ranges,
#'   the plusgroup and the quant range that the average fishing mortality should
#'   be calculated over (\code{numeric}).} }
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link{as.FLBiol}, \link{as.FLSR},
#' \link{computeCatch}, \link{computeDiscards}, \link{computeLandings},
#' \link[graphics]{plot}, \link{ssb}, \link{ssbpurec}, \link{trim},
#' \link{FLComp}
#' @keywords classes
#'
setClass("FLStockLen",
  representation(
  "FLS",
  halfwidth = "numeric"
  ),
  prototype=prototype(
    range  = unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
    halfwidth = as.numeric(NA),
    catch  = FLQuant(dimnames=list(len=as.numeric(NA))),
    catch.n  = FLQuant(dimnames=list(len=as.numeric(NA))),
    catch.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    discards= FLQuant(dimnames=list(len=as.numeric(NA))),
    discards.n = FLQuant(dimnames=list(len=as.numeric(NA))),
    discards.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    landings   = FLQuant(dimnames=list(len=as.numeric(NA))),
    landings.n = FLQuant(dimnames=list(len=as.numeric(NA))),
    landings.wt= FLQuant(dimnames=list(len=as.numeric(NA))),
    stock     = FLQuant(dimnames=list(len=as.numeric(NA))),
    stock.n   = FLQuant(dimnames=list(len=as.numeric(NA))),
    stock.wt = FLQuant(dimnames=list(len=as.numeric(NA))),
    m     = FLQuant(dimnames=list(len=as.numeric(NA))),
    mat     = FLQuant(dimnames=list(len=as.numeric(NA))),
    harvest   = FLQuant(dimnames=list(len=as.numeric(NA))),
    harvest.spwn = FLQuant(dimnames=list(len=as.numeric(NA))),
    m.spwn   = FLQuant(dimnames=list(len=as.numeric(NA)))
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

# FLI    {{{

#' Class FLI
#'
#' A VIRTUAL class that holds data and parameters related to abundance indices.
#'
#' @name FLI
#' @docType class
#' @aliases FLI FLI-class
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
#' \link{iter}, \link[graphics]{plot}, \link[FLCore]{propagate},
#' \link[base]{summary}, \link[base]{transform}, \link{trim},
#' \link[stats]{window}, \link{FLComp}
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
  
  # iter is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter)))) > 2)
     stop("iter dimension in FLI can only be '1' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    # TODO: Double check relaxation of unit
    if(!identical(dimnms[[i]][c(-1,-3,-6)], dimnms[[1]][c(-1,-3,-6)]))
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
#' A class for modelling abundance indices.
#'
#' The \code{FLIndex} object holds data and parameters related to abundance
#' indices.
#'
#' @name FLIndex
#' @docType class
#' @aliases FLIndex-class catch.n,FLIndex-method catch.wt,FLIndex-method
#' desc,FLIndex-method distribution,FLIndex-method effort,FLIndex-method
#' index,FLIndex-method index.q,FLIndex-method index.var,FLIndex-method
#' name,FLIndex-method range,FLIndex-method sel.pattern,FLIndex-method
#' type,FLIndex-method
#' catch.n<-,FLIndex,FLQuant-method catch.n<-,FLIndex,numeric-method
#' catch.wt<-,FLIndex,FLQuant-method catch.wt<-,FLIndex,numeric-method
#' desc<-,FLIndex,character-method distribution<-,FLIndex,character-method
#' effort<-,FLIndex,FLQuant-method index<-,FLIndex,FLQuant-method
#' index<-,FLIndex,numeric-method index.q<-,FLIndex,FLQuant-method
#' index.q<-,FLIndex,numeric-method index.var<-,FLIndex,FLQuant-method
#' index.var<-,FLIndex,numeric-method name<-,FLIndex,character-method
#' range<-,FLIndex,numeric-method sel.pattern<-,FLIndex,FLQuant-method
#' sel.pattern<-,FLIndex,numeric-method type<-,FLIndex,character-method
#' @section Slots: \describe{ \item{type}{Type of index (\code{character}).}
#' \item{distribution}{Statistical distribution of the index values
#' (\code{character}).} \item{index}{Index values (\code{FLQuant}).}
#'     \item{index.var}{Variance of the index (\code{FLQuant}).}
#'     \item{catch.n}{Catch numbers used to create the index (\code{FLQuant}).}
#'     \item{catch.wt}{Catch weight of the index (\code{FLQuant}).}
#'     \item{effort}{Effort used to create the index (\code{FLQuant}).}
#'     \item{sel.pattern}{Selection pattern for the index (\code{FLQuant}).}
#' \item{index.q}{Catchability of the index (\code{FLQuant}).} \item{name}{Name
#' of the stock (\code{character}).} \item{desc}{General description of the
#' object (\code{character}).} \item{range}{Named numeric vector containing the
#' quant and year ranges, the plusgroup, and the period of the year, expressed
#' as proportions of a year, that corresponds to the index (\code{numeric}).} }
#' @author The FLR Team
#' @seealso \link{computeCatch}, \link{dims}, \link{iter},
#' \link[graphics]{plot}, \link{propagate}, \link[base]{summary},
#' \link[base]{transform}, \link{trim}, \link[stats]{window}, \link{FLComp}
#' @keywords classes
#' @examples
#'
#' # Create an FLIndex object.
#' fli <- FLIndex(index=FLQuant(rnorm(8), dim=c(1,8)), name="myTestFLindex")
#' summary(fli)
#' index(fli)
#'
#' # Creat an FLIndex object using an existing FLQuant object.
#'   data(ple4)
#'   # Create a perfect index of abundance from abundance at age
#'     fli2 <- FLIndex(index=stock.n(ple4))
#'   # Add some noise around the signal
#'     index(fli2) <- index(fli2)*exp(rnorm(1, index(fli2)-index(fli2), 0.1))
#'
setClass("FLIndex",
  representation(
    "FLI",
    type         = "character"),
    prototype=prototype(
    type         = character(0)),
  validity=function(object) {

    # quant is 1 or N
    if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
       stop("quant dimension in FLI can only be 'all' or n")

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
#' A class for modelling biomass indices.
#'
#' The \code{FLIndexBiomass} object holds data and parameters related to
#' biomass indices.
#'
#' @name FLIndexBiomass
#' @docType class
#' @aliases FLIndexBiomass-class catch.n,FLIndexBiomass-method catch.wt,FLIndexBiomass-method
#' desc,FLIndexBiomass-method distribution,FLIndexBiomass-method
#' effort,FLIndexBiomass-method index,FLIndexBiomass-method
#' index.q,FLIndexBiomass-method index.var,FLIndexBiomass-method
#' name,FLIndexBiomass-method range,FLIndexBiomass-method
#' sel.pattern,FLIndexBiomass-method
#' catch.n<-,FLIndexBiomass,FLQuant-method
#' catch.n<-,FLIndexBiomass,numeric-method
#' catch.wt<-,FLIndexBiomass,FLQuant-method
#' catch.wt<-,FLIndexBiomass,numeric-method
#' desc<-,FLIndexBiomass,character-method
#' distribution<-,FLIndexBiomass,character-method
#' effort<-,FLIndexBiomass,FLQuant-method
#' index<-,FLIndexBiomass,FLQuant-method
#' index<-,FLIndexBiomass,numeric-method
#' index.q<-,FLIndexBiomass,FLQuant-method
#' index.q<-,FLIndexBiomass,numeric-method
#' index.var<-,FLIndexBiomass,FLQuant-method
#' index.var<-,FLIndexBiomass,numeric-method
#' name<-,FLIndexBiomass,character-method
#' range<-,FLIndexBiomass,numeric-method
#' sel.pattern<-,FLIndexBiomass,FLQuant-method
#' sel.pattern<-,FLIndexBiomass,numeric-method
#' @section Slots: \describe{ \item{distribution}{Statistical distribution of
#' the index values (\code{character}).} \item{index}{Index values
#' (\code{FLQuant}).} \item{index.var}{Variance of the index (\code{FLQuant}).}
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

newlogLik <- function(value=as.numeric(NA), nall=as.numeric(NA),
  nobs=as.numeric(NA), df=as.numeric(NA)) {

  attr(value, 'nall') <- nall
  attr(value, 'nobs') <- nobs
  attr(value, 'df') <- df
  attr(value, 'class') <- 'logLik'

  return(value)
}

#' Class FLModel
#'
#' A virtual class for statistical models
#'
#' The \code{FLModel} class provides a virtual class that developers of various
#' statistical models can use to implement classes that allow those models to
#' be tested, fitted and presented.
#'
#' Slots in this class attempt to map all the usual outputs for a modelling
#' exercise, together with the standard inputs. Input data are stored in slots
#' created by a specified class based on \code{FLModel}. See for example
#' \code{\linkS4class{FLSR}} for a class used for stock-recruitment models.
#'
#' The \code{initial} slot contains a function used to obtain initial values for
#' the numerical solver. It can also contain two attributes, \code{upper} and
#' \code{lower} that limit the sarch area for each parameter.
#'
#' Various fitting algorithms, similar to those present in the basic R packages,
#' are currently available for \code{FLModel}, including \code{\link{fmle}},
#' \code{\link{nls-FLCore}} and \code{\link[stats]{glm}}.
#'
#' @name FLModel
#' @aliases FLModel-class FLModel FLModel-methods FLModel,formula-method
#' FLModel,missing-method FLModel,character-method FLModel,function-method
#' @docType class
#' @section Slots: \describe{
#'   \item{name}{Name of the object, \code{character}.}
#'   \item{desc}{Description of the object, \code{character}.}
#'   \item{range}{Range, \code{numeric}.}
#'   \item{distribution}{Associated error probability dfistribution, \code{factor}.}
#'   \item{fitted}{Estimated values, \code{FLQuant}.}
#'   \item{residuals}{Residuals obtained from the model fit, \code{FLQuant}.}
#'   \item{model}{Model formula, \code{formula}.}
#'   \item{gr}{Function returning the gradient of the likelihood, \code{function}.}
#'   \item{logl}{Log-likelihood function. \code{function}.}
#'   \item{initial}{Function returning initial parameter values for the
#'     optimizer, as an object of class \code{FLPar}, \code{function}.}
#'   \item{params}{Estimated parameter values, \code{FLPar}.}
#'   \item{logLik}{Value of the log-likelihood, \code{logLik}.}
#'   \item{vcov}{Variance-covariance matrix, \code{array}.}
#'   \item{hessian}{Hessian matrix obtained from the parameter fitting, \code{array}.}
#'   \item{details}{extra information on the model fit procedure, \code{list}.} }
#' @author The FLR Team
#' @seealso \link[stats]{AIC}, \link[stats]{BIC}, \link{fmle},
#' \link[stats]{nls}, \link{FLComp}
#' @keywords classes
#' @examples
#'
#' # Normally, FLModel objects won't be created if "class" is not set
#'   summary(FLModel(length~width*alpha))
#'
#' # Objects of FLModel-based classes use their own constructor,
#' # which internally calls FLModel
#'   fsr <- FLModel(rec~ssb*a, class='FLSR')
#'   is(fsr)
#'   summary(fsr)
#'
#' # An example constructor method for an FLModel-based class
#'   # Create class FLGrowth with a single new slot, 'mass'
#'     setClass('FLGrowth', representation('FLModel', mass='FLArray'))
#'
#'   # Define a creator method based on FLModel
#' 		 setGeneric("FLGrowth", function(object, ...) standardGeneric("FLGrowth"))
#'     setMethod('FLGrowth', signature(object='ANY'),
#'       function(object, ...) return(FLModel(object, ..., class='FLGrowth')))
#'     setMethod('FLGrowth', signature(object='missing'),
#'       function(...) return(FLModel(formula(NULL), ..., class='FLGrowth')))
#'
#'   # Define an accessor method
#'     setMethod('mass', signature(object='FLGrowth'),
#'       function(object) return(slot(object, 'mass')))
#'

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
    residuals=FLQuant(),
    logLik=newlogLik()),
	validity=function(object)
		{
	  # All FLArray slots are of the same exact class
  	flarr <- getSlotNamesClass(object, 'FLArray')
	  class <- class(slot(object, 'fitted'))
  	for(i in flarr[-1])
    	if(!is(slot(object, i), class))
      	return(paste('FLQuant/FLCohort slots in object should all be of the same class: ',
        	i))
	
		return(TRUE)
		}
)
invisible(createFLAccesors("FLModel", exclude=c('name', 'desc', 'range', 'params', 'distribution')))
# }}}
