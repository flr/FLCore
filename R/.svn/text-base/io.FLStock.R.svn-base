# io.FLStock.R - read and write assessment input files into an FLStock

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$

# readFLStock		{{{
readFLStock <- function(file, type = "VPA", name, desc = paste("Imported from a", 
    type, "file. (", file, "). ", date()), m = 0.2, quant="age", quiet=TRUE,
    no.discards=FALSE, harvest.units, sep="")
	{
    ow <- options()$warn
    options(warn = -1)
    on.exit(options(warn = ow))

    res <- switch(type,
                  VPA = readVPA(file, quiet=quiet, sep=sep),
                  Adapt = readAdaptFile(file,m),
                  PA = readPAFile(file),
                  CSA = readCSAFile(file),
                  stop("type must be either 'VPA', 'Adapt', 'PA' or 'CSA'!"))
                  
    Mat <- res@stock.wt
    Mat[, , , , ] <- NA
    Dim <- dim(Mat)
    Mat0 <- Mat
    Mat0[, , , , ] <- 0
    if (is.null(res@landings.n)  || !all(dim(res@landings.n)  == Dim)) 
        res@landings.n  <- Mat
    if (is.null(res@landings.wt) || !all(dim(res@landings.wt) == Dim)) 
        res@landings.wt <- Mat
    if (is.null(res@catch.n)     || !all(dim(res@catch.n)     == Dim)) 
        res@catch.n     <- Mat
    if (is.null(res@catch.wt)    || !all(dim(res@catch.wt)    == Dim)) 
        res@catch.wt    <- Mat
    if (is.null(res@discards.n)  || !all(dim(res@discards.n)  == Dim)) 
        res@discards.n  <- Mat
    if (is.null(res@discards.wt) || !all(dim(res@discards.wt) == Dim)) 
        res@discards.wt <- Mat
    if (is.null(res@m)           || !all(dim(res@m)           == Dim)) 
        res@m           <- Mat
    if (is.null(res@stock.wt)    || !all(dim(res@stock.wt)    == Dim)) 
        res@stock.wt    <- Mat
    if (is.null(res@mat)         || !all(dim(res@mat)         == Dim)) 
        res@mat         <- Mat
    if (is.null(res@stock.n)     || !all(dim(res@stock.n)     == Dim)) 
        res@stock.n     <- Mat
    if (is.null(res@harvest)           || !all(dim(res@harvest)     == Dim)) 
        res@harvest     <- Mat
    if (is.null(res@harvest.spwn)      || !all(dim(res@harvest.spwn)== Dim)) 
        res@harvest.spwn<- Mat
    if (is.null(res@m.spwn)      || !all(dim(res@m.spwn)      == Dim)) 
        res@m.spwn      <- Mat
    Mat <- Mat[1, , , , ]

    if (is.null(res@catch) || !all(dim(res@catch) == dim(Mat)))
        res@catch <- Mat
    if (is.null(res@discards) || !all(dim(res@discards) == dim(Mat)))
        res@discards <- Mat
    if (is.null(res@landings) || !all(dim(res@landings) == dim(Mat)))
        res@landings <- Mat
    pars <- dims(res@stock.wt)
    res@range <- unlist(list(min = pars$min, max = pars$max,
        plusgroup = NA, minyear = pars$minyear, maxyear = pars$maxyear,minfbar = pars$min, maxfbar = pars$max))
    if (length(res@name) < 1 | !missing(name))
        res@name <- as.character(name)
    if (!is.null(desc)) 
        res@desc <- as.character(desc)
        
    names. <- names(getSlots(class(res))[getSlots(class(res))=="FLQuant"])

    for (s. in names.) {
        quant(slot(res, s.)) <- quant
    }

   stock(res) <- computeStock(res)

    if(no.discards)
      {
      discards(res) <- 0
      discards.n(res) <- 0
      discards.wt(res) <- 0
      catch(res) <- computeCatch(res, 'all')
      }

#    if(type=="VPA") units(harvest(res)) <- "f"
#    if(type=="Adapt") units(harvest(res)) <- "f"
#    if(type=="PA") units(harvest(res)) <- "f"
#    if(type=="CSA") units(harvest(res)) <- "hr"
    if(missing(harvest.units)) harvest.units  <- switch(type, VPA = "f", Adapt = "f", PA = "f", CSA = "hr")
    units(harvest(res)) <- harvest.units
   
    return(res)
}	# }}}

# writeFLStock	{{{
writeFLStock <- function(FLStock, output.file=FLStock@name, type="VPA") {
	if (!inherits(FLStock, "FLStock"))
		stop("FLStock must be an 'FLStock' object!")
	switch(type,
		"VPA" = writeVPA(FLStock, output.file),
		stop("type must be 'VPA'!"))
}	# }}}
