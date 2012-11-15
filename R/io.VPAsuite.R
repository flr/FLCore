# .R - 
# /R/.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $


# readVPAFile		{{{
readVPAFile <- function(file, sep = "", units = "NA", quiet = TRUE) {	
    if (!file.exists(file)){
        if(quiet==TRUE) stop()
        if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
    }

    switch (as.character(file.access(file)),
        "0"  = info <- read.table(file, colClasses = "character", 
                                  header = FALSE, fill = TRUE, skip = 1, 
                                  nrows = 4, sep = sep, comment.char='#'),
        "-1" = info <- matrix(rep("0", 8), nrow = 4, ncol = 2))

    misc <- info[1, 1]
    type <- info[1, 2]
    dfor <- info[4, 1]

    # Switch for file type (dfor; e.g. matrix, scalar, vector)
    switch(misc,
      "1" = {range <- scan(file, skip = 2, nlines = 2, sep = sep, comment.char='#',
        quiet=quiet)
        ages <- range[3:4]
        nages <- ages[2] - ages[1] + 1
        yrs <- range[1:2]
        nyrs <- yrs[2] - yrs[1] + 1
        dms <- list(age=as.character(ages[1]:ages[2]),year=as.character(yrs[1]:yrs[2]))
        switch(dfor,
          "1" = a. <- as.FLQuant(matrix(t(read.table(file = file, skip = 5,
            nrows = nyrs, sep = sep, comment.char='#')[, 1:nages]), nrow=nages,
            ncol=nyrs),dimnames= dms),
          "2" = a. <- as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep,
            comment.char='#', quiet=quiet)[1:nages], nyrs), nrow = nages,
            ncol = nyrs), dimnames = dms),
          "3" = a. <- as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep,
            comment.char='#', quiet=quiet)[1], nyrs * nages),nrow = nages,
            ncol = nyrs), dimnames = dms),
          "5" = {
            dms <- list(age="all",year=as.character(yrs[1]:yrs[2]))
            a. <- as.FLQuant(matrix(t(read.table(file = file, skip = 5,
              nrows = nyrs, sep = sep)[,1]), nrow = 1, ncol = nyrs), dimnames = dms)
            }
         )
         #needed to go from int to double
         a. <-  FLQuant(as.numeric(a.),dimnames=dimnames(a.))
         return(a.)
       },
       "0" = cat("Invalid file. Cannot read file:-", file, "\n"),
       if(quiet != TRUE) cat("Tuning file", file, "not read", "\n")
    )
}	# }}}

# readVPA		{{{
readVPA <- function(file, sep = "", quiet=TRUE) {
    if (!file.exists(file)){
        if(quiet==TRUE) stop()
        if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
    }
    dir    <- dirname(file)
    files. <- scan(file, what = "character", skip = 2, sep = sep, quiet=quiet)
    
    for(i in seq(length(files.)))
      if (!grepl(dir,files.[i]))    
        files.[i] <- file.path(dir, files.[i], fsep = .Platform$file.sep)
    
    range1 <- scan(files.[1], skip = 2, nlines = 1, sep = sep, quiet=quiet)
    range2 <- scan(files.[1], skip = 3, nlines = 1, sep = sep, quiet=quiet)
    range  <- c(range1[1:2],range2[1:2])
    
    ages <- range[3:4]
    yrs <- range[1:2]

    FLStock. <- FLStock(catch.n=FLQuant(NA, dimnames = list(age = ages[1]:ages[2], year = yrs[1]:yrs[2], unit = "unique", season = "all",  area = "unique")))
    
    for (i in files.) {
        if (!file.exists(i)){
           if(quiet != TRUE) cat("File ", i, "does not exist", "\n")
           }
        if (file.exists(i)) {
            a.   <-  readVPAFile(i, sep=sep, quiet=quiet)

            switch(as.character(scan(i, skip = 1, nlines = 1, sep = sep, comment.char='#', quiet=TRUE)[2]),
            "1" = FLStock.@landings    <-a.,
            "2" = FLStock.@landings.n  <-a.,
            "3" = FLStock.@landings.wt <-a.,
            "4" = FLStock.@stock.wt    <-a.,
            "5" = FLStock.@m           <-a.,
            "6" = FLStock.@mat         <-a.,
            "7" = FLStock.@harvest.spwn<-a.,
            "8" = FLStock.@m.spwn      <-a.,
            "21"= FLStock.@discards    <-a.,
            "22"= FLStock.@discards.n  <-a.,
            "23"= FLStock.@discards.wt <-a.,
            "24"= FLStock.@catch       <-a.,
            "25"= FLStock.@catch.n     <-a.,
            "26"= FLStock.@catch.wt    <-a.,
            "27"= FLStock.@harvest     <-a.,
            "28"= FLStock.@stock.n     <-a. )
        }
    }

    FLStock.@range <- c(min = ages[1], max = ages[2],
        plusgroup = ages[2], minyear = yrs[1], maxyear = yrs[2])
    FLStock.@desc <- paste("Imported from a VPA file (",
        file, "). ", date(), sep="")
    FLStock.@name <- scan(file, nlines = 1, what = character(0),
        sep = "\n", quiet=TRUE)

    return(FLStock.)
}	# }}}

# writeVPA - Mark Payne, DTU-Aqua		{{{
writeVPA <- function(FLStock, output.file=FLStock@name,slots="missing") {
    #Check for dimensions that Lowestoft VPA can't handle 
    if(dims(FLStock)$iter >1) stop("FLStock object contains more than
one iteration. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$area >1) stop("FLStock object contains more than
one area. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$season >1) stop("FLStock object contains more than
one season. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$unit >1) stop("FLStock object contains more than
one unit. Lowestoft VPA can't handle this unfortunately.")

    #List of configuration information
    config.df  <-  as.data.frame(rbind(
      c(1,  5, "landings", "LATON", "total landings"),
      c(2,  1, "landings.n", "LANUM", "landings-at-age"),
      c(3,  1, "landings.wt", "WELAND", "landings weight-at-age"),
      c(4,  1, "stock.wt", "WEST", "stock weight-at-age"),
      c(5,  1, "m", "NATMOR", "natural mortality"),
      c(6,  1, "mat", "MATPROP", "maturity-at-age ogive"),
      c(7,  1, "harvest.spwn", "FPROP", "proportion of F before spawning"), 
      c(8,  1, "m.spwn", "MPROP", "proportion of M before spawning"),
      c(12, 1, "harvest", "F", "fishing mortality"),
      c(13, 1, "stock.n", "N", "stock numbers at age"),
      c(21, 5, "discards", "DISTON", "total discards"),
      c(22, 1, "discards.n", "DISNUM", "discards-at-age"),
      c(23, 1, "discards.wt", "WEDIS", "discards weight-at-age"),
      c(24, 5, "catch", "CATON", "total catch"),                                  
      c(25, 1, "catch.n", "CANUM", "catch-at-age"),
      c(26, 1, "catch.wt", "WECA", "catch weight-at-age")))
    colnames(config.df) <-  c("idx","dfi","stock.slot","file.ext","desc")
    config.df$idx=as.numeric(as.character(config.df$idx))
    config.df$dfi=as.numeric(as.character(config.df$dfi))
    
    config.df   <- as.data.frame(config.df)

    #Only write the harvest slot if units are f.
    if(!any(units(FLStock@harvest) %in% c("f","F")))
    {
      config.df <- config.df[-which(config.df$stock.slot=="harvest"),]
      warning("Harvest slot is not a fishing mortality - it will not
        be written as part of the output.")
    }
    
    #calculates the numbers of ages
    nage  <- FLStock@range[2]-FLStock@range[1]+1
    #calculates the number of years
    nyear <- dims(FLStock)$year

    #Function to write files. Based on original code by David Bromley, CEFAS
    writeVPAFile <- function(file.ext,stock.slot,idx,dfi,desc)
    {
      # handles the annoying "strings as factors" option
      stock.slot <- as.character(stock.slot)
      # open the output file connections
      temp <- try(file(paste(output.file,"-",file.ext,".txt",sep=""),
        "w"),silent=TRUE)
      if(is(temp,"try-error"))
      {
        stop(paste("Cannot open output file. The supplied name,
        \"",output.file,"\", may not be a valid filename. Try setting the
        output.file argument.",sep=""))
        }
        # adds the VPA format info to the begining of each file
        cat(paste(FLStock@name, desc, "-",stock.slot,
          "(units :",units(slot(FLStock,stock.slot)),")\n"), file=temp)
        cat(1, idx,"\n", file=temp, sep="\t")
        cat(FLStock@range["minyear"], FLStock@range["maxyear"], "\n",
          file=temp, sep="\t")
        cat(FLStock@range["min"], FLStock@range["max"], "\n", file=temp,  sep="\t")
        cat(dfi,"\n",  file=temp)
        # append the data to the file
        if (dfi==1) 			  
           write(matrix(slot(FLStock,stock.slot), nrow=nage, ncol=nyear),ncolumns=nage, file=temp)
        else if (dfi==5)
           write(matrix(slot(FLStock,stock.slot), nrow=1, ncol=nyear),ncolumns=1, file=temp)
      
        close(temp)
    }

    #Now write the data out
    if(missing(slots))
    {
      #Write all the data out, and an index file
      #Write the slots sequentially
      #do.call(mapply,list(config.df$file.ext, config.df$stock.slot, config.df$idx, config.df$desc, FUN=writeVPAFile))
      mapply(config.df$file.ext, config.df$stock.slot, config.df$idx,config.df$dfi, config.df$desc, FUN=writeVPAFile)
      # produces the index file
      temp <- file(paste(output.file, "-INDEX.txt", sep=""), "w")
      cat(FLStock@name, "\n", file=temp)
      cat(1,paste(output.file,"-",config.df$file.ext,".txt",sep=""),
        file=temp, sep="\n")
      close(temp)
    } else 
    {
      output.config <-  config.df[config.df$stock.slot %in% slots,]
      do.call(mapply,c(as.list(output.config),FUN=writeVPAFile))
    }
    return(invisible(NULL))
}	# }}}
