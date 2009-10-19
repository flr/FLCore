# io.FLIndex - 

# Author: FLR Team
# Last Change: 11 Feb 2009 11:24
# $Id$

# Reference:
# Notes:

# TODO 26/11/2004 iagoazti: Review checkIndex, add write.FLIndices function

## writeIndicesVPA		{{{
writeIndicesVPA <- function(FLIndices., file.) {	

    # opens connection to the output file
	temp <- file(file., "w")
    on.exit(close(temp))

    # Enters files title	
	cat(paste("Originally", FLIndices.@desc, sep=" "), "\n", file=temp)

    # Enters the code specifying the number of fleets
	cat(length(FLIndices.)+100, "\n", file=temp)

    for(i in 1:length(FLIndices.)) {

        # Retrieves individual fleet from the FLIndices list
    	FLIndex. <- FLIndices.[[i]]
        nages <- FLIndex.@range[2] - FLIndex.@range[1] + 1
        nyrs  <- FLIndex.@range[5] - FLIndex.@range[4] + 1

        # creates empty matrix for the catch and effort data
        catch  <- matrix(rep(0, (nyrs*nages)+nyrs), nrow=nyrs, ncol=nages+1)

        # Retrieves effort data from each FLIndex object
        effort <- matrix(FLIndex.@effort)
        
		# Retrieves index data from each FLIndex object
        index  <- t(matrix(FLIndex.@index, nrow=nages, ncol=nyrs))

        # converts index data into catch data and adds to the empty matrix
        for (j in 1:nages) {
            catch[,j+1]  <- index[,j]*effort[j,]
        }

        # appends the eff data onto the front of the matrix which now contains catch data
        catch[,1] <- effort

        # Writes relevent info for each individual fleet to the file 
        cat(FLIndex.@name, "\n", file=temp)
        cat(FLIndex.@range[4], FLIndex.@range[5], "\n", file=temp, sep="\t")
        cat(1, 1, FLIndex.@range[6], FLIndex.@range[7], "\n", file=temp, sep="\t")
        cat(FLIndex.@range[1], FLIndex.@range[2], "\n", file=temp, sep="\t")

        # Appends the data for each individual fleet to the file
        write(t(catch), file=temp, ncolumns=nages+1)
    }
}	# }}}

## writeIndicesICA		{{{
writeIndicesICA <- function(FLIndices., file., ssb.) {

    num <- length(FLIndices.)

    # opens connection to the output file
	temp <- file(file., "w")
    on.exit(close(temp))

    # Enters files title	
	cat(paste("Originally", FLIndices.@desc, sep=" "), "\n", file=temp)

    # Enters the code specifying the number of fleets
	cat(num+99, "\n", file=temp)

    for(i in 1:(num-1)) {
        # Retrieves individual fleet from the FLIndices list
    	FLIndex. <- FLIndices.[[i]]
        nages <- FLIndex.@range[2] - FLIndex.@range[1] + 1
        nyrs  <- FLIndex.@range[5] - FLIndex.@range[4] + 1

        # creates empty matrix for the catch and effort data
        catch  <- matrix(rep(0, (nyrs*nages)+nyrs), nrow=nyrs, ncol=nages+1)

        # Retrieves effort data from each FLIndex object
        effort <- matrix(FLIndex.@effort)

        # Retrieves index data from each FLIndex object
        index  <- t(matrix(FLIndex.@index, nrow=nages, ncol=nyrs))

        # converts index data into catch data and adds to the empty matrix
        for (j in 1:nages) {
            catch[,j+1]  <- index[,j]*effort[j,]
        }

        # appends the eff data onto the front of the matrix which now contains catch data
        catch[,1] <- effort

        # Writes relevent info for each individual fleet to the file 
        cat(FLIndex.@name, "\n", file=temp)
        cat(FLIndex.@range[4], FLIndex.@range[5], "\n", file=temp, sep="\t")
        cat(1, 1, FLIndex.@range[6], FLIndex.@range[7], "\n", file=temp, sep="\t")
        cat(FLIndex.@range[1], FLIndex.@range[2], "\n", file=temp, sep="\t")

        # Appends the data for each individual fleet to the file
        write(t(catch), file=temp, ncolumns=nages+1)
    }

    close(temp)

    # Code for producing SSB file
    # Retrieves the SSB FLIndex, (this assumes the SSB file is always last!!!!
    last	<- FLIndices.[[num]]

    # Calculates year information
    minyear <- last@range[4]
    maxyear <- last@range[5]
    nyrs  <- maxyear - minyear + 1

    # Produces empty matrix for storing data
    final <- matrix(rep(0, nyrs*3), nrow=nyrs, ncol=3)

    #Retrieves information to add to matrix
    years <- matrix(minyear:maxyear)
    effort <- matrix(last@effort)
    index  <- matrix(last@index)

    # Adds info to matrix
    final[,1] <- years
    final[,2] <- effort
    final[,3] <- index

    # Creates the SSB file
    ssbtemp <- file(ssb., "w")
    on.exit(close(ssbtemp))
    cat(FLIndices.[[num]]@name, "\n", file=ssbtemp)

    # not sure what the 1 and 2 stand for yet!!!!!!!
    cat(1, nyrs, 2, "\n", sep="\t", file=ssbtemp)

    # except year, not sure if these titles are standard titles
    cat("YEAR", "VPA", "MLAI", "\n", sep="\t", file=ssbtemp)
    write(t(final), file=ssbtemp, ncolumns=3)
}	# }}}

## readIndicesVPA		{{{
readIndicesVPA <- function(file., sep="", quiet=TRUE, cchar='#', na.strings=na.strings) {

    # calculates number of fleets contained in Index file
    num	<- (scan(file., skip=1, nlines=1, sep=sep, quiet=quiet, comment.char=cchar))-100
    #description of the group of fleets
    desc <- paste(scan(file., nlines=1, what="character", sep="\n", quiet=quiet, comment.char=cchar), ". Imported from VPA file.")

    # produces starting values for v w x y z
    v <- -2
    w <- -1
    x <- 0
    y <- 1
    z <- 2
    nyrs <- 0

    # creates empty FLIndices object
    FLIndices. <- FLIndices()		

    for(i in 1:num) {

        # produces the values for v w x y z relevant to each fleet in Index file
        v <- v + 4+nyrs
        w <- w + 4+nyrs
        x <- x + 4+nyrs
        y <- y + 4+nyrs
        z <- z + 4+nyrs

        # calculates the year range
        yrs	<- scan(file., skip=w, nlines=1, sep=sep, quiet=quiet, comment.char=cchar)
        nyrs  <- yrs[2]-yrs[1]+1 

        # calculates the age range
        ages  <- scan(file., skip=y, nlines=1, sep=sep, quiet=quiet, comment.char=cchar)
        nages <- ages[2]-ages[1]+1

        # calculates the values for alpha and beta
        AB	<- scan(file., skip=x, nlines=1, sep=sep, quiet=quiet,comment.char=cchar, na.strings=na.strings)

        alpha <- AB[3]
        beta  <- AB[4]
    
        # retrieve information for the effort slot
        eff  <-  read.table(file=file., skip=z, nrows=nyrs ,sep=sep, comment.char=cchar, na.strings=na.strings)[1:nyrs,1]

    	# retrieves catch numbers at age
        catch <- read.table(file=file., skip=z, nrows=nyrs,sep=sep, comment.char=cchar, na.strings=na.strings)[1:nyrs,2:(nages+1)]

      # calculates values for the index slot
      index <- catch/eff

    	# Creates index slot
      index <- matrix(t(index),nrow=nages, ncol=nyrs,
        dimnames=list(ages[1]:ages[2],yrs[1]:yrs[2]))
    	# retreives the names for each fleet
      name <- scan(file., skip=v, nlines=1, what="character", quiet=quiet, comment.char=cchar, sep="#", strip.white=TRUE)
    
        # Produces a FLIndex object for each fleet
        effort <- FLQuant(eff, dimnames=list(age = "all",
                               year=as.character(yrs[1]:yrs[2]),
                               unit="unique",
                               season ="all",
                               area = "unique"))
    		catch <- FLQuant(t(unname(as.matrix(catch))), dimnames=list(age = as.character(ages[1]:ages[2]),
                                     year=as.character(yrs[1]:yrs[2]),
                                     unit="unique",
                                     season ="all",
                                     area = "unique"))
        index <- FLQuant(array(index, dim=c(nages, nyrs, 1,1,1),
                       dimnames=list(quant =as.character(ages[1]:ages[2]),
                                     year=as.character(yrs[1]:yrs[2]),
                                     unit="unique",
                                     season="all",
                                     area="unique")), quant="age")
#        temp@catch.wt <- FLQuant(temp@index)
#        temp@q <- FLQuant(temp@index)
#        temp@index.var <- FLQuant(temp@index)
#        temp@sel.pattern  <- FLQuant(temp@index)
        temp <- FLIndex(index=index)
        temp@catch.n <- catch
        temp@effort <- effort
		temp@name	<- paste(name, collapse=" ")
        temp@desc	<- desc
        temp@range  <- c(ages[1], ages[2], ages[2], yrs[1], yrs[2], alpha, beta)
        names(temp@range)<-c("min","max","plusgroup","minyear","maxyear","startf","endf")

        # Creates the FLIndices list of fleets
        FLIndices.[[name]] <- temp
    }
        FLIndices.@desc <- desc
        return(FLIndices.)
}	# }}}

## readIndicesAdapt		{{{
readIndicesAdapt <- function(file.,na.strings="NA") {
    skip.hash<-function(i) {
        i<-i+1
        while (substr(scan(file.,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
            i<-i+1
        return(i)
    }
    skip.until.minus.1<-function(i) {
        i<-i+1
        while (scan(file.,skip=i,nlines=1,what=("character"),quiet=TRUE)[1]!="-1")
            i<-i+1
        return(i)
    }

    # range
    range<-numeric(5)
	names(range)<-c("min","max","plusgroup","minyear","maxyear")
	i <- skip.hash(0)
    range[c("minyear", "maxyear")] <- scan(file.,skip = i, nlines = 1, nmax = 2, quiet = TRUE,na.strings=na.strings)
    i <- skip.hash(i)
    range[c("min", "max", "plusgroup")] <- scan(file.,skip = i, nlines = 1, nmax = 3, quiet = TRUE,na.strings=na.strings)
    i <- skip.hash(i+1)
    NIndex <- scan(file.,skip=i,nlines=1,nmax=1,quiet=TRUE)
    i <- skip.until.minus.1(i)
    i <- skip.hash(i)
    smry. <- array(0,dim=c(NIndex,7))
      
    j <- numeric(1)
    for (i in i:(i+NIndex-1)) {
        j<-j+1
        smry.[j,]<-scan(file.,skip=i,nlines=1,nmax=7,quiet=TRUE,na.strings=na.strings)
    }

    i<-skip.hash(i)
    i<-skip.hash(i)
    index.<-array(0,dim=c(skip.until.minus.1(i)-i, 4))
    for (j in i:(skip.until.minus.1(i)-1))
        index.[j-i+1,]<-scan(file.,skip=j,nlines=1,nmax=4,quiet=T,na.strings=na.strings)
    i<-skip.until.minus.1(i)+1	   	
    p.<-read.table(file.,skip=i,fill=T,nrows=(skip.until.minus.1(i)-i-3),colClasses="numeric",na.strings=na.strings)
	  l. <- FLIndices()
    for (i in 1:NIndex)
        l.[[i]]<-FLIndex(iniFLQuant=FLQuant(NA,dimnames=list(age=range["min"]:range["max"],year=range["minyear"]:range["maxyear"])))
    return(set.index(smry.,index.,p.,l.,range))
}

## readIndicesCSA
readIndicesCSA <- function(file.,na.strings="NA") {
    t.    <- scan(file=file.,skip=1,sep=",",na.strings=na.strings)
    nrow. <- length(t.)/9
    t.    <- t(array(t.,dim=c(9,nrow.)))
    t.    <- array(t.[,-1],dim=c(20,8),
        dimnames=list(t.[,1],c("m","c.rec","c.full","w.rec","w.full","s.rat","u.rec","u.full")))

    c. <- FLIndex(
        index=as.FLQuant(t(array(cbind(t.[,"u.rec"],t.[,"u.full"]),
            dim=c(nrow.,2),
            dimnames=list(dimnames(t.)[[1]],c("r","full"))))),
	    effort=as.FLQuant(t(array(1, dim=c(nrow.,1),
            dimnames=list(dimnames(t.)[[1]],"all"     )))),
	    w=as.FLQuant(t(array(1, dim=c(nrow.,1),
            dimnames=list(dimnames(t.)[[1]],"all"     )))),
        p=as.FLQuant(t(array(t.[,"s.rat"], dim=c(nrow.,1),
            dimnames=list(dimnames(t.)[[1]],"all"     )))))
    c.@range["minyear"]     <-min(t.[,1])   
    c.@range["maxyear"]     <-max(t.[,1])   
    c.@range["startf"]<-0.0
    c.@range["endf"]  <-0.0
    c. <- FLIndices(c.)
    c.@desc <- paste("read in from CSA file", file.)
return(c.)
}	# }}}

## readIndicesICA		{{{
readIndicesICA <- function(file, file2, sep="", na.strings=na.strings) {
  if (file=="" & file2!="")       return(readIndicesICA.ssb(file.=file2,sep=sep))
  else if (file!="" & file2=="")  return(readIndicesVPA(file.=file,na.strings=na.strings))
  else                            return(FLIndices(c(readIndicesVPA(file.=file,na.strings=na.strings), readIndicesICA.ssb(file.=file2,sep=sep,na.strings=na.strings))))
  }
  
readIndicesICA.ssb <- function(file., sep="",na.strings=na.strings) {
      
      title<-scan(file=file.,skip=0,quiet=TRUE,nlines=1,what=character())
      stuff<-scan(file=file.,skip=1,quiet=TRUE,nlines=1)
      names<-scan(file=file.,skip=2,quiet=TRUE,nlines=1,what=character())[-c(1,stuff[3])]
      data <-scan(file=file.,skip=3,quiet=TRUE,na.strings=na.strings)
      data <-t(matrix(data,c(length(data)/stuff[2],stuff[2])))[,-stuff[3]]
       
      indices<-new("FLIndices")
      
      for (i in 1:stuff[1])
        {
        indices[[i]]<-FLIndex(index=as.FLQuant(data[,i+1],dimnames=list(age="all",year=data[,1])),name=names[i],desc=title)
        indices[[i]]@type<-"biomass"
        }
        
      return(indices)  
}	# }}}

## checkIndex	{{{
# TODO 26/11/2004 iagoazti: Check whether all this is needed
checkIndex <- function(Index, name=NULL, desc=NULL) {
    if (!inherits(Index, "FLIndex"))
        stop("Individual items returned should be 'FLIndex' objects!")
    if (!is.null(name))
        Index@name <- as.character(name)
    if (!is.null(desc))
        Index@desc <- as.character(desc)
    if (names(dimnames(Index@index))[[1]] == "all") {
        dnms    <-dimnames(Index@index)
        dnms$age<-as.character(Index@range["min"]:Index@range["maxage"])
        dm      <- dim(Index@index)
        dm[1]   <- length(dnms)
        ByAge   <- FLQuant(array(NA,dm,dnms))
        SumAge <- FLQuant(array(NA,dim=dim(Index@index),dimnames(Index@index)))
    } else {
        ByAge <- FLQuant(array(NA,dim=dim(Index@index),dimnames(Index@index)))
        dnms <- dimnames(Index@index)
        dnms$age <- as.character("all")
        dm <- dim(Index@index)
        dm[1] <- 1
        SumAge <- FLQuant(array(NA,dm,dnms))
    }

    # p should have the same dimensions as index plus the same ages as in range
    if (is.null(Index@p) || !all(dimnames(Index@p)[[2]] == dnms[[2]])
        || !all(dimnames(Index@p)[[3]] == dnms[[3]])
        || !all(dimnames(Index@p)[[4]] == dnms[[4]])
        || !all(dimnames(Index@p)[[5]] == dnms[[5]]))
            Index@p <- ByAge

    # wt should have the same dimensions as index
    # if (is.null(Index@w))
    {
        Index@w       <- Index@index
        Index@w[,,,,] <- NA
    }

    # effort should have same dimensions as index	
    if (is.null(Index@effort)  || !all(dim(Index@effort) == dim(SumAge))) 
    	Index@effort <- SumAge

    # Verify range and set names to it
    rng <- as.vector(Index@range)
    if (length(rng) != 7)
        stop("range slot must have seven items!")
    names(rng) <- c("min", "maxage", "plusgroup", "minyear",
        "maxyear", "startf", "endf")
    return(Index)
}	# }}}

## readFLIndices	{{{
readFLIndices <- function(file, file2, type="VPA", index.names, descs,
    desc = paste("Imported from ", type, " file '", file, "'", sep = ""),na.strings="NA", sep="") {

    # TODO 26/11/2004 iagoazti: pass an open connection, but read.table!
    # open connection that closes on exit
#    con <- file(file, "r")
#    on.exit(close(con))

	# Core of the function...
    res <- switch(type[1],
        "VPA"   = readIndicesVPA(file,na.strings=na.strings, sep=sep),
        "Adapt" = readIndicesAdapt(file,na.strings=na.strings),
        "CSA"   = readIndicesCSA(file,na.strings=na.strings),
	"ICA"   = readIndicesICA(file, file2,na.strings=na.strings),
        stop("type must be 'VPA', 'Adapt', 'ICA' or 'CSA'!"))

    # desc
    if (length(desc)==0 && length(res@desc)==0)
        desc=paste("Imported from ", type, " file '", file, "'", sep="")   
    if (!missing(index.names))
        if (length(index.names)!=length(res) || !is.character(index.names))
            stop("index.names must be a character vector of the same length as the number of Indexs")
    if (!missing(descs))
        if (length(descs)!=length(res) || !is.character(descs))
            stop("descs must be a character vector of the same length as the number of Indexs")

    for (i in 1:length(res)) {
        # index.names
        if (!missing(index.names))
            res[[i]]@name <- index.names[i]
        # descs
        if (!missing(descs))
            res[[i]]@desc <- descs[i]
    }
	if(!validObject(res))
		stop("Error: object is not valid")
    return(res)
}	# }}}

## readFLIndex		{{{
readFLIndex <- function(file, type="VPA", index.names, descs, 
    desc=paste("Imported from ", type, " file '", file, "'", sep="")) {

	res <-  readFLIndices(file, type=type, index.names=index.names, desc=desc)
    if (length(res) > 1)
        warning("The ", type, " file ", file, " contains more than 1 index. Returning the first")
    return(res[[1]])
}

set.index <- function(smry.,index.,p.,l.,range) {
    yr.range  <- tapply(index.[,2],index.[,1],range)
	for (i in 1:length(l.)) {
    	    l.[[i]]@range[1:2]<-smry.[i,6:7]
    	    l.[[i]]@range[4:5]<-yr.range[[i]]

    	    # TIMING (-1 = AVERAGE DURING YEAR, POSITIVE INTEGER = NUMBER OF MONTHS ELAPSED)
    	    if (smry.[i,5]==-1) 
    	        l.[[i]]@range[6:7]<-c(-1,-1)
    	    else   
    	        l.[[i]]@range[6:7]<-c(smry.[i,5]/12,smry.[i,5]/12)
    	    names(l.[[i]]@range)[6:7]<-c("startf","endf")

    	    # PDF         (0= do not use,1=lognormal, 2=normal)
    	    if (smry.[i,2]==2)
    	        l.[[i]]@distribution<-"normal"
    	    else 
    	        l.[[i]]@distribution<-"lognormal"

    	    # UNITS       (1 = numbers, 2 = biomass)
    	    if (smry.[i,3]==2) 
    	        l.[[i]]@type<-"biomass" 
    	    else 
    	        l.[[i]]@type<-"numbers"

    	    # SELECTIVITY (1 = fixed, 2 = fractional catches, 
    	    # 3 = Powers and Restrepo partial catches,4=Butterworth and Geromont eq 4)
            if (smry.[i,4]==1) 
                l.[[i]]@type<-c(l.[[i]]@type,"sel") 
            else if (smry.[i,4]==2) 
                l.[[i]]@type<-c(l.[[i]]@type,"catches")
            else if (smry.[i,4]==3) 
                l.[[i]]@type<-c(l.[[i]]@type,"Powers")
            else if (smry.[i,4]==4) 
                l.[[i]]@type<-c(l.[[i]]@type,"Butterworth")  
            names(l.[[i]]@type)<-c("type")

            l.[[i]]@index <- FLQuant(array(index.[index.[,1]==i,3],
                dim=c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                dimnames=list(age="all",year=(index.[index.[,1]==i,2]))))

			l.[[i]]@catch.wt <- FLQuant(l.[[i]]@index)
			l.[[i]]@catch.n  <- FLQuant(l.[[i]]@index)
			l.[[i]]@sel.pattern      <- FLQuant(l.[[i]]@index)

            l.[[i]]@effort <- FLQuant(array(1,
                dim=c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                dimnames=list(age="all",year=index.[index.[,1]==i,2])))

            l.[[i]]@index.var <- FLQuant(array(ifelse(index.[index.[,1]==i,4]<0,
                -1,index.[index.[,1]==i,4]), dim=c(1,yr.range[[i]][2]-yr.range[[i]][1]+1),
                dimnames=list(age="all",year=index.[index.[,1]==i,2])))

            if (any(p.[,1]==i)){
				l.[[i]]@sel.pattern <-FLQuant(array(t(as.matrix(p.[p.[,1]==i,3:length(p.[1,])])),
								  dim=c(length(p.[1,])-2,yr.range[[i]][2]-yr.range[[i]][1]+1),
                                  dimnames=list(as.character(range["min"]:range["max"]),year=index.[index.[,1]==i,2])))
				l.[[i]]@sel.pattern <-l.[[i]]@sel.pattern[as.character(smry.[i,6]:smry.[i,7]),,,,]
				}
			}
    return(l.)
}	# }}}

# read.FLIndex
read.FLIndex <- function(...)
{
  warning("read.FLIndex has been renamed as readFLIndex and will de deprecated", inmediate. = TRUE)
  readFLIndex(...)
}

# read.FLIndices
read.FLIndices <- function(...)
{
  warning("read.FLIndices has been renamed as readFLIndices and will de deprecated", inmediate. = TRUE)
  readFLIndices(...)
}
