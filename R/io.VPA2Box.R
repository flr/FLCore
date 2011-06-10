# io.VPA2Box - «Short one line description»
# io.VPA2Box

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC & Laurie Kell, ICCAT
# $Id:  $

# readVPA2Box {{{
readVPA2Box <- function(file, args=missing, ...) {

  if(!missing(args))
    args <- c(args, list(...))

  # control file 
  dir  <- getDir(file)
  files <- paste(dir, .Platform$file.sep, vpa2BoxFiles(file), sep="")
  
  nS  <- getNBootRetro(file)
  nits <- max(1, nS[2])
  nRet <- max(1, nS[1])

  # "csv" file
  # data
  dat <- scan(files[5], what="", sep="\n", strip.white=TRUE )
  dat <- dat[nchar(dat)>0]

  # gets line number for start of data  
  ln <-c(F=grep("F",dat)[1],
         N=grep("N",dat)[1],
         C=grep("C",dat)[1],
         W=grep("W",dat)[1],
         I=grep("I",dat)[2])
  
  # function to convert data in "csv" file into an FLQuant
  aaIn <- function(aa,minage=1) {
    aa <- aa[nchar(aa)>1]
    N <- length(aa)
    aa <- unlist(strsplit(aa," +"))
    aa <- aa[nchar(aa)>0]

    dms <- c(length(aa)/N,N)

    aa <- array(as.numeric(aa),dim=dms)
    
    return(FLQuant(c(aa[-1,]), dimnames=list(age=minage+(0:(dms[1]-2)),
      year=aa[1,])))
  }

  stk <- FLStock(stock.n=aaIn(dat[(ln[2]+1):(ln[3]-1)]))

  harvest <- aaIn(dat[(ln[1]+1):(ln[2]-1)])
  landings.n <- aaIn(dat[(ln[3]+1):(ln[4]-1)])
  stock.wt <- aaIn(dat[(ln[4]+1):(ln[5]-1)])

  harvest(stk) <- harvest
  landings.n(stk) <- landings.n
  stock.wt(stk) <- stock.wt
  landings.wt(stk) <- stock.wt
  discards.wt(stk) <- 0
  
  # data file
  # year range
  i <-0
  i <- skip.hash(i,files[1])
  yrRng <- read.table(files[1], skip=i, nrows=1, sep="\n",
    colClasses="character", strip.white=TRUE)[[1,1]]
  yrRng <- gsub("\t"," ",yrRng)
  yrRng <- as.integer(strsplit(yrRng," +")[[1]][1:2])

  ## age range
  i <- skip.hash(i, files[1])
  ageRng <- read.table(files[1], skip=i, nrows=1, sep="\n",
    colClasses="character", strip.white=TRUE)[[1,1]]
  ageRng <- gsub("\t"," ",ageRng)
  ageRng <- as.integer(strsplit(ageRng," \t+")[[1]][1:4])

  ## number of indices
  i <- skip.hash(i, files[1])
  read.table(files[1], skip=i,nrows=1, sep="\n")

  ## xxx.spwn
  i <- skip.hash(i, files[1])
  x.spwn <- read.table(files[1], skip=i, nrows=1, sep="\n", colClasses="character",
    strip.white=TRUE)[[1,1]]
  x.spwn <- gsub("\t"," ",x.spwn)
  x.spwn <- as.integer(strsplit(x.spwn," +")[[1]][1])
  x.spwn <- (x.spwn)/12
  m.spwn(stk) <- x.spwn
  harvest.spwn(stk) <- m.spwn(stk)

  ## mat
  i <- skip.hash(i,files[1])
  mat <- read.table(files[1],skip=i,nrows=1,sep="\n",colClasses="character",strip.white=TRUE)[[1,1]]
  mat <- gsub("\t"," ",mat)
  mat <- as.integer(strsplit(mat," +")[[1]])
  
  mat(stk)[] <- mat[1:dim(mat(stk))[1]]

  # Binary files
  dmns <- dimnames(stock.n(stk))
  dmns$iter <- 1:nits

  if (file.exists(paste(dir,"MAA.OUT",sep="/")))
    m(stk) <- readBinary(paste(dir,"MAA.OUT",sep="/"), dmns)
  if (file.exists(paste(dir,"FAA.OUT",sep="/")))
    harvest(stk) <- readBinary(paste(dir,"FAA.OUT",sep="/"), dmns)
  if (file.exists(paste(dir,"NAA.OUT",sep="/")))
    stk@stock.n <- readBinary(paste(dir,"NAA.OUT",sep="/"), dmns)
  if (file.exists(paste(dir,"CAA.OUT",sep="/")))
    catch.n(stk) <- readBinary(paste(dir,"CAA.OUT",sep="/"), dmns)
  else
    catch.n(stk) <- stock.n(stk)*harvest(stk)/(harvest(stk)+
      m(stk))*(1-exp(-((harvest(stk)+m(stk)))))

  catch.n(stk) <- landings.n(stk)
  discards.n(stk) <- 0

  if (file.exists(paste(file,"WAA.OUT",sep="/"))) {
    stock.wt(   stk) <- readBinary(paste(file,"WAA.OUT",sep="/"),dimnames(stock.n(stk)))
    catch.wt(   stk) <- stock.wt(stk)
    landings.wt(stk) <- stock.wt(stk)
    discards.wt(stk) <- stock.wt(stk)
  }

  # replace any slots
  slt <- names(getSlots("FLStock"))[getSlots("FLStock")=="FLQuant"]
  for(i in names(args)[names(args) %in% slt]) {
    if (args[[1]])
      if (all(c("numeric","vector") %in% is(args[[i]])))
        args[[i]] <- FLQuant(args[[i]],dimnames=dimnames(m(stk)))
        slot(stk, i) <- args[[i]]
  }

  catch(stk)   <- computeCatch(stk,"all")
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)

  units(harvest(stk)) <- "f"

  if (nRet > 1)
    stk <- getRetros(stk,files[3],n=nRet)

  return(stk)
} # }}}

# getDir {{{
getDir <- function(file) {
  if (!grepl(.Platform$file.sep,file))
    res <- getwd()
  else
    res <- substr(file,1,max(gregexpr(.Platform$file.sep,file)[[1]])-1)
  return(res)
} # }}}

# vpa2boxfiles {{{
vpa2BoxFiles <- function(file) {
  i <- skip.hash(0,file)
  j <- skip.until.hash(i,file)

  res <- gsub(" ","",gsub("'","",substr(scan(file,skip=i+1,nlines=j-i-1,
    quiet=TRUE,what=character(),sep="\n"),1,20)))
   
  print(res)
  
  return(res)
} # }}}

# skip.hash {{{
skip.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)=="#")
        i <- i+1

    return(i)}

skip.until.hash <- function(i,file) {
    i <- i+1
    while (substr(scan(file,skip=i,nlines=1,what=("character"),quiet=TRUE)[1],1,1)!="#")
        i <- i+1

    return(i)
} # }}}

# getNBootRetro {{{
getNBootRetro <- function(file) {

    tmp <- scan(file,what=character(),sep="\n")
    tmp <- unlist(lapply(strsplit(tmp[substr(tmp,1,1)!="#"]," +"),
      function(x) x[x!=""][1]))

    as.numeric(tmp[length(tmp)-1:2])
} # }}}

# readBinary {{{
readBinary <- function(x,dmns=list(),size=4) {
  # Specify dims
  if ( "iter" %in% names(dmns))
    dmns <- list(year=dmns$year,age=dmns$age,unit="unique",season="all",
      area="unique",iter=dmns$iter)
  else
    dmns <- list(year=dmns$year,age=dmns$age,unit="unique",season="all",
      area="unique",iter=1)

  # Get binary data
  res <- readBin(x, what=double(), size=size, prod(unlist(lapply(dmns,length))))

  ## create array and swap year & age
  res <- array(res,lapply(dmns,length),dmns)
  res <- FLQuant(aperm(res,c(2,1,3,4,5,6)))

  return(res)
} # }}}
