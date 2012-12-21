# io.MFCL - Loading Multifan-CL results into FLR
# io.MFCL

# Copyright 2010 FLR Team <flr-team@flr-project.org>
# Distributed under the GPL 2 or later
# $Id:  $

# Code inside these methods was taken from the getmfclstuff package 2.0.0
# by Pierre Kleiber. Licensed under MIT license
# http://code.google.com/p/r4mfcl/

setGeneric("readMFCL",     function(file,...)      standardGeneric("readMFCL"))

setMethod( "readMFCL", signature(file="character"),
  function(file, ...) {
    
  for (i in file) {
     if (getExt(i)=="rep") repfile<-i
     if (getExt(i)=="par") parfile<-i}

  nreg <- getnreg(repfile)
  if (nreg>1)
     nreg=1:nreg else nreg="unique"

  # seasons
  if (getnpd(repfile) != getnyr(repfile)) 
    seasons <- 1:(getnpd(repfile)/getnyr(repfile)) else seasons <- "all"

  minage <- 1
  minyr <- floor(min(unlist(getrtimes(repfile))))

  # dimnames
  dmns <- list(age=minage:getnages(repfile)+minage-1,
      year=minyr:(minyr+getnyr(repfile)-1), unit="unique", season=seasons, area=nreg)

  # output stock
  stk <- FLStock(
    catch       = FLQuant(NA, dimnames=dmns[-1], quant='age'),
    catch.n     = FLQuant(NA, dimnames=dmns),
    catch.wt    = FLQuant(getwt.age(repfile), dimnames=dmns),
    discards    = FLQuant(0, dimnames=dmns[-1], quant='age'),
    discards.n  = FLQuant(0, dimnames=dmns),
    discards.wt = FLQuant(getwt.age(repfile), dimnames=dmns),
    landings    = FLQuant(NA, dimnames=dmns[-1], quant='age'),
    landings.n  = FLQuant(NA, dimnames=dmns),
    landings.wt = FLQuant(getwt.age(repfile), dimnames=dmns),
    stock       = FLQuant(NA, dimnames=dmns[-1], quant='age'),
    stock.wt    = FLQuant(getwt.age(repfile), dimnames=dmns),
    harvest     = FLQuant(NA, dimnames=dmns, units="f"),
    m           = FLQuant(getM.age(   repfile)/length(seasons), dimnames=dmns),
    mat         = FLQuant(getmaturity(parfile), dimnames=dmns),
    harvest.spwn= FLQuant(0, dimnames=dmns),
    m.spwn      = FLQuant(0, dimnames=dmns),
    name        ="",
    desc        ="read in from Multifan-CL")

  # stock.n
  N <- getNyar(repfile, byyear=FALSE)
  N <- array(c(N), c(length(seasons), length(dmns$year), length(dmns$age), length(nreg), 1, 1))
  stock.n(stk) <- FLQuant(aperm(N, c(3,2,5,1,4,6)), dimnames=dmns)

  # harvest (F)
  f <- getFya(repfile)
  f <- array(c(f), c(length(seasons), length(dmns$year), length(dmns$age), length(nreg), 1, 1))
  harvest(stk) <- FLQuant(aperm(f, c(3,2,5,1,4,6)), dimnames=dmns, units="f")

  # catch.n = stock.n * harvest/(harvest + m) * (1- exp(-harvest - m))
  catch.n(stk) <- stock.n(stk)*harvest(stk)/(harvest(stk)+m(stk))*
    (1-exp(-harvest(stk)-m(stk)))
  # landings.n = catch.n
  landings.n(stk)<- catch.n(stk)
  catch(stk)     <- computeCatch(stk, "all")
  landings(stk)  <- computeLandings(stk)
  discards(stk)  <- computeDiscards(stk)
  stock(stk)     <- computeStock(stk)

  return(stk)})

# getExt {{{
getExt <- function(file)
  substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)) # }}}

# getmfclstuff functions  {{{
getqedlist<-function(plotrepfile="plot.rep"){
## List of catchability+effort dev. vectors by fishery
  dat <- getplotdat4("# Catch.+effort dev. by realization",plotrepfile)
  nreal <- getnreal(plotrepfile)
  nfish <- getnfish(plotrepfile)
  splitter <- rep(seq(nfish), nreal)

  split(dat, splitter)}
  
getqlist<-function(plotrepfile="plot.rep"){
## List of catchability vectors by fishery
  dat <- getplotdat4("# Catchability by realization",plotrepfile)
  nreal <- getnreal(plotrepfile)
  nfish <- getnfish(plotrepfile)
  splitter <- rep(seq(nfish), nreal)

  split(dat, splitter)}

getColist<-function(plotrepfile="plot.rep"){
## Observed catch by fishery (down) and time (across)
##   Returns list w/ 1 element (vector) per fishery
  dat <- getplotdat4("# Observed catch by fishery",plotrepfile)
  nreal <- getnreal(plotrepfile)
  nfish <- getnfish(plotrepfile)
  splitter <- rep(seq(nfish), nreal)
  split(dat, splitter)}

getselect<-function(plotrepfile="plot.rep"){
## Selectivity by age class (across) and fishery (down)
  dat <- getplotdat4("# Selectivity by age class",plotrepfile)
  nages <- getnages(plotrepfile)
  matrix(dat, byrow=TRUE,ncol=nages)}

getCPUEolist<-function(plotrepfile="plot.rep"){
## Observed CPUE by fishery (down) and time (across)
  dat <- getplotdat4("# Observed CPUE by fishery",plotrepfile)
  nreal <- getnreal(plotrepfile)
  nfish <- getnfish(plotrepfile)
  splitter <- rep(seq(nfish), nreal)

  split(dat, splitter)}

getFya<-function(plotrepfile="plot.rep"){
## Fishing mortality by age class (across) and year (down)
  nages <- getnages(plotrepfile)
  dat <- getplotdat4("# Fishing mortality by .*down)$",plotrepfile)
  matrix(dat, byrow=TRUE,ncol=nages)}

getmaturity<-function (parfile = getoutputparfile("plot.rep")) {
  getplotdat1(parfile,h="# percent maturity")}
  
getM.age<-function(plotrepfile="plot.rep"){
## Natural mortality at age
  getplotdat1(plotrepfile,h="# Natural mortality at age")}

getwt.age<-function(plotrepfile="plot.rep"){
## Mean weights at age
  getplotdat1("# Mean weights at age",plotrepfile)}
  
getnreal<-function(plotrepfile="plot.rep"){
### Number of realizations per fishery
  getplotdat1(plotrepfile,h="# Number of realizations per fishery")}

getnfish<-function(plotrepfile="plot.rep"){
## Number of fisheries
  getplotdat1(plotrepfile,h="# Number of fisheries")}

getplotdat0 <- function(h,plotrepfile) {
  ##=================================================
  ## List remainder of line containing header h. 
  ##=================================================
  dat <- readLines(plotrepfile)
  recnum <- grep(h, dat)
  if(length(recnum) != 1)
     stop(paste("zero or multiple instances of",
                '"',h,'"',"in",plotrepfile," Die yuppie scum!"))
  tt <- (strsplit(dat[recnum], split = " +"))[[1]]
  tt[length(tt)]
}

getplotdat4<-function(h="",plotrepfile) {
## Start listing after header h.  Quit if encounter
##  "^#"
  dat <- readLines(plotrepfile)
  rec1 <- grep(h, dat)
  if(length(rec1) <= 0)
     stop(paste('"',h,'"',"not found in",plotrepfile," Die yuppie scum!"))
  recnum <- rec1+1
  tt <- numeric(0)
  
  for(i in recnum:length(dat)) {
    if (regexpr("^#", dat[i]) != -1) break
    tt <- c(tt, scanText(dat[i], what = 0))}
  
  tt}

getrtimes<-function(plotrepfile="plot.rep"){
## Time of each realization by fishery (down)
  dat <- getplotdat4("# Time of each realization by fishery",plotrepfile)
  nreal <- getnreal(plotrepfile)
  nfish <- getnfish(plotrepfile)
  splitter <- rep(seq(nfish), nreal)
  split(dat, splitter)}
  
getnreg<-function(plotrepfile="plot.rep"){
  ## Number of regions
  getplotdat1(plotrepfile,h="# Number of regions")}

getplotdat1<-function (h = "", plotrepfile, skip = 1) {
    dat <- readLines(plotrepfile)
    recnum <- grep(h, dat)
    scanText(dat[recnum + skip], what = 0)}
    
scanText<-function(string, what = character(0), ...){
## Like scan() but reading from a vector of character strings
    tc <- textConnection(string)
    result <- scan(tc, what = what, quiet = TRUE, ...)
    close(tc)
    return(result)}
    
getnpd<-function(plotrepfile="plot.rep"){
## Number of time periods
  getplotdat1(plotrepfile,h="# Number of time periods")}
  
getnyr<-function (plotrepfile = "plot.rep") {
    1 + diff(floor(range(unlist(getrtimes(plotrepfile)))))}  

getNyar<-function (plotrepfile = "plot.rep", byyear = TRUE) {
### Population number array:  N[time pd, age, region]
  nreg <- getnreg(plotrepfile)
  nages <- getnages(plotrepfile)
  npd <- getnpd(plotrepfile)
  dat <- getplotdat2("# Population [Nn]umber by age", "# Region",plotrepfile)
  
  a <- array(dat, c(nages, npd, nreg))
  if (byyear) a <- a[, seq(1, d <- dim(a)[2], d/getnyr(plotrepfile)), ,
                     drop = FALSE]
  aperm(a, c(2, 1, 3))}

getnages<-function(plotrepfile="plot.rep"){
## Number of age classes
  getplotdat1(plotrepfile,h="# Number of age classes")}
      
getplotdat2 <- function(h,k="",plotrepfile) {
  ## Start listing after header h.  Skip if encounter
  ## header k.  Quit if encounter any other "^#"
  dat <- readLines(plotrepfile)
  rec1 <- grep(h, dat)
  if(length(rec1) <= 0)
     stop(paste('"',h,'"',"not found in",plotrepfile," Die yuppie scum!"))
  recnum <- rec1+1
  tt <- numeric(0)
  for(i in recnum:length(dat)) {
    if(regexpr(k,dat[recnum]) != -1) {recnum <- recnum+1; next}
    if(regexpr("^#",dat[recnum]) != -1) break
    tt <- c(tt,scanText(dat[recnum],what=0))
    recnum <- recnum+1}
  
  tt}

getBHSR<-function(plotrepfile="plot.rep"){
  scanText<-function(string, what = character(0), ...){
    ## Like scan() but reading from a vector of character strings
    tc <- textConnection(string)
    result <- scan(tc, what = what, quiet = TRUE, ...)
    close(tc)
    return(result)}

  ## Beverton-Holt stock-recruitment relationship report
    qq <- if(.Platform$OS.type!="unix") '\" '
          else "' "
    gawk <- paste("gawk ",qq,"(/^# Beverton-Holt stock-recruitment rel/)",
                  "{getline;print $4,$7,$10;",
                  "getline;getline;print;getline;getline;print;exit}",
                   qq,plotrepfile)
    a <- scanpipe(gawk,quiet=TRUE)
    output <- list(alpha=a[1],beta=a[2],slope=a[3])
                   
    a <- a[-(1:3)]
    n <- length(a)
    h <- n/2
    output$spawnB <- a[1:h]
    output$recrut <- a[-(1:h)]
    
    output}
    
scanpipe<-function (cmd, ...) {
##  scan from a connection and then tidy up.
    con <- pipe(cmd)
    out <- scan(con, ...)
    close(con)
    
    out}
    
getoutputparfile <- function(plotrepfile="plot.rep") {
  ##===============================================================
  ## returns string with name (and path) of .par file corresponding
  ## to the plotrepfile.
  ##===============================================================
  name <- getplotdat0("# Output par file = ",plotrepfile)
  file.path(dirname(name),name)
} 
    # }}}
