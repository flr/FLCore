# io.VPA2Box - «Short one line description»
# io.VPA2Box

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Thu 01 Apr 2010 11:08:00 AM CEST IM:

# readVPA2Box {{{
readVPA2Box <- function(data, results, m=as.numeric(NA), no.discards=TRUE)
{
  # read input data in Adapt format
  res <- readFLStock(data, type='Adapt', m=m)
  
  # read N and F from summary output file
  nf <- getNF(results)
  stock.n(res)[,dimnames(nf[["stock.n"]])$year]<-nf[["stock.n"]]
  harvest(res)[,dimnames(nf[["harvest"]])$year]<-nf[["harvest"]]
  units(harvest(res))<-units(nf[["harvest"]])

  # M
  if(is.character(m))
    m(res) <- getM(m, ages=dim(m(res))[1])
  else if (is(m, 'FLQuant'))
    m(res) <- m
  else if(is.vector(m))
    if(length(m) == 1)
      m(res) <- FLQuant(m, dimnames=dimnames(stock.n(res)))
    else
      m(res) <- FLQuant(matrix(m, ncol=1, nrow=length(m)),
        dimnames=dimnames(stock.n(res)))

  # discards
  if(no.discards)
  {
    discards.n(res) <- 0
    discards.wt(res) <- landings.wt(res)
    catch(res) <- computeCatch(res, 'all')
  }

  # desc
  desc(res) <- paste('Imported from a set of VPA2Box files: [', data, ',', results,
      ']', sep='')
  return(res)
} # }}}

# getNF {{{
getNF <- function(filename)
{
  posFile <- function(i,filename,char="-")
  {
    while (TRUE)
    {
      firstChar <- substr(scan(filename, skip = i, nlines = 1,
        what = ("character"), quiet = TRUE)[1], 1, 1)
      if (!is.na(firstChar))
        if (firstChar == char) break
      i<-i+1
    }

    return(i)
  }

  getFLQ <- function(filename,pos1, pos2)
  {
    nyrs <- pos2-pos1-1
    t. <- scan(filename, skip = pos1+1, nlines=nyrs, quiet = TRUE)
    nages <- length(t.)/nyrs
    t. <- array(t.,c(nages,nyrs))

    yrs <- array(t.,c(nages,nyrs))[1,]
    ages <- scan(filename, skip = pos1-1, nlines=1, quiet = TRUE)

    flq <- FLQuant(t.[-1,],dimnames=list(age=ages,year=yrs))

    return(flq)
  }

  i <- 0
  pos1 <- posFile(i,filename)
  pos2 <- posFile(pos1,filename,char="=")
  harvest <- getFLQ(filename,pos1, pos2)
  units(harvest) <- "f"

  pos1 <- posFile(pos2,filename)
  pos2 <- posFile(pos1,filename,char="=")
  stock.n <- getFLQ(filename,pos1, pos2-1)

  return(FLQuants(stock.n=stock.n,harvest=harvest))
} # }}}

# getM {{{
getM <- function(filename, ages)
{
  i <- 0
  while(TRUE)
  {
    firstChar <- scan(filename, skip = i, nlines = 1,
        what = ("character"), quiet = TRUE)
      if (!is.na(firstChar)[1])
        if ('natural' %in% firstChar)
          break
      i<-i+1
   }

   mat <- matrix(scan(filename, skip = i+1, nlines=ages, what='character', quiet = TRUE),
     nrow=ages, byrow=TRUE)[,2]
   return(as.double(sub('D', 'E', mat)))
 }  # }}}
