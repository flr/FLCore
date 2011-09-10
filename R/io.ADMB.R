# io.ADMB.R - 
# FLCore/R/io.ADMB.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

readADMB<-function(file){
    ## read in data from ADMB Par or Rep file
    dat  <-scan(file,what="",sep="\n",skip=1)
    ## Get values
    vals <-lapply(strsplit(dat[grep("#",dat,invert=TRUE)]," "), function(x) as.numeric(x[nchar(x)>0]))
    ## name elements
    names(vals)<-lapply(grep("#",dat,value=T),function(x) substr(x,3,nchar(x)-1))
    return(vals)}
  
writeADMB<-function(x,file){
    cat("#", names(x[1]),"\n",file=file,append=FALSE)
    cat(x[[1]],"\n",file=file,append=TRUE)
    for (i in 2:length(x)){
      cat("#", names(x[i]),"\n",file=file,append=TRUE)
      cat(x[[i]],"\n",file=file,append=TRUE)}}
      

# # readADMB {{{
# readADMB <- function(file) {
#   
#   # read in data from ADMB Par or Rep file
#   dat  <-scan(file, what="", sep="\n", skip=1)
#   # Get values
#   vals <- lapply(strsplit(dat[grep("#", dat, invert=TRUE)], " "),
#     function(x) as.numeric(x[nchar(x)>0]))
#   # name elements
#   names(vals) <- lapply(grep("#", dat, value=T),
#     function(x) substr(x,3,nchar(x)))
#   return(vals)
# } # }}}
#   
# # writeADMB {{{
# writeADMB <- function(x,file) {
#   cat("#", names(x[1]), "\n",file=file, append=FALSE)
#   cat(x[[1]], "\n", file=file, append=TRUE)
#   
#   for (i in 2:length(x)) {
#     cat("#", names(x[i]), "\n", file=file, append=TRUE)
#     cat(x[[i]], "\n", file=file, append=TRUE)}
# } # }}}
