# FLtest - Check tools

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Author: Ernesto Jardim, IPIMAR
# $Id$

# Reference:
# Notes:

# TODO Sat 08 Sep 2007 08:24:25 PM CEST iagoazti:

startTest <- function(file="testReport.txt"){
	file(file, open="w")
}

finishTest <- function(con=getOption("con")){
	if(is.character(con)) con <- get(con)
	close(con)
}

tagTest <- function(tag="My tag is better than yours !", con=getOption("con")){
	if(is.character(con)) con <- get(con)
	cat(tag, "\n", file=con)
	cat(date(), "\n", file=con)
	cat("========================\n\n", file=con)
}

setCon <- function(con="zz"){
	options(con=con)	
}

checkIdentical <- function(x, y, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkIdentical: ", identical(x,y), "; call: ", CALL, "\n", file=con)
}

checkEqual <- function(x, y, con=getOption("con"),
  tolerance = .Machine$double.eps ^ 0.5, ...){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkEqual: ", all.equal(x,y, tolerance=tolerance, ...), "; call: ", CALL,
    "\n", file=con)
}

checkTrue <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkTrue: ", isTRUE(x), "; call: ", CALL, "\n", file=con)
}

checkFalse <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	cat("+ checkTrue: ", !isTRUE(x), "; call: ", CALL, "\n", file=con)
}

checkFail <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	te <- try(x, TRUE)
	cat("+ checkFail: ", identical(is(te),"try-error"), "; call: ", CALL, "\n", file=con)
}

checkRun <- function(x, con=getOption("con")){
	CALL <- deparse(match.call())
	if(is.character(con)) con <- get(con)
	te <- try(x, TRUE)
	cat("+ checkRun: ", !identical(is(te),"try-error"), "; call: ", CALL, "\n", file=con)
}
