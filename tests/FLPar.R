# FLPar - Tests for the FLPar class and associated methods
# FLPar

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 30 Dec 2008 11:04
# $Id$

# Reference:
# Notes:

library(FLCore)

setCon()
zz <- startTest("FLPar.txt")
tagTest("FLQuants testing ...")

# Class
checkTrue(is(new('FLPar'), 'FLPar'))
checkTrue(is(new('FLPar'), 'array'))

# Validity

# Creator
# array
flp <- FLPar(array(rnorm(100), dim=c(10,10)))
# dim
checkEqual(dim(flp), c(10,10))
# dimnames
checkEqual(dimnames(flp), list(params=letters[1:10], iter=as.character(1:10)))
