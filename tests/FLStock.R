# FLStock - «Short one line description»
# FLStock

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 17 Feb 2009 19:06
# $Id:  $

# Reference:
# Notes:

# TODO Tue 17 Feb 2009 06:56:10 PM CET IM:

library(FLCore)

# start test
setCon()
zz <- startTest("FLStock.txt")
tagTest("FLStock testing ...")


# Bug IM_20090217_1855_rec
# http://flr-project.org/bugs/index.php?do=details&task_id=6&project=2
#   rec(FLStock) should return an FLQuant with rec.age instead of 'all'
checkIdentical(dimnames(rec(ple4))$age, as.character(dims(ple4)$min))


finishTest()
