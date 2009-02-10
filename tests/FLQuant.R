# FLQuant - «Short one line description»
# FLQuant

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

library(FLCore)

# start test
setCon()
zz <- startTest("FLQuant.txt")
tagTest("FLQuant testing ...")

#---  class
# new runs
checkRun(new('FLQuant'))
checkRun(new('FLQuant', array(2, dim=c(2,2,2,2,2,2), dimnames=list(quant=1:2, year=1:2,
  unit=1:2, season=1:2, area=1:2, iter=1:2))))
checkRun(new('FLQuant', array(2, dim=c(2,2,2,2,2,2), dimnames=list(quant=1:2, year=1:2,
  unit=1:2, season=1:2, area=1:2, iter=1:2)), units='kg'))
# output of new() is array, FLArray, FLQuant
checkTrue(is(new('FLQuant'), 'FLQuant'))
checkTrue(is(new('FLQuant'), 'FLArray'))
checkTrue(is(new('FLQuant'), 'array'))
# dim of .Data is right
checkEqual(dim(new('FLQuant', array(2, dim=c(2,2,2,2,2,2), dimnames=list(quant=1:2, 
  year=1:2, unit=1:2, season=1:2, area=1:2, iter=1:2)), units='kg')@.Data), rep(2,6))
# units are right
checkTrue(attr(new('FLQuant', array(2, dim=c(2,2,2,2,2,2), dimnames=list(quant=1:2, 
  year=1:2, unit=1:2, season=1:2, area=1:2, iter=1:2)), units='kg'), 'units') == 'kg')

#--- FLQuant(missing)
checkRun(FLQuant())
checkIdentical(FLQuant(), new('FLQuant'))

#--- FLQuant(vector)
checkRun(FLQuant(1:10))
checkEqual(FLQuant(1:10)@.Data, array(1:10, dim=c(1,10,1,1,1,1),
  dimnames=list(quant='all', year=1:10, unit='unique', season='all', area='unique',
  iter=1)))

# Bug IM_20081008_1130_arith
#     Arith operations for FLQuant objects with different iter failed to return iter n
fa <- FLQuant(rnorm(200), dim=c(10,20))
fb <- propagate(fa, 10)
all.equal(fa*fb, fb*fa)
all.equal(dim(fa*fb), dim(fb))
all.equal(dimnames(fa*fb), dimnames(fb))
all.equal(c((fa*fb)[1,1]), c(fa[1,1])*c(fb[1,1]))

# Bug IM_20081222_1030_arith
#     Arith operations for FLQuants with different iter carried out in wrong order
fa <- FLQuant(4, dim=c(10,20))
fb <- FLQuant(2, dim=c(10,20,1,1,1,10))
all(fa/fb == 2)
all(fb/fa == 0.5)

finishTest()
