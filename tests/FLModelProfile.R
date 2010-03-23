# FLModelProfile - «Short one line description»
# FLModelProfile

# Copyright 2010 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Tue 02 Mar 2010 10:14:12 AM WET IM:

library(FLCore)

data(nsher)

model(nsher) <- bevholt

nsher <- fmle(nsher)

nprof1 <- profile(nsher)

nprof2 <- profile(nsher, which='a')

nprof3 <- profile(nsher, range=0.5)

nprof4 <- profile(nsher, range=list(a=seq(1, 8000, length=20), b=seq(1, 100, length=10)))

nprof5 <- profile(nsher, range=0.5, maxsteps=20)
