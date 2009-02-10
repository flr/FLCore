# FLFleet - demo of FLFleet et al
# FLCore/demo/FLFleet.R

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 26 Jan 2009 15:56
# $Id$

library(FLCore)

# bt4 is an fleet object with a single metier ('TBB') and 2 species ('ple' & 'sol')
data(bt4)

# First, let's inspect the fleet object
summary(bt4)

# and its single metier
summary(metier(bt4, 1))
summary(metier(bt4, 'TBB'))

# and two species
summary(catches(metier(bt4, 'TBB'), 'ple'))
summary(catches(metier(bt4, 'TBB'), 'sol'))

# dims returns a list of maximum dimensions
dims(bt4)

# Data for individual stocks and/or metiers can be direcyly extracted
#   using the same accesors as in FLCatch, by specifying the metier and/or catch
landings(bt4, metier='TBB', catch='ple')
catch.n(bt4, metier='TBB', catch='sol')

# and they can be modified too
landings(bt4, 'TBB', 'ple') <- landings(bt4, 'TBB', 'ple') + 0.0001

# Operations can be carried out for slots at the metier level, like extracting ...
effshare(bt4, 'TBB')
effshare(metier(bt4, 'TBB'))

# ... and modifying
effshare(metier(bt4, 'TBB')) <- FLQuant(0.8, dimnames=dimnames(effshare(metier(bt4, 'TBB'))))
effshare(metier(bt4, 'TBB')) <- 1

# Landings for all species in a given metier can be plotted as
xyplot(data~year|qname, landings(bt4, metier='TBB'), type='b', pch=19, xlab="",
  ylab="landings (t)")

# and for a single species accross all metiers with
xyplot(data~year|qname, landings(bt4, catch='ple'), type='b', pch=19, xlab="",
  ylab="landings (t)")

# A whole FLFleet can be modified by subsetting or extending across 'year',
newbt4 <- window(bt4, start=1990, end=2010)

# and trimming along more than one dimension
newbt4 <- trim(newbt4, age=1:5)

# Creating new FLFleet objects is better done in steps
# first an FLCatch, in this by conversion of an FLStock
ca <- as(ple4, 'FLCatch')

# or from a set of FLQuant objects

# then into a metier
me <- FLMetier(ca, name='all')

# and then into a fleet
fl <- FLFleet(me, name='allNS', desc='All NS catches')

# or in one go, although this gives less control over slots
# at the metier and catch level
fl <- FLFleet(ca, name='allNS', desc='All NS catches')


revenue(catches(metier(bt4,1),1))
revenue(catches(metier(bt4,1)))
