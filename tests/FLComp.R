# FLComp - «Short one line description»
# FLComp

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# Last Change: 09 Jan 2009 16:18
# $Id$

# Reference:
# Notes:

# TODO Fri 09 Jan 2009 04:11:56 PM CET IM:

library(FLCore)

biol <- as(ple4, 'FLBiol')

# Bug 20080109_1612 trim(FLComp) had worng variables names
trim( biol, age=1:10)
trim( biol, age=1:10, year=1990:1999)
