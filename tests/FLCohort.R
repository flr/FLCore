#=====================================================================
#
# Date: 03/10/2007
# Version: 0.1-0
# Authors: Ernesto Jardim
#
# Short description: tests for FLCohort
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#
#=====================================================================

library(FLCore)
data(ple4)
data(ple4.indices)
data(ple4sex)

# start test
setCon()
zz <- startTest("FLCohortTest.txt")
tagTest("FLCohort testing ...")

checkRun(flc <- FLCohort(catch.n(ple4)))
checkTrue(is(flc, "FLCohort"))
checkRun(flq <- flc2flq(flc))
checkTrue(is(flq, "FLQuant"))
checkIdentical(flq, catch.n(ple4))

finishTest()

