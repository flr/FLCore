#=====================================================================
#
# Date: 29/01/2008
# Version: 0.1-0
# Authors: Ernesto Jardim
#
# Short description: tests for FLQuants
#
# ToDo: A lot
#
# References (bibtex):
#
#!Notes:
#
#=====================================================================

library(FLCore)
data(ple4)
data(ple4sex)

# start test
setCon()
zz <- startTest("FLQuantsTest.txt")
tagTest("FLQuants testing ...")

# different unit levels
# note that the resultnig FLQuants is a mess !!
data(ple4)
data(ple4sex)
flq1 <- ple4sex@catch.n
flq2 <- ple4@catch.n
checkRun(flqs <- mcf(list(flq1, flq2)))
checkTrue(is.FLQuants(flqs))
checkIdentical(dimnames(flqs[[1]]), dimnames(flqs[[2]]))
# check it does leave levels out
flq1 <- window(ple4@catch.n, ple4@range["minyear"], 1990)
flq2 <- window(ple4@catch.n, 1991, ple4@range["maxyear"])
checkRun(flqs <- mcf(list(flq1, flq2)))
checkTrue(is.FLQuants(flqs))
checkIdentical(dimnames(flqs[[1]]), dimnames(flqs[[2]]))
checkIdentical(dimnames(flqs[[1]]), dimnames(ple4@catch.n))
# check numeric quant dim
flq1 <- trim(ple4@catch.n, age=c(ple4@range["min"]: 4))
flq2 <- trim(ple4@catch.n, age=c(5: ple4@range["max"]))
checkRun(flqs <- mcf(list(flq1, flq2)))
checkTrue(is.FLQuants(flqs))
checkIdentical(dimnames(flqs[[1]]), dimnames(flqs[[2]]))
checkIdentical(dimnames(flqs[[1]]), dimnames(ple4@catch.n))
# check character quant dim
flq1 <- ple4@catch
flq2 <- ple4@catch
dimnames(flq2)[[1]]<-"unique"
checkRun(flqs <- mcf(list(flq1, flq2)))
checkTrue(is.FLQuants(flqs))
checkIdentical(dimnames(flqs[[1]]), dimnames(flqs[[2]]))
flq01 <- trim(flqs[[1]], age="all")
units(flq01)<-units(flq1)
flq02 <- trim(flqs[[1]], age="unique")
units(flq02)<-units(flq2)
checkIdentical(flq1, flq01)
checkIdentical(flq2, flq02)
# check numeric and character quant dim
flq1 <- ple4@catch
flq2 <- ple4@catch.n
checkRun(flqs <- mcf(list(flq1, flq2)))
checkTrue(is.FLQuants(flqs))
checkIdentical(dimnames(flqs[[1]]), dimnames(flqs[[2]]))

finishTest()


