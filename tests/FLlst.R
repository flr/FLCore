#=====================================================================
#
# Date: 13/04/2007
# Version: 0.1-0
# Authors: Ernesto Jardim
#
# Short description: tests for FLlst
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
zz <- startTest("FLlstTest.txt")
tagTest("FLlst testing ...")

#! Constructors

# FLlst
checkRun(fll01 <- new("FLlst", list(a=1:10, b=10:20)))
checkRun(fll02 <- new("FLlst", list(1:10, 10:20), names=c("a","b")))
checkRun(fll03 <- FLlst(a=1:10, b=10:20))
checkRun(fll04 <- FLlst(list(a=1:10, b=10:20)))
checkRun(fll05 <- FLlst(c(1:10), c(10:20)))
checkRun(names(fll05) <- names(fll01))
checkIdentical(is(fll01),c("FLlst", "list", "vector"))
checkIdentical(names(fll01), c("a","b"))
checkIdentical(fll01, fll02)
checkIdentical(fll01, fll03)
checkIdentical(fll01, fll04)
checkIdentical(fll01, fll05)

# FLStocks
checkRun(s1 <- FLStock())
checkRun(s2 <- FLStock())
checkRun(fls01 <- new("FLStocks", list(a=s1, b=s2)))
checkRun(fls02 <- new("FLStocks", list(s1, s2), names=c("a","b")))
checkRun(fls03 <- FLStocks(a=s1, b=s2))
checkRun(fls04 <- FLStocks(list(a=s1, b=s2)))
checkRun(fls05 <- FLStocks(s1, s2))
checkRun(names(fls05) <- names(fls01))
checkIdentical(is(fls01), c("FLStocks", "FLlst", "list", "vector"))
checkTrue(is(fls01, "FLStocks"))
checkIdentical(names(fls01), c("a","b"))
checkIdentical(fls01, fls02)
checkIdentical(fls01, fls03)
checkIdentical(fls01, fls04)
checkIdentical(fls01, fls05)

# FLIndices
checkRun(s1 <- FLIndex())
checkRun(s2 <- FLIndex())
checkRun(fli01 <- new("FLIndices", list(a=s1, b=s2)))
checkRun(fli02 <- new("FLIndices", list(s1, s2), names=c("a","b")))
checkRun(fli03 <- FLIndices(a=s1, b=s2))
checkRun(fli04 <- FLIndices(list(a=s1, b=s2)))
checkRun(fli05 <- FLIndices(s1, s2))
checkRun(names(fli05) <- names(fli01))
checkIdentical(is(fli01), c("FLIndices", "FLlst", "list", "vector"))
checkTrue(is(fli01, "FLIndices"))
checkIdentical(names(fli01), c("a","b"))
checkIdentical(fli01, fli02)
checkIdentical(fli01, fli03)
checkIdentical(fli01, fli04)
checkIdentical(fli01, fli05)

# FLBiols
checkRun(s1 <- FLBiol())
checkRun(s2 <- FLBiol())
checkRun(flb01 <- new("FLBiols", list(a=s1, b=s2)))
checkRun(flb02 <- new("FLBiols", list(s1, s2), names=c("a","b")))
checkRun(flb03 <- FLBiols(a=s1, b=s2))
checkRun(flb04 <- FLBiols(list(a=s1, b=s2)))
checkRun(flb05 <- FLBiols(s1, s2))
checkRun(names(flb05) <- names(flb01))
checkIdentical(is(flb01), c("FLBiols", "FLlst", "list", "vector"))
checkTrue(is(flb01, "FLBiols"))
checkIdentical(names(flb01), c("a","b"))
checkIdentical(flb01, flb02)
checkIdentical(flb01, flb03)
checkIdentical(flb01, flb04)
checkIdentical(flb01, flb05)

# FLCatches
checkRun(s1 <- FLCatch(name='HKE'))
checkRun(s2 <- FLCatch(name='HKE'))
checkRun(flc01 <- new("FLCatches", list(a=s1, b=s2)))
checkRun(flc02 <- new("FLCatches", list(s1, s2), names=c("a","b")))
checkRun(flc03 <- FLCatches(a=s1, b=s2))
checkRun(flc04 <- FLCatches(list(a=s1, b=s2)))
checkRun(flc05 <- FLCatches(s1, s2))
checkRun(names(flc05) <- names(flc01))
checkIdentical(is(flc01), c("FLCatches", "FLlst", "list", "vector"))
checkTrue(is(flc01, 'FLCatches'))
checkIdentical(names(flc01), c("a","b"))
checkIdentical(flc01, flc02)
checkIdentical(flc01, flc03)
checkIdentical(flc01, flc04)
checkIdentical(flc01, flc05)

# FLFleets
checkRun(s1 <- FLFleet())
checkRun(s2 <- FLFleet())
checkRun(flf01 <- new("FLFleets", list(a=s1, b=s2)))
checkRun(flf02 <- new("FLFleets", list(s1, s2), names=c("a","b")))
checkRun(flf03 <- FLFleets(a=s1, b=s2))
checkRun(flf04 <- FLFleets(list(a=s1, b=s2)))
checkRun(flf05 <- FLFleets(s1, s2))
checkRun(names(flf05) <- names(flf01))
checkIdentical(is(flf01), c("FLFleets", "FLlst", "list", "vector"))
checkTrue(is(flf01, "FLFleets"))
checkIdentical(names(flf01), c("a","b"))
checkIdentical(flf01, flf02)
checkIdentical(flf01, flf03)
checkIdentical(flf01, flf04)
checkIdentical(flf01, flf05)

# FLQuants
checkRun(s1 <- FLQuant())
checkRun(s2 <- FLQuant())
checkRun(flq01 <- new("FLQuants", list(a=s1, b=s2)))
checkRun(flq02 <- new("FLQuants", list(s1, s2), names=c("a","b")))
checkRun(flq03 <- FLQuants(a=s1, b=s2))
checkRun(flq04 <- FLQuants(list(a=s1, b=s2)))
checkRun(flq05 <- FLQuants(s1, s2))
checkRun(names(flq05) <- names(flq01))
checkIdentical(is(flq01), c("FLQuants", "FLlst", "list", "vector"))
checkTrue(is(flq01, "FLQuants"))
checkIdentical(names(flq01), c("a","b"))
checkIdentical(flq01, flq02)
checkIdentical(flq01, flq03)
checkIdentical(flq01, flq04)
checkIdentical(flq01, flq05)

#! Replacement

# FLlst
checkRun(fll01[[1]] <- c(1:2))
checkRun(fll02$a <- c(1:2))
checkRun(fll03[1] <- list(c(1:2)))
checkFail(fll01[[1]] <- rnorm(10))
checkFail(fll01$a <- rnorm(10))
checkFail(fll01[1] <- list(rnorm(10)))
fll04 <- FLlst()
checkRun(fll04[[2]] <- c(10:20))
checkRun(fll04[[1]] <- c(1:2))
checkRun(names(fll04) <- c("a","b"))
fll05 <- FLlst()
checkRun(fll05[["a"]] <- c(1:2))
checkRun(fll05[["b"]] <- c(10:20))
fll06 <- FLlst()
checkRun(fll06$a <- c(1:2))
checkRun(fll06$b <- c(10:20))
checkIdentical(fll01, fll02)
checkIdentical(fll01, fll03)
checkIdentical(fll01, fll04)
checkIdentical(fll01, fll05)
checkIdentical(fll01, fll06)

# FLStocks
checkRun(fls01[[1]] <- ple4)
checkRun(fls02$a <- ple4)
checkRun(fls03[1] <- list(ple4))
checkFail(fls01[[1]] <- c(1:2))
checkFail(fls02$a <- c(1:2))
checkFail(fls03[1] <- list(c(1:2)))
fls04 <- FLStocks()
checkRun(fls04[[2]] <- FLStock())
checkRun(fls04[[1]] <- ple4)
checkRun(names(fls04) <- c("a","b"))
fls05 <- FLStocks()
checkRun(fls05[["a"]] <- ple4)
checkRun(fls05[["b"]] <- FLStock())
fls06 <- FLStocks()
checkRun(fls06$a <- ple4)
checkRun(fls06$b <- FLStock())
checkIdentical(fls01, fls02)
checkIdentical(fls01, fls03)
checkIdentical(fls01, fls04)
checkIdentical(fls01, fls05)
checkIdentical(fls01, fls06)

# FLIndices
checkRun(fli01[[1]] <- ple4.indices[[1]])
checkRun(fli02$a <- ple4.indices[[1]])
checkRun(fli03[1] <- list(ple4.indices[[1]]))
checkFail(fli01[[1]] <- c(1:2))
checkFail(fli02$a <- c(1:2))
checkFail(fli03[1] <- list(c(1:2)))
fli04 <- FLIndices()
checkRun(fli04[[2]] <- FLIndex())
checkRun(fli04[[1]] <- ple4.indices[[1]])
checkRun(names(fli04) <- c("a","b"))
fli05 <- FLIndices()
checkRun(fli05[["a"]] <- ple4.indices[[1]])
checkRun(fli05[["b"]] <- FLIndex())
fli06 <- FLIndices()
checkRun(fli06$a <- ple4.indices[[1]])
checkRun(fli06$b <- FLIndex())
checkIdentical(fli01, fli02)
checkIdentical(fli01, fli03)
checkIdentical(fli01, fli04)
checkIdentical(fli01, fli05)
checkIdentical(fli01, fli06)

# FLBiols
checkFail(flb01[[1]] <- c(1:2))
checkFail(flb02$a <- c(1:2))
checkFail(flb03[1] <- list(c(1:2)))
flb04 <- FLBiols()
checkRun(flb04[[2]] <- FLBiol())
checkRun(flb04[[1]] <- FLBiol())
checkRun(names(flb04) <- c("a","b"))
flb05 <- FLBiols()
checkRun(flb05[["a"]] <- FLBiol())
checkRun(flb05[["b"]] <- FLBiol())
flb06 <- FLBiols()
checkRun(flb06$a <- FLBiol())
checkRun(flb06$b <- FLBiol())
checkIdentical(flb01, flb02)
checkIdentical(flb01, flb03)
checkIdentical(flb01, flb04)
checkIdentical(flb01, flb05)
checkIdentical(flb01, flb06)

#! Selection

# FLlst
checkIdentical(is(fll01[1]), c("FLlst", "list", "vector"))

# FLStocks
checkIdentical(is(fls01[1]), c("FLStocks", "FLlst", "list", "vector"))

# FLIndices
checkIdentical(is(fli01[1]), c("FLIndices", "FLlst", "list", "vector"))

# FLBiols
checkIdentical(is(flb01[1]), c("FLBiols", "FLlst", "list", "vector"))

# FLCatches
checkIdentical(is(flc01[1]), c("FLCatches", "FLlst", "list", "vector"))

# FLCatches
checkIdentical(is(flf01[1]), c("FLFleets", "FLlst", "list", "vector"))

# FLQuants
checkIdentical(is(flq01[1]), c("FLQuants", "FLlst", "list", "vector"))

finishTest()
