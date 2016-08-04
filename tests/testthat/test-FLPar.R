# test-FLPar.R - DESC
# FLCore/tests/testthat/test-FLPar.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC

# testFLPar {{{
testFLPar <- function(par, msg, dim, dmns) {

  test_that(msg, {

    # check dim
    expect_equal(dim(par), dim)
    # check dimnames
    expect_equal(dimnames(par), dmns)
    # validObject
    expect_true(validObject(par))
    # is("FLPar")
    expect_s4_class(par, "FLPar")

  })
} # }}}

# FLPar('missing')
context("FLPar('missing')")

# FLPar(a=1)
testFLPar(FLPar(a=1), "FLPar(a=1) works as expected",
  c(1,1), list(params="a", iter="1"))

# FLPar(a=1, b=rnorm(9))
testFLPar(FLPar(a=1, b=rnorm(9)), "FLPar(a=1, b=rnorm(9)) works as expected",
  c(2,9), list(params=c("a", "b"), iter=ac(seq(9))))

# FLPar(a=1, iter=20)
testFLPar(FLPar(a=1, iter=20), "FLPar(a=1, iter=20) works as expected",
  c(1,20), list(params="a", iter=ac(seq(1, 20))))

# FLPar(a=1, b=rnorm(9), iter=9)
testFLPar(FLPar(a=1, b=rnorm(9), iter=9),
  "FLPar(a=1, b=rnorm(9), iter=9) works as expected",
  c(2,9), list(params=c("a","b"), iter=ac(seq(9))))

# FLPar(a=1, b=rnorm(9), iter=12)
testFLPar(FLPar(a=1, b=rnorm(9), iter=12),
  "FLPar(a=1, b=rnorm(9), iter=12) works as expected",
  c(2,12), list(params=c("a", "b"), iter=ac(seq(12))))

# FLPar(params=c("a","b"))
testFLPar(FLPar(params=c("a","b")), "FLPar(params=c('a','b')) works as expected",
 c(2,1), list(params=c("a", "b"), iter=ac(seq(1))))
