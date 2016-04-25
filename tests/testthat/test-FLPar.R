# test-FLPar.R - DESC
# /test-FLPar.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC

# FLPar('missing')
context("FLPar('missing')")

# FLPar(a=1)
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})

# FLPar(a=1, b=rnorm(9))
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})


# FLPar(a=1, iter=20)
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})


# FLPar(a=1, b=rnorm(9), iter=9)
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})


# FLPar(a=1, b=rnorm(9), iter=12)
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})

# FLPar(params=c('a','b'))
test_that("FLPar(a=1) works as expected", {

  res <- FLPar(a=1)

  # check dim
  expect_that(dim(res), equals(c(1,1)))
  # check dimnames
  expect_that(dimnames(res), equals(list(params='a', iter='1')))

})


