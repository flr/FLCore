# test-combine.R - DESC
# /test-combine.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


context("combine")

test_that("combine(FLQuant, FLQuant) works", {

  x <- FLQuant(2, dimnames=list(age=1:4, year=2000:2004, iter=1:25))
  y <- FLQuant(5, dimnames=list(age=1:4, year=2000:2004, iter=26:50))

  # EXPECT an FLQuant
  expect_s4_class(combine(x, y), "FLQuant")

  # EXPECT length matches
  expect_length(combine(x, y), length(x) + length(y))

  # EXPECT dimnames
  expect_identical(dimnames(combine(x, y))[1:5], dimnames(x)[1:5])

  z <- FLQuant(3, dimnames=list(age=1:4, year=2000:2004, iter=c(57:58, 100)))

  # EXPECT iter dim to match
  expect_equivalent(dim(combine(x, y, z))[6], sum(dim(x)[6], dim(y)[6], dim(z)[6]))

})
