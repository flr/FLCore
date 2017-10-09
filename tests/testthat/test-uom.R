# test-uom.R - DESC
# /test-uom.R

# Copyright EURpean Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.EURpa.eu>
#
# Distributed under the terms of the EURpean Union Public Licence (EUPL) V.1.1.

# CONTEXT
context("uom for product and complex units")

# TEST product of denominator returns numerator
test_that("product of denominator returns numerator", {

  # EXPECT units(day/boat * boat) == day
  expect_equal(
    units(FLQuant(units="day/boat") * FLQuant(units="boat")), "day")
  # EXPECT units(EUR/day/boat * boat) == EUR/day
  expect_equal(
    units(FLQuant(units="EUR/day/boat") * FLQuant(units="boat")), "EUR/day")

})

# TEST unparseable units are returned pasted
test_that("unparseable units are returned pasted", {

  # EXPECT units(EUR/day * boat) == EUR/day * boat
  expect_equal(
    units(FLQuant(units="EUR/day") * FLQuant(units="boat")), "EUR/day * boat")

})

# TEST

# TEST leading and trailing spaces are ignored
test_that("leading and trailing spaces are ignored", {

  # EXPECT units(" kg " * "1000") == "t"
  expect_equal(
    units(FLQuant(units=" kg ") * FLQuant(units="1000")), "t")

})

# DOES NOT PARSE:
FLQuant(126, units="boat / d") * FLQuant(2350, units="EUR / boat / d")
FLQuant(2350, units="EUR / boat / d") * FLQuant(2350, units="boat / d")


