# test-uom.R - DESC
# /test-uom.R

# Copyright EURpean Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.EURpa.eu>
#
# Distributed under the terms of the EURpean Union Public Licence (EUPL) V.1.1.

# --- CONTEXT: product for complex units
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

# TEST leading and trailing spaces are ignored
test_that("leading and trailing spaces are ignored", {

  # EXPECT units(" kg " * "1000") == "t"
  expect_equal(
    units(FLQuant(units=" kg ") * FLQuant(units="1000")), "t")

})


# --- CONTEXT numeric values with different styles
context("uom for numeric values different styles")

# TEST 
test_that("", {

  a1 <- FLQuant(1, units="1000")
  a2 <- FLQuant(1, units="1e3")
  a3 <- FLQuant(1, units="1e+03")
  b <- FLQuant(1, units="10")
  
  # EXPECT 100 / 10 = 100
  expect_equal(units(a1 / b), "100")
  expect_equal(units(a2 / b), "100")
  expect_equal(units(a3 / b), "100")

})


# ---

# DOES NOT PARSE:
FLQuant(126, units="kg / d") * FLQuant(2350, units="EUR / kg / d")

FLQuant(126, units="boat / d") * FLQuant(2350, units="EUR / boat / d")
FLQuant(2350, units="EUR / boat / d") * FLQuant(2350, units="boat / d")
FLQuant(2350, units="eur / boat / d") * FLQuant(2350, units="boat / d")

FLQuant(2350, units="€ / boat / d") * FLQuant(2350, units="boat / d")
FLQuant(2350, units="$ / boat / d") * FLQuant(2350, units="boat / d")

FLQuant(2, units="1000") * (FLQuant(3, units="kg") * FLQuant(0.004, units="EUR / kg"))
FLQuant(2, units="1000") * FLQuant(3, units="kg") * FLQuant(4, units="EUR / t")

FLQuant(2, units="EUR") + FLQuant(3, units="EUR / boat") * FLQuant(4, units="boat")
FLQuant(2, units="EUR") + FLQuant(3, units="EUR/boat") * FLQuant(4, units="boat")

FLQuant(2, units="EUR / crew") * FLQuant(3, units="crew") + FLQuant(0.5, units="") * FLQuant(1200, units="EUR")
