# FLBiol.R - DESC
# FLBiol.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

data(ple4)

# CREATE objects

# COERCE objects

# TEST coerced FLBiol matches FLStock

test_that("FLBiol matches quantities in FLStock", {

  ple4b <- as(ple4, "FLBiol")
  
  # N
  expect_equal(stock.n(ple4), n(ple4b)) 

  # WT
  expect_equal(stock.wt(ple4), wt(ple4b)) 

  # M
  expect_equal(m(ple4), m(ple4b)) 

  # mat
  expect_equal(mat(ple4), mat(ple4b)) 

})


# TEST dataset, ple4 with spwn=0.5
m.spwn(ple4) <- 0.5
harvest.spwn(ple4) <- 0.5
ple4b <- as(ple4, "FLBiol")

# SSB
test_that("ssb(FLBiol) is correct", {

  # COMPARE ssb(FLStock) ~ ssb(FLBiol, catch.n)
  # expect_equal(ssb(ple4), ssb(ple4b, catch.n=catch.n(ple4)))

  # COMPARE ssb(FLStock) ~ ssb(FLBiol, f)
  expect_equal(ssb(ple4), ssb(ple4b, f=harvest(ple4)))

  # COMPARE ssb(FLStock) ~ ssb(FLBiol, hr)
  # expect_equal(ssb(ple4), ssb(ple4b, hr=catch.n(ple4)/n(ple4b)))
})

