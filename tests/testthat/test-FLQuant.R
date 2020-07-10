# test-FLQuant.R - DESC
# /test-FLQuant.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(hedgehog)

# VECTOR

test_that("FLQuant() handles vectors of all lengths",
  forall(gen.c(gen.element(seq(1, 100))), function(x)
    expect_equal(length(x), dim(FLQuant(x))[2]))
)

# MATRIX

gen_matrix   <- generate(for (i in gen.int(200)) matrix(i, ncol=i, nrow=i))

test_that( "FLQuant() uses matrices with the right dimensions",
  forall( gen_matrix, function(x) expect_equal(dim(x), dim(FLQuant(x))[1:2]))
)

test_that( "FLQuant() uses matrices with the right numbers",
  forall( gen_matrix, function(x) expect_equal(sum(x), sum(FLQuant(x))))
)

test_that( "FLQuant() uses matrices with the right numbers",
  forall( gen_matrix, function(x) expect_s4_class(FLQuant(x), "FLQuant"))
)

# ARRAY

gen_array   <- generate(for (i in gen.int(200)) array(i, dim=c()))

test_that( "FLQuant() uses matrices with the right dimensions",
  forall( gen_matrix, function(x) expect_equal(dim(x), dim(FLQuant(x))[1:2]))
)

test_that( "FLQuant() uses matrices with the right numbers",
  forall( gen_matrix, function(x) expect_equal(sum(x), sum(FLQuant(x))))
)

test_that( "FLQuant() uses matrices with the right numbers",
  forall( gen_matrix, function(x) expect_s4_class(FLQuant(x), "FLQuant"))
)
