# test-simplify.R - DESC
# FLCore/tests/testthat/test-simplify.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

load("setup.rda")

x <- simplify[['30']]

# -- TEST noseason {{{

res <- noseason(x, spwn.season=1, rec.season=1)

# TEST: dim and dimnames are correct
test_that("noseason: collapse seasons (stock has 1 season, so no-op)", {
  expect_equal(dim(res), c(dim(x)[1:3],  1, dim(x)[5:6]))
  expect_equal(dimnames(res)$season, "all")
})

# TEST: m: sum
test_that("noseason: m: sum", {
  expect_equal(m(res), apply(m(x), c(1,2,3,5,6), sum))
})

# TEST: mat: spawn.season
test_that("noseason: mat: spawn.season", {
  expect_equal_values(mat(res), mat(x)[,,,1])
})

# TEST: harvest.spwn, m.spwn: spwn in spawn.season
test_that("noseason: harvest.spwn, m.spwn: spawn.season", {
  expect_equal_values(harvest.spwn(res), harvest.spwn(x)[,,,1])
  expect_equal_values(m.spwn(res), m.spwn(x)[,,,1])
})

# TEST: catch/discards/landings.n: sum
test_that("noseason: catch/discards/landings.n: sum", {
  expect_equal(catch.n(res), apply(catch.n(x), c(1,2,3,5,6), sum))
  expect_equal(discards.n(res), apply(discards.n(x), c(1,2,3,5,6), sum))
  expect_equal(landings.n(res), apply(landings.n(x), c(1,2,3,5,6), sum))
})

# TEST: stock.n: 1st season
test_that("noseason: stock.n: 1st season, but age=0 corrected for rec.season", {
  expect_equal_values(stock.n(res)[1,], stock.n(x)[1,,,1])
})

# TEST: catch/discards/landings/stock.wt: weighted mean
test_that("noseason: catch/discards/landings/stock.wt: weighted mean", {
  expect_equal_values(catch.wt(res), apply(catch.n(x) * catch.wt(x), c(1,2,3,5,6), sum) / apply(catch.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(discards.wt(res), apply(discards.n(x) * discards.wt(x), c(1,2,3,5,6), sum) / apply(discards.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(landings.wt(res), apply(landings.n(x) * landings.wt(x), c(1,2,3,5,6), sum) / apply(landings.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(stock.wt(res), apply(stock.n(x) * stock.wt(x), c(1,2,3,5,6), sum) / apply(stock.n(x), c(1,2,3,5,6), sum))
})

# }}}

# test noseason (age0) {{{

# SET age=0
dimnames(x) <- list(age=seq(0, 9))

res <- noseason(x, spwn.season=1, rec.season=1)

# TEST: dim and dimnames are correct
test_that("noseason (age0): collapse seasons (stock has 1 season, so no-op)", {
  expect_equal(dim(res), c(dim(x)[1:3],  1, dim(x)[5:6]))
  expect_equal(dimnames(res)$season, "all")
})

# TEST: m: sum
test_that("noseason (age0): m: sum", {
  expect_equal(m(res), apply(m(x), c(1,2,3,5,6), sum))
})

# TEST: mat: spawn.season
test_that("noseason (age0): mat: spawn.season", {
  expect_equal_values(mat(res), mat(x)[,,,1])
})

# TEST: harvest.spwn, m.spwn: spwn in spawn.season
test_that("noseason (age0): harvest.spwn, m.spwn: spawn.season", {
  expect_equal_values(harvest.spwn(res), harvest.spwn(x)[,,,1])
  expect_equal_values(m.spwn(res), m.spwn(x)[,,,1])
})

# TEST: catch/discards/landings.n: sum
test_that("noseason (age0): catch/discards/landings.n: sum", {
  expect_equal(catch.n(res), apply(catch.n(x), c(1,2,3,5,6), sum))
  expect_equal(discards.n(res), apply(discards.n(x), c(1,2,3,5,6), sum))
  expect_equal(landings.n(res), apply(landings.n(x), c(1,2,3,5,6), sum))
})

# TEST: stock.n: 1st season
test_that("noseason (age0): stock.n: 1st season, but age=0 corrected for rec.season", {
  expect_equal_values(stock.n(res)[1,], stock.n(x)[1,,,1])
})

# TEST: catch/discards/landings/stock.wt: weighted mean
test_that("noseason (age0): catch/discards/landings/stock.wt: weighted mean", {
  expect_equal_values(catch.wt(res), apply(catch.n(x) * catch.wt(x), c(1,2,3,5,6), sum) / apply(catch.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(discards.wt(res), apply(discards.n(x) * discards.wt(x), c(1,2,3,5,6), sum) / apply(discards.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(landings.wt(res), apply(landings.n(x) * landings.wt(x), c(1,2,3,5,6), sum) / apply(landings.n(x), c(1,2,3,5,6), sum))
  expect_equal_values(stock.wt(res), apply(stock.n(x) * stock.wt(x), c(1,2,3,5,6), sum) / apply(stock.n(x), c(1,2,3,5,6), sum))
})

# }}}

# test noseason (age0, rec.season > 1) {{{

# rec.season > 1 triggers the age-0 correction: stock.n from rec.season,
# m summed over seasons from rec.season onwards

for (rs in 2:4) {
  res <- noseason(x, spwn.season=1, rec.season=rs)

  test_that(paste0("noseason (age0, rec.season=", rs, "): stock.n: age 0 from rec.season"), {
    expect_equal_values(stock.n(res)["0",], stock.n(x)["0",,,rs])
  })

  test_that(paste0("noseason (age0, rec.season=", rs, "): m: age 0 summed from rec.season"), {
    expect_equal_values(m(res)["0",], Reduce('+', lapply(rs:4, function(s) m(x)["0",,,s])))
  })

  test_that(paste0("noseason (age0, rec.season=", rs, "): ages > 0 unchanged by correction"), {
    expect_equal_values(stock.n(res)[-1,], stock.n(x)[-1,,,1])
  })
}

# }}}

# test nounit: F/M rule {{{

x <- simplify[['30']]

# F/M present: mat, harvest.spwn, m.spwn taken from F
res <- nounit(x)

test_that("nounit (F/M): mat, harvest.spwn, m.spwn taken from F", {
  expect_equal_values(mat(res), mat(x)[,,"F"])
  expect_equal_values(harvest.spwn(res), harvest.spwn(x)[,,"F"])
  expect_equal_values(m.spwn(res), m.spwn(x)[,,"F"])
})

# no F/M: mat, harvest.spwn, m.spwn averaged across units
y <- x
dimnames(y) <- list(unit=c("A", "B"))
res <- nounit(y)

test_that("nounit (no F/M): mat, harvest.spwn, m.spwn averaged across units", {
  expect_equal_values(mat(res), (mat(y)[,,"A"] + mat(y)[,,"B"]) / 2)
  expect_equal_values(harvest.spwn(res), (harvest.spwn(y)[,,"A"] + harvest.spwn(y)[,,"B"]) / 2)
  expect_equal_values(m.spwn(res), (m.spwn(y)[,,"A"] + m.spwn(y)[,,"B"]) / 2)
})

# }}}

