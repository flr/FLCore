# setup.R - Build testing objects
# FLCore/tests/testthat/setup.R

# Copyright (c) WMR, 2026.
# Author: Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# -- simplify: FLStocks with units, seasons, areas {{{

data(ple4)

make_stk <- function(niter = 1) {
  stk <- expand(ple4, unit = c("F", "M"), season = 1:4, area = 1:2)
  if (niter > 1) stk <- propagate(stk, niter)

  dn  <- dimnames(catch.n(stk))
  idx <- arrayInd(seq_len(prod(lengths(dn))), lengths(dn))

  mk <- function(fu, fs, fa, fi = rep(1, niter)) {
    FLQuant(array(fu[idx[, 3]] * fs[idx[, 4]] * fa[idx[, 5]] * fi[idx[, 6]],
                  dim = lengths(dn), dimnames = dn))
  }
  fi <- seq(1, 1.2, length.out = niter)

  w <- mk(c(0.4, 0.6), c(0.1, 0.2, 0.3, 0.4), c(0.7, 0.3), fi)
  for (s in c("landings.n", "discards.n", "stock.n"))
    slot(stk, s) <- slot(stk, s) * w

  r <- mk(c(0.9, 1.1), c(0.8, 0.95, 1.05, 1.2), c(0.85, 1.15), fi)
  for (s in c("m", "stock.wt", "landings.wt", "discards.wt"))
    slot(stk, s) <- slot(stk, s) * r
  mat(stk) <- mat(stk) * r
  mat(stk)[mat(stk) > 1] <- 1

  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch(stk)    <- computeCatch(stk, slot = "all")
  stock(stk)    <- computeStock(stk)

  units(stk) <- standardUnits(stk)
  stk
}

simplify <- FLStocks(`1` = make_stk(1), `30` = make_stk(30))

save(simplify, file = "setup.rda")

# }}}

